library(pdftools)

txt = pdf_text(here::here("datasets/cuyahoga-county-voter-registration-locations/voter-registration-locations.pdf"))

# Look at the first page

first = txt[1]

first |> cat()
first_split = first |> strsplit('\n')
first_split[[1]] = trimws(first_split[[1]])
first_split[[1]] = first_split[[1]][-1]
headers = grepl(pattern = "^\\w+", x = first_split[[1]])
first_split[[1]][headers]
values = grepl(pattern = "•", x = first_split[[1]])
first_split[[1]][values]

out_list = list()
headers = grepl(pattern = "^\\w+", x = first_split[[1]])
values = grepl(pattern = "•", x = first_split[[1]])
for (i in 1:length(first_split[[1]])) {
    line_value = first_split[[1]][i]
    if (line_value == '') {
        next
    } else if (headers[i]) {
        header_name = line_value
    } else if (values[i]) {
        out_list[[header_name]] = c(out_list[[header_name]], line_value)
    } else {
        next
    }
}

out_list = lapply(X = out_list, FUN = \(x) gsub(pattern = "^\\s*•\\s+", replacement = "", x))
out_df = reshape2::melt(out_list, value.name = 'address') |> setNames(nm = c("address", "city"))
out_df |> tidyr::separate(address, into = c("location", "address"), sep = "-(?=\\s\\d)")
out_df |> tidyr::separate(address, into = c("location", "address"), sep = "\\s-(?=\\s\\d)\\s")
out_df |> tidyr::separate(address, into = c("location", "address"), sep = "\\s[-–](?=\\s\\d)\\s")
out_df = out_df |> tidyr::separate(address, into = c("location", "address"), sep = "\\s[-–](?=\\s\\d)\\s")
out_df

# Generalize to all pages

parse_pages = function(text) {
    split = strsplit(txt, split = '\n')
    single_document = do.call(what = c, args = split)
    parse_page = function(page) {
        # Header is a line that starts immediately with letters
        headers = grepl(pattern = "^\\w+", x = page)
        # Values that have a dot
        values = grepl(pattern = "•", x = page)
        # Sometimes the line continues over to the next; this starts with spaces and letters/numbers
        continuation = grepl(pattern = "^\\s+[A-Za-z0-9]", x = page)
        out_list = list()
        for (i in 1:length(page)) {
            line_value = page[i]
            if (line_value == '') {
                next
            } else if (headers[i]) {
                header_name = line_value
            } else if (values[i]) {
                next_continuation = continuation[i+1]
                if (next_continuation & !is.na(next_continuation)) {
                    line_value = paste0(line_value, trimws(page[i+1]))
                }
                out_list[[header_name]] = c(out_list[[header_name]], line_value)
            } else {
                next
            }
        }
        out_list = lapply(out_list, \(x) gsub(pattern = "^\\s*•\\s*", replacement = "", x))
        out_df = reshape2::melt(out_list) |> setNames(nm = c("address", "city"))
        out_df = out_df |> tidyr::separate(address, into = c("location", "address"), sep = "\\s[-–](?=\\s\\d)\\s")
        out_df
    }
    pages_parsed = parse_page(single_document)
    pages_parsed
}

pages = parse_pages(txt)

# Map it!

library(ggmap)

ggmap::register_google(key = Sys.getenv("GOOGLE_API"))

geocode_m = memoise::memoise(ggmap::geocode, cache = cachem::cache_disk('./datasets/cuyahoga-county-voter-registration-locations/cache'))

lonlat = purrr::map(pages$address, .f = function(x) {
    if (!is.na(x)) res = geocode_m(x) else res = tibble::tibble(lon = NA, lat = NA)
    res
})

pages$lonlat = lonlat

lonlat_df = lonlat |> do.call(what = rbind, args = _)
lonlat_df$location = pages$location

get_map_m = memoise::memoise(ggmap::get_map, cache = cachem::cache_disk('./datasets/cuyahoga-county-voter-registration-locations/cache/map'))

ggmap(get_map_m(location = 'Cleveland')) +
    geom_point(data = lonlat_df, aes(x = lon, y = lat))
