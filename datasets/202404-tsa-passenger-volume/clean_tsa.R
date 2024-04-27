library(rvest)
library(dplyr)
library(lubridate)

baseurl = "https://www.tsa.gov/travel/passenger-volumes"
years = seq(2019, 2023)
joined_urls = paste(baseurl, years, sep = '/')

# Pull in the HTML
html_docs = lapply(joined_urls, \(x) {
    rvest::read_html(x)
})

# Pull out the table from the HTML (only one table per page), and `pluck` to
# turn it into a list of tables, not a list of lists (html_table returns a list)
# Then fix the data fields to proper data types from character.
data_tables = lapply(html_docs, \(x) {
    x |> rvest::html_table() |> purrr::pluck(1) |>
        mutate(
            Date = as.Date(Date, format = '%m/%d/%Y'),
            Numbers = gsub(pattern = "[^0-9\\.]", replacement = "", x = Numbers) |> as.numeric()
        )

})

# This year's data -- this is slightly different so we'll treat it slightly differently.
this_years_data = rvest::read_html(baseurl) |> rvest::html_table() |> purrr::pluck(1)
# Pull out the first two columns -- this _ought_ to be just the Date field and the Current Year field.
this_years_data = this_years_data[, c(1,2)]
# Current Year field needs to be named "Numbers" to coincide with the archive data fields.
colnames(this_years_data)[2] = 'Numbers'
# Convert the data fields to proper data types
this_years_data = this_years_data |>
    mutate(
        Date = as.Date(Date, format = '%m/%d/%Y'),
        Numbers = gsub(pattern = "[^0-9\\.]", replacement = "", x = Numbers) |> as.numeric()
    )

# Put it all together -- bind together the list of data frames that is
# data_tables, and then bind to that this year's data.
df = bind_rows(data_tables)
df = bind_rows(df, this_years_data)

# As a single function
clean_tsa_data = function(years) {
    baseurl = "https://www.tsa.gov/travel/passenger-volumes"
    joined_urls = paste(baseurl, years, sep = '/')

    # Pull in the HTML
    html_docs = lapply(joined_urls, \(x) {
        rvest::read_html(x)
    })

    fix_data_types = function(df) {
        df |> mutate(
            Date = as.Date(Date, format = '%m/%d/%Y'),
            Numbers = gsub(pattern = "[^0-9\\.]", replacement = "", x = Numbers) |> as.numeric()
        )
    }

    # Pull out the table from the HTML (only one table per page), and `pluck` to
    # turn it into a list of tables, not a list of lists (html_table returns a list)
    # Then fix the data fields to proper data types from character.
    data_tables = lapply(html_docs, \(x) {
        x |> rvest::html_table() |> purrr::pluck(1) |> fix_data_types()
    })

    # This year's data -- this is slightly different so we'll treat it slightly differently.
    this_years_data = rvest::read_html(baseurl) |> rvest::html_table() |> purrr::pluck(1)
    # Pull out the first two columns -- this _ought_ to be just the Date field and the Current Year field.
    this_years_data = this_years_data[, c(1,2)]
    # Current Year field needs to be named "Numbers" to coincide with the archive data fields.
    colnames(this_years_data)[2] = 'Numbers'
    # Convert the data fields to proper data types
    this_years_data = this_years_data |> fix_data_types()

    # Put it all together -- bind together the list of data frames that is
    # data_tables, and then bind to that this year's data.
    df = bind_rows(data_tables)
    df = bind_rows(df, this_years_data)
    df
}

df = clean_tsa_data(years = years)

# Visualize

plot(df$Date, df$Numbers, type = 'l')

# Model

holidays = tis::holidays(years)
holiday_names = names(holidays)
holidays = as.Date(as.character(holidays), format = '%Y%m%d')
names(holidays) = holiday_names

df = df |>
    mutate(
        Year = as.factor(lubridate::year(Date)),
        Wday = factor(lubridate::wday(Date, label = TRUE), levels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"), ordered = FALSE),
        DayOfMonth = as.factor(lubridate::day(Date)),
        DayOfYear = lubridate::yday(Date),
        WeekOfYear = as.factor(lubridate::week(Date)),
        IsHoliday = Date %in% holidays,
        WhichHoliday = names(holidays)[match(Date, holidays)],
        WhichHoliday = relevel(as.factor(case_when(is.na(WhichHoliday) ~ "ordinary", .default = WhichHoliday)), 'ordinary')
    )

df$ClosestHoliday = as.Date(sapply(df$Date, \(x) holidays[which.min(abs(x - holidays))]), origin = '1970-01-01')

days_around_holiday = 5
df = df |> mutate(SurroundsHoliday = abs(Date - ClosestHoliday) < days_around_holiday & abs(Date - ClosestHoliday) != 0)

df = df |> arrange(Date)

# Model could be improved by adding in a COVID covariate explicitly instead of just modeling overall trend in Date.
mod = mgcv::gam(Numbers ~ s(as.numeric(Date)) + s(DayOfYear) + Wday + WhichHoliday + SurroundsHoliday, data = df)
mod_scaled = mgcv::gam(scale(Numbers) ~ s(as.numeric(Date)) + s(DayOfYear) + Wday + WhichHoliday + SurroundsHoliday, data = df)
summary(mod_scaled)
f = fitted(mod)
plot(df$Date, df$Numbers, type = 'l')
lines(df$Date, f, col = 'red')

# Tuesdays are the least busy day to fly, followed by Saturday, and Wednesday.
# Fridays are the busiest day to fly, followed by Sunday, and Thursday
# Weekday (+/-5 days) surrounding holiday increases passenger volume by 0.13 standard deviations
# Least-traveled holidays are Thanksgiving, Independence Day, Christmas, in that order.
