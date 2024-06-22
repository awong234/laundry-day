library(rvest)
library(terra)
library(dplyr)
library(sf)

# Your working directory should be in the folder described below.
# ..../laundry-day/datasets/202406-nexrad
# Ideally, start your R session by double-clicking the .Rproj file in the root
# of the laundry-day project.
setwd(here::here('datasets', '202406-nexrad'))

# Tinkering ---------------------------

config = list(year = '2024', month = '04', day = '02')
dirname = paste0(config, collapse = '')
baseurl = 'https://mesonet.agron.iastate.edu/archive/data/{year}/{month}/{day}/GIS/uscomp'
url = glue::glue(baseurl, .envir = config)
# Need to get the list of images that iastate is providing from this website.
page = rvest::read_html(url)
# We get all the links on the page, and then the href. This is just a bare
# template HTML listing the contents of the directory so this should be fairly
# robust, but there's surely a better way...
files = rvest::html_elements(page, xpath = r'(//a)') |>
    rvest::html_attr('href')

files = files[grepl(pattern = 'n0q', x = files) & !grepl(pattern = 'max', x = files)]
# Avoid downloading files twice
files_keep = setdiff(files, list.files(file.path('nexrad_images', dirname)))

dir.create(file.path('nexrad_images', dirname), showWarnings = FALSE, recursive = TRUE)

files_download = file.path(url, files_keep)
files_dest = file.path('nexrad_images', dirname, files_keep)

# # # # # # # # # # # # # # #
# Download files
if (length(files_download)) {
    curl::multi_download(files_download, files_dest, progress = TRUE)
}

# # # # # # # # # # # # # # #

# Let's play around with a test file
test_file = files[1]

img = terra::rast(file.path('nexrad_images', dirname, test_file))
crs(img) = crs("EPSG:4326")     # Set the CRS explicitly to WGS84. It's already using that but do it to be safe.
utm_zone_17 = crs('EPSG:32617') # We'll use another projection -- UTM Zone 17 -- to see how it differs
img[img == 0] = NA # Remove 0 levels; these are either missing or no cloud cover so we discard them for the purpose of visualization
img2 = terra::project(img, utm_zone_17) # Reproject the original image to utm Zone 17.

# This next rounding step is necessary for the mapping later. The original
# dataset has integer precision, so we want to keep it that way. Reprojection
# has the side-effect of aggregating some pixel values so we might lose a bit of
# accuracy but we round back to integer precision for the purpose of the color
# mapping.
values(img2) = round(values(img2))
# Reclassify values. Raw values range from 0-255. Output values range -35 to 75
# dbz, but < 0dbz is mapped to 0.
raw_range = seq(0, 255)
output_range = seq(-30, 75, length.out = length(raw_range))
mapping = cbind(
    raw_range,
    output_range
)
img = classify(img, mapping)
img2 = classify(img2, mapping)
colors = c(
    '#646464',
    '#04e9e7',
    '#019ff4',
    '#0300f4',
    '#02fd02',
    '#01c501',
    '#008e00',
    '#fdf802',
    '#e5bc00',
    '#fd9500',
    '#fd0000',
    '#d40000',
    '#bc0000',
    '#f800fd',
    '#9854c6',
    '#fdfdfd'

)

# Get US State boundaries
tmp = tempfile()
state_shapefile = 'cb_2018_us_state_20m'
download.file(sprintf('https://www2.census.gov/geo/tiger/GENZ2018/shp/%s.zip', state_shapefile), tmp)
unzip(tmp, exdir = state_shapefile)

states = sf::read_sf(state_shapefile)
conus = states %>% filter(! STUSPS %in% c("AK", "HI", "GU", "MP", "AS", "PR", "VI"))
conus = sf::st_transform(conus, st_crs("EPSG:4326"))
conus2 = sf::st_transform(conus, utm_zone_17)

# Plotting the images using the CRS set to EPSG:4326
# They are still _technically_ projected, unless you have a 3D visualizer you
# always need to project to map on a 2D surface.
# If the `sf` package is any reference, then the default projection used is an
# equirectangular projection
# https://en.wikipedia.org/wiki/Equirectangular_projection
plot(img, type = 'interval', breaks = c(-30, 0, seq(5, 75, by = 5)), col = colors)
plot(st_geometry(conus), graticule = TRUE, axes = TRUE, add = TRUE)

# Notice that the axis units are different, and the US is warped.
# UTM 17 is good for mapping the Northeast US, but direction is not preserved
# away from longitudes near the NE US.
plot(img2, type = 'interval', breaks = c(-30, 0, seq(5, 75, by = 5)), col = colors)
plot(st_geometry(conus2), graticule = TRUE, axes = TRUE, add = TRUE)

# Full workflow ------------------------------------------------------------

# Make an animation.
# Now that we've tinkered with the workflow we can assemble it into functions
# for ease-of-use.

download_nexrad_day = function(year, month, day) {
    baseurl = 'https://mesonet.agron.iastate.edu/archive/data/{year}/{month}/{day}/GIS/uscomp'
    url = glue::glue(baseurl)
    page = rvest::read_html(url)
    files = rvest::html_elements(page, xpath = r'(//a)') |> rvest::html_attr('href')
    files = files[grepl(pattern = 'n0q', x = files) & !grepl(pattern = 'max', x = files)]
    # Avoid downloading files twice
    files_keep = setdiff(files, list.files(file.path('nexrad_images', dirname)))

    dir.create(file.path('nexrad_images', dirname), showWarnings = FALSE, recursive = TRUE)

    files_download = file.path(url, files_keep)
    files_dest = file.path('nexrad_images', dirname, files_keep)

    # # # # # # # # # # # # # # #
    # Download files -------------
    if (length(files_download)) {
        curl::multi_download(files_download, files_dest, progress = TRUE)
    }
}


load_image = function(path) {
    img = terra::rast(path)
    crs(img) = crs("EPSG:4326")
    img[img == 0] = NA # Remove 0 levels
    raw_range = seq(0, 255)
    output_range = seq(-30, 75, length.out = length(raw_range))
    mapping = cbind(
        raw_range,
        output_range
    )
    img = classify(img, mapping)
    img
}

load_us_shapefile = function() {
    tmp = tempfile()
    state_shapefile = 'cb_2018_us_state_20m'
    download.file(sprintf('https://www2.census.gov/geo/tiger/GENZ2018/shp/%s.zip', state_shapefile), tmp)
    unzip(tmp, exdir = state_shapefile)

    states = sf::read_sf(state_shapefile)
    conus = states %>% filter(! STUSPS %in% c("AK", "HI", "GU", "MP", "AS", "PR", "VI"))
    conus = sf::st_transform(conus, st_crs("EPSG:4326"))
    conus
}

create_plots = function(dirname, time_resolution = 60, width = 720, height = 480) {
    stopifnot((time_resolution %% 5) == 0)
    image_files = list.files(path = file.path('nexrad_images', dirname), pattern = '.png')
    image_files = image_files[seq(1, length(image_files), by = time_resolution / 5)]
    for (i in seq_along(image_files)) {
        logger::log_info("image {i}")
        image_file = image_files[i]
        image_index = substr(image_file, 5, 16)
        image_time = substr(image_file, 13, 16)
        image_time = strsplit(image_time, '')[[1]] |> append(":", after = 2) |> paste0(collapse = '')
        img = load_image(file.path('nexrad_images', dirname, image_file))
        dir.create(dirname, showWarnings = FALSE)
        png(filename = glue::glue('{dirname}/{dirname}_{image_index}.png'), width = width, height = height)
        plot(img, type = 'interval', breaks = c(-30, 0, seq(5, 75, by = 5)), col = colors, main = paste(dirname, image_time))
        plot(st_geometry(conus), add = TRUE)
        dev.off()
    }
}

create_animation = function(dirname, fps = 10) {
    images = list.files(dirname, pattern = 'png')
    images = magick::image_read(file.path(dirname, images))
    gif = magick::image_animate(images, fps = fps)
    magick::image_write(gif, path = file.path(dirname, 'animation.gif'))
}

colors = c(
    '#646464',
    '#04e9e7',
    '#019ff4',
    '#0300f4',
    '#02fd02',
    '#01c501',
    '#008e00',
    '#fdf802',
    '#e5bc00',
    '#fd9500',
    '#fd0000',
    '#d40000',
    '#bc0000',
    '#f800fd',
    '#9854c6',
    '#fdfdfd'
)


# April 2 2024

conus = load_us_shapefile()
config = list(year = '2024', month = '04', day = '02')
dirname = paste0(config, collapse = '')
download_nexrad_day(config$year, config$month, config$day)
create_plots(dirname)
create_animation(dirname)
