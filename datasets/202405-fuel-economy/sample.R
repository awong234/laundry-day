# Start R here using .Rproj file

# Strategy for reproducibility ---------------

library_path = file.path('lib', version$platform, version$major, version$minor)
dir.create(library_path, showWarnings = FALSE, recursive = TRUE)
.libPaths(library_path)
options(repos = c("CRAN" = 'https://packagemanager.posit.co/cran/2024-05-01'))
if (! 'pak' %in% .packages(all.available = TRUE)) install.packages('pak', type = 'binary')

pak::pkg_install(c('DBI', 'duckdb', 'dplyr', 'data.table', 'ggplot2', 'ggrepel', 'patchwork', 'dbplyr'))

library(DBI)
library(duckdb)
library(dplyr)
library(data.table)
library(ggplot2)
library(ggrepel)
library(patchwork)

# Save datasets
veh_url = 'https://fueleconomy.gov/feg/epadata/vehicles.csv'
emi_url = 'https://fueleconomy.gov/feg/epadata/emissions.csv'

if(! file.exists('vehicles.csv')) download.file(veh_url, 'vehicles.csv')
if(! file.exists('emissions.csv')) download.file(emi_url, 'emissions.csv')

# duckdb ------------------------------------

# Delete the database -- (we don't have to do this, but for illustration I'm starting from nothing)
if (file.exists('data.db')) unlink('data.db')
# Make a connection to the duckdb database. It's just a file, so if it's not there it creates it.
con = dbConnect(duckdb(), 'data.db')

# Read the emissions data into duckdb -- ~50K records read in
dbExecute(con, glue::glue("create table emissions as select * from 'emissions.csv'"))
# (Attempt to) read the vehicles data into duckdb -- problem is a literal string 'NA' value in the cylinders column.
# This results in an error. Not run.
# dbExecute(con, glue::glue("create table vehicles as select * from 'vehicles.csv'"))

# Remedy this temporarily by having duckdb scan the entire table to figure out
# data types. This will set the cylinders and displ columns to be varchar, but
# that's easily fixed on the R side.
# If you were working with high-precision numeric fields, you would want to read
# those in as numeric values so as to prevent loss of precision when reading as
# varchar.
q = glue::glue("
create table vehicles as
select * from read_csv(
    'vehicles.csv',
    sample_size=-1
)
")
# ~47K records
dbExecute(con, q)

# We should see 'vehicles' and 'emissions' as tables in our database
dbListTables(con)

epa_scores = dbGetQuery(con,
"
select
    e.score
    ,v.make
    ,v.model
    ,v.year
    ,v.co2TailpipeGpm
from emissions as e
left join vehicles as v on
    v.id = e.id
")

str(epa_scores)

table(is.na(epa_scores$score))

# Correlate tail pipe emissions with EPA score
ggplot(epa_scores %>% filter(score >= 0)) +
    geom_point(aes(x = score, y = co2TailpipeGpm), alpha = 0.01) +
    geom_smooth(aes(x = score, y = co2TailpipeGpm), method = 'lm')

# Count transmission types by year
trans_types = dbGetQuery(con,
"
with trans_types as (
    select
    year,
    case
        when trany like 'Automatic%' then 'Automatic'
        when trany like 'Manual%' then 'Manual'
    end as trans_type
    from vehicles
)

select count(*) as n_vehicles, trans_type, year
from trans_types
group by trans_type, year
order by year, trans_type
")

# Manual availability decreasing
ggplot(trans_types) +
    geom_col(aes(x = year, y = n_vehicles, fill = trans_type))

# DBPLYR -------------------

# You can have duckdb do the heavy lifting, but code using dplyr!

# Connect to a table using this `dplyr::tbl` function:

veh_tbl = tbl(con, 'vehicles')

# Now we can code in dplyr, but get results from SQL

# Best cars in segments
# Highest highway MPG overall
veh_tbl %>%
    filter(highway08 == max(highway08)) %>%
    distinct(make, model, year, highway08, displ, cylinders, trany)

# Highest highway MPG for a manual transmission
veh_tbl %>%
    filter(trany %like% 'Manual%') %>%
    filter(highway08 == max(highway08)) %>%
    distinct(make, model, year, highway08, displ, cylinders, trany)

# Highest highway MPG for a hybrid vehicle
veh_tbl %>%
    filter(atvType %like% 'Hybrid%') %>%
    filter(highway08 == max(highway08)) %>%
    distinct(make, model, year, highway08, displ, cylinders, trany)

# MPG > 50 for a gasoline vehicle
veh_tbl %>%
    filter(is.na(atvType)) %>%
    filter(highway08 > 50) %>%
    distinct(make, model, year, highway08, displ, cylinders, trany) %>%
    print(n = 100)

# `show_query()` makes it obvious that this is just a SQL query run against duckdb.
veh_tbl %>%
    filter(is.na(atvType)) %>%
    filter(highway08 > 50) %>%
    distinct(make, model, year, highway08, displ, cylinders) %>%
    show_query()


# Other analyses -----------------------

# Warning is because there are literal "NA" character values in displ and cyl
vehicles = dbReadTable(con, 'vehicles') %>%
    mutate(
        displ = as.numeric(displ),
        cylinders = as.numeric(cylinders),
        luggage_volume = pmax(hlv, lv2, lv4),
        passenger_volume = pmax(hpv, pv2, pv4),
        total_volume = luggage_volume + passenger_volume,
        VClass2 = case_when(
            VClass %like% "Station Wagon" ~ "Station Wagon",
            VClass %like% "Minivan" ~ "Minivan",
            VClass %like% "Small Pickup Trucks" ~ "Small Pickup Trucks",
            VClass %like% "Small Sport Utility Vehicle" ~ "Small Sport Utility Vehicle",
            VClass %like% "Sport Utility Vehicle" ~ "Sport Utility Vehicle",
            VClass %like% "Standard Pickup Trucks" ~ "Standard Pickup Trucks",
            VClass %like% "Special Purpose Vehicle" ~ "Special Purpose Vehicle",
            VClass %like% "Vans" ~ "Vans",
            .default = VClass
        ),
        power_type = case_when(
            atvType |> is.na() ~ 'Gasoline',
            atvType == 'Diesel' ~ 'Diesel',
            atvType == 'Hybrid' ~ 'Hybrid',
            atvType == 'Plug-in Hybrid' ~ 'PHEV',
            atvType == 'EV' ~ 'EV',
            .default = 'OTHER'
        ),
        trans_type = case_when(
            grepl('Manual', trany) ~ 'Manual',
            grepl('Automatic', trany) ~ 'Automatic'
        ),
        decade = cut(year, breaks = c(1980, 1990, 2000, 2010, 2020, 2030), labels = c(1980, 1990, 2000, 2010, 2020))
    ) %>%
    filter(!VClass2 == 'Special Purpose Vehicle')

# Vehicles MPG by decade
vehicles %>%
    group_by(decade, VClass2) %>%
    summarize(
        comb08 = mean(comb08),
        city08 = mean(city08),
        highway08 = mean(highway08)
    ) %>%
    tidyr::pivot_wider(names_from = decade, values_from = c(comb08, city08, highway08))

# Vehicles MPG by power type
vehicles %>%
    group_by(decade, power_type) %>%
    summarize(
        comb08 = mean(comb08),
        city08 = mean(city08),
        highway08 = mean(highway08)
    ) %>%
    tidyr::pivot_wider(names_from = decade, values_from = c(comb08, city08, highway08))


# Overall trends by power type and vehicle class
ggplot(vehicles) +
    geom_point(aes(x = year, y = comb08), alpha = 0.01) +
    geom_smooth(aes(x = year, y = comb08)) +
    facet_grid(power_type~VClass2)

# Focusing on gasoline vehicles
gas_vehicles = vehicles %>% filter(power_type == 'Gasoline')
# Some improvement since 2010, but overall quite flat. Hard to improve efficiency of ICE!
ggplot(gas_vehicles) +
    geom_point(aes(x = year, y = comb08), alpha = 0.01) +
    geom_smooth(aes(x = year, y = comb08)) +
    facet_wrap(~VClass2)

# Improvement by year is ~0.01 MPG per year
mod = glm(data = gas_vehicles, formula = comb08 ~ year - 1)
summary(mod)
coef(mod)

# By VClass
mod = glm(data = gas_vehicles, formula = comb08 ~ year * VClass2 - 1)
summary(mod)
# Slopes per year
slopes = coef(mod)[1] + coef(mod)[c("year:VClass2Large Cars",
                           "year:VClass2Midsize Cars", "year:VClass2Minicompact Cars", "year:VClass2Minivan",
                           "year:VClass2Small Pickup Trucks", "year:VClass2Small Sport Utility Vehicle",
                           "year:VClass2Sport Utility Vehicle", "year:VClass2Standard Pickup Trucks",
                           "year:VClass2Station Wagon", "year:VClass2Subcompact Cars", "year:VClass2Two Seaters",
                           "year:VClass2Vans")]

# Vans decreasing, small SUV's most increasing (but lacks data for 1990, slope is exxagerated). Midsize cars increased greatly.
sort(slopes)

predictions = expand.grid(
    year = 1990:2024,
    VClass2 = unique(gas_vehicles$VClass2)
)
predictions$pred = predict(mod, predictions, type = "response")

ggplot(subset(predictions)) +
    geom_line(aes(x = year, y = pred)) +
    geom_point(data = gas_vehicles, aes(x = year, y = comb08), alpha = 0.01) +
    facet_wrap(~VClass2)

# Account for engine power
mod = glm(data = gas_vehicles, formula = comb08 ~ year * VClass2 + displ + cylinders - 1)
summary(mod)
slopes = coef(mod)[1] + coef(mod)[c("year:VClass2Large Cars",
                           "year:VClass2Midsize Cars", "year:VClass2Minicompact Cars", "year:VClass2Minivan",
                           "year:VClass2Small Pickup Trucks", "year:VClass2Small Sport Utility Vehicle",
                           "year:VClass2Sport Utility Vehicle", "year:VClass2Standard Pickup Trucks",
                           "year:VClass2Station Wagon", "year:VClass2Subcompact Cars", "year:VClass2Two Seaters",
                           "year:VClass2Vans")]

# E.g. Two seaters were not improving overall, but accounting for engine power they are improving
# This implies that two-seaters are becoming more efficient, but they're being made more powerful
# Aligns with the fact everyone buys SUV's now, small cars for enthusiast drivers now.
sort(slopes)

predictions = gas_vehicles
predictions$pred = predict(mod, gas_vehicles, type = "response")

ggplot(predictions) +
    geom_point(aes(x = year, y = pred, color = displ)) +
    scale_color_viridis_c() +
    facet_grid(VClass2~cut(cylinders,3))

# MPG by transmission type trends
p1 = ggplot(gas_vehicles) +
    geom_smooth(aes(x = year, y = comb08, color = trans_type))

# Overall, manual seems more efficient
glm(data = gas_vehicles, formula = comb08 ~ trans_type - 1)
# Accounting for engine power, the difference goes away
glm(data = gas_vehicles, formula = comb08 ~ trans_type + displ + cylinders - 1)


p2 = ggplot(gas_vehicles) +
    geom_smooth(aes(x = year, y = displ, color = trans_type))

p3 = ggplot(gas_vehicles) +
    geom_smooth(aes(x = year, y = cylinders, color = trans_type))

# Manuals typically more efficient than automatics
# Manual MPG has dropped in recent years
# This can be explained by an increasing mix of more powerful manual vehicles.
# Manual cars used to be sold as base models, but are now sold as enthusiast
# models which so they tend to be more powerful
p1 / p2 / p3

