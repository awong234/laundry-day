# Start R here using .Rproj file in this folder.

# Strategy for reproducible "virtual" library. Also in the .Rprofile, but put here for greater visibility.
dir.create(file.path('lib', version$platform, version$major, version$minor), showWarnings = FALSE, recursive = TRUE)
.libPaths(file.path('lib', version$platform, version$major, version$minor))
options(repos = c("CRAN" = 'https://packagemanager.posit.co/cran/2025-02-07'))
if (! 'pak' %in% .packages(all.available=TRUE)) install.packages('pak', type = 'binary')
pak::pkg_install(c('shiny', 'readxl', 'curl', 'dplyr', 'janitor', 'scales', 'DT'))

library(shiny)
library(dplyr)
library(DT)

clean_report_data = function(report_links) {

    # Download reports
    lapply(report_links, \(x) {
        file_out = file.path(basename(x))
        curl::curl_fetch_disk(x, file_out)
    })

    file_names = list.files(pattern = '*.xlsx', full.names = TRUE)

    years = substr(file_names, 47, 57)

    report_data = lapply(file_names, \(x) {
        sheet_names = readxl::excel_sheets(x)
        if (identical(sheet_names, 'Engagement')) {
            engagement = readxl::read_excel(x, sheet = 'Engagement', skip = 5)
            engagement$Type = 'TVorFilm'
            engagement$`Release Date` = as.Date(engagement$`Release Date`)
        }
        if (identical(sheet_names, c("TV", "Film"))) {
            tv = readxl::read_excel(x, sheet = 'TV', skip = 5)
            film = readxl::read_excel(x, sheet = 'Film', skip = 5)
            out_list = list(TV=tv, Film=film)
            engagement = bind_rows(out_list, .id = 'Type')
            engagement$`Release Date` = as.Date(engagement$`Release Date`)
        }
        return(engagement)
    })

    names(report_data) = years
    report_data = bind_rows(report_data, .id = 'Period')
    report_data = report_data %>% janitor::clean_names()
    report_data = report_data %>% relocate(type, .after = period)
    report_data
}

runtime_as_decimal_hours = function(x) {
    x[x == "*"] = NA_character_
    x[is.na(x)] = NA_real_
    split_time = strsplit(x, ":")
    sapply(split_time, \(x) {
        as.numeric(x[1]) + as.numeric(x[2])/60
    })
}

# Get netflix data
# Links
report_links = list(
    'https://assets.ctfassets.net/4cd45et68cgf/1HyknFM84ISQpeua6TjM7A/97a0a393098937a8f29c9d29c48dbfa8/What_We_Watched_A_Netflix_Engagement_Report_2023Jan-Jun.xlsx',
    'https://assets.ctfassets.net/4cd45et68cgf/inuAnzotdsAEgbInGLzH5/1be323ba419b2af3a96bffa29acc31a3/What_We_Watched_A_Netflix_Engagement_Report_2023Jul-Dec.xlsx',
    'https://assets.ctfassets.net/4cd45et68cgf/2PoZlfdc46dH2gQvI8eUzI/9db5840720c47acfcf7b89ffe2402860/What_We_Watched_A_Netflix_Engagement_Report_2024Jan-Jun.xlsx'
)

reports = clean_report_data(report_links)

ui <- fluidPage(
    fluidRow(
        column(width = 2,
               textInput('search', label = 'Search (regex):', value = ''),
               selectInput('period', label = 'Period:', choices = unique(reports$period), selected = unique(reports$period)[1], multiple = TRUE),
               selectInput('global', label = 'Available Globally', choices = c("Yes", "No"), selected = c("Yes", "No"), multiple = TRUE),
               selectInput('media', label = 'Media Type', choices = unique(reports$type), selected = c("TVorFilm", "TV", "Film"), multiple = TRUE),
               numericInput('minviews', label = 'Min. Views', value = 0, min = 0, max = max(reports$views, na.rm = TRUE), step = 1000)
        ),
        column(width = 10, {
            DT::DTOutput('results')
        })
    )
)

server <- function(input, output, session) {

    observe({
        message('inputs')
        inputs = c('global', 'media', 'period', 'search', 'minviews')
        lapply(inputs, \(x) {
            input[[x]] |> print()
        })
        message('rows')
        distinct(reports, period, type) %>% print()
        reports_reactive() %>% nrow() %>% print()

    })

    reports_reactive = reactive({
        reports %>%
            filter(
                available_globally %in% input$global,
                type %in% input$media,
                period %in% input$period,
                (views >= input$minviews) | is.na(views)
            ) %>%
            mutate(
                hours_per_view = hours_viewed / views,
                runtime_h      = runtime_as_decimal_hours(runtime),
                multiplay_idx  = ifelse(is.na(hours_per_view), NA_real_, hours_per_view / runtime_h),
                ranked_hours   = dense_rank(-hours_viewed),
                ranked_views   = dense_rank(-views),
                ranked_hpv     = dense_rank(-hours_per_view),
                ranked_multi   = dense_rank(-multiplay_idx)
            ) %>%
            ungroup() %>%
            filter(grepl(pattern=input$search, title, perl=TRUE, ignore.case = TRUE)) %>%
            {.}
    })

    output$results = DT::renderDataTable({
        reports = reports_reactive()
        reports %>%
            DT::datatable(
                colnames = stringr::str_to_title(colnames(reports)) |> gsub('_', x = _, ' '),
                style = 'bootstrap', class = 'table-bordered table-condensed'
            ) %>%
            DT::formatDate("release_date") %>%
            DT::formatRound(columns = c('hours_per_view', 'runtime_h'), digits = 1) %>%
            DT::formatRound(columns = c('multiplay_idx'), digits = 3) %>%
            DT::formatCurrency(columns = c('hours_viewed', 'views'), currency = '', digits = 0)
    })
}

if (interactive()) shinyApp(ui, server)
