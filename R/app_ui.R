#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#' @importFrom plotly plotlyOutput
#' @importFrom lubridate as_date
#'
#' @importFrom lubridate today
#' @importFrom plotly plotlyOutput
#' @importFrom shinyWidgets pickerInput pickerOptions
app_ui <- function(request) {

  Variant =

  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      h1("FluView"),
      sidebarLayout(
        sidebarPanel(
          bookmarkButton(id = "bookmark1", label = "Bookmark current inputs"),
          actionButton("Build P-Chart", inputId = "goButton", class = "btn-success"),
          br(),
          br(),

          dateRangeInput(
            inputId = "dates",
            label = "Dates to include in SPC analysis",
            start = "2015-10-01",
            min = "2015-10-01",
            end = lubridate::today()
          ),
          shinyWidgets::pickerInput(
            "states",
            options = shinyWidgets::pickerOptions(
              `actions-box` = TRUE,
              `deselect-all-text` = "None",
              `select-all-text` = "All",
              liveSearch = TRUE,
              dropupAuto = TRUE
            ),
            multiple = TRUE,
            label = "States to Include",
            choices =
              states,
            selected =
              "California"),


          shinyWidgets::pickerInput(
            "lab",
            options = shinyWidgets::pickerOptions(
              `actions-box` = TRUE,
              `deselect-all-text` = "None",
              `select-all-text` = "All",
              liveSearch = TRUE,
              dropupAuto = TRUE
            ),
            multiple = TRUE,
            label = "Lab Type",
            choices =
              setNames(c("clinical_labs", "combined_prior_to_2015_16"), c("Clinical Lab", "Clinical and Public Health Labs Before 2015") ),
            #1/11/23 Addition ^
            selected =
              setNames(c("clinical_labs", "combined_prior_to_2015_16"), c("Clinical Lab", "Clinical and Public Health Labs Before 2015") )),
            #1/11/23 Addition ^
          shinyWidgets::pickerInput(
            "variant",
            options = shinyWidgets::pickerOptions(
              `actions-box` = TRUE,
              `deselect-all-text` = "None",
              `select-all-text` = "All",
              liveSearch = TRUE,
              dropupAuto = TRUE
            ),
            multiple = TRUE,
            label = "Variant",
            choices =
              setNames(c("a", "b", "h3n2v"), c("Influenza A", "Influenza B", "Influenza H3N2")),
            #1/11/23 Addition ^
            selected =
              setNames(c("a", "b", "h3n2v"), c("Influenza A", "Influenza B", "Influenza H3N2"))),
          #1/11/23 Addition ^
          #https://shinyapps.dreamrs.fr/shinyWidgets/
          shiny::numericInput(
            inputId = "Lim_Min",
            label = "Minimum phase length before a special cause can be detected",
            min = 1,
            step = 1,
            value = 4
          ),


        HTML("<b>Current Data Source: </b>"),
        textOutput("data_source"),
        br(),

        HTML("<b>Last CDC Database Connection Attempt: </b>"),
        textOutput("last_connection_time"),

        br(),
        actionButton(inputId = "reloadingCDC",label = "Connect to CDC Database", class = "btn-success")

        ),
        mainPanel(
          h2("Test Positivity Rates"),
          fluidRow(
            downloadButton('downloadData', 'Download Chart Data')
          ),

          plotly::plotlyOutput("graph1") |> fluidRow(),
          h2("Test Counts"),
          plotly::plotlyOutput("graph2") |> fluidRow()

        )
      )

    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "FluView"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
