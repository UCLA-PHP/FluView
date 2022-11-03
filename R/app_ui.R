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
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      h1("FluView"),
      sidebarLayout(
        sidebarPanel(
          actionButton("Build P-Chart", inputId = "goButton", class = "btn-success"),
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
              c("clinical_labs", "combined_prior_to_2015_16"),
            selected =
              "clinical_labs"),

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
              c("a", "b"),
            selected =
              "a")

        ),
        mainPanel(
          plotly::plotlyOutput("graph1")
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
