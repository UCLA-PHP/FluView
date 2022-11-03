#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#' @importFrom plotly renderPlotly
#' @importFrom lubridate %within% int_diff
#' @importFrom cdcfluview who_nrevss
#' @importFrom dplyr filter
#' @importFrom plotly renderPlotly
#' @importFrom cdcfluview who_nrevss
#' @importFrom dplyr summarize group_by filter
#' @importFrom lubridate year
#' @importFrom plotly renderPlotly
#' @import lubridate
app_server <- function(input, output, session) {
  # Your application server logic

  years =
    seq(
      from = input$dates[1] |> lubridate::year(),
      to = input$dates[2] |> lubridate::year()
    ) |>
    reactive()

  #dataset0 = cdcfluview::who_nrevss("state")$clinical_labs

  dataset_prelab = cdcfluview::who_nrevss("state") #DP Added
  min(cdcfluview::who_nrevss("state")$clinical_labs$wk_date)
  labs = reactive({
    input$lab
  })

  variants = reactive({
    input$variant
  })

  observeEvent(
    labs(),
    shiny::updateDateRangeInput(session, inputId  = "dates",
                                start = if("combined_prior_to_2015_16" %in% labs()) {"2010-10-01"}else{"2015-10-01"},
                                min = if("combined_prior_to_2015_16" %in% labs()) {"2010-10-01"} else{"2015-10-01"},
                                end = if("clinical_labs" %in% labs()){lubridate::today()} else{"2015-09-27"}
                               )
  )


  dataset =
    dataset_prelab |> combine_labs(lab_name = c("combined_prior_to_2015_16", "clinical_labs")) |>
    dplyr::filter(
      !is.na(total_specimens),
      !is.na(total_a),
      !is.na(total_b),
      region %in% input$states,
      wk_date %within% (input$dates |> int_diff())) |>
    dplyr::group_by(wk_date) |>
    dplyr::summarize(
      .groups = "drop",
      total_specimens =
        total_specimens |>
        as.numeric() |>
        sum(na.rm = TRUE),
      `TOTAL POSITIVE` = if(variants()[1] == "a") {sum(
        total_a |> as.numeric(), na.rm = TRUE)}
          else if(variants()[1] == "b") {sum(
        total_b |> as.numeric(),  na.rm = TRUE)}
          else {sum(
          total_a |> as.numeric(),
          total_b |> as.numeric(),
          na.rm = TRUE)} ) |>
    reactive()

  # test = cdcfluview::who_nrevss("state")
  # testing =
  #   test[["clinical_labs"]] |> as_tibble("clinical_labs")
  # #DP Testing
  # dataset =
  #   dataset_prelab[[input$lab]] |>
  #   dplyr::filter(
  #     !is.na(total_specimens),
  #     !is.na(total_a),
  #     !is.na(total_b),
  #     region %in% input$states,
  #     wk_date %within% (input$dates |> int_diff())) |>
  #   dplyr::group_by(wk_date) |>
  #   dplyr::summarize(
  #     .groups = "drop",
  #     total_specimens =
  #       total_specimens |>
  #       as.numeric() |>
  #       sum(na.rm = TRUE),
  #     `TOTAL A` = sum(
  #       a_2009_h1n1 |> as.numeric(),
  #       a_h3 |> as.numeric(),
  #       a_subtyping_not_performed |> as.numeric(),
  #       na.rm = TRUE),
  #     `TOTAL B` = sum(
  #       b |> as.numeric(),
  #       bvic |> as.numeric(),
  #       byam |> as.numeric(),
  #       na.rm = TRUE)
  #
  #   ) |>
  #   reactive()

  plot1 = eventReactive(
    input$goButton,
    {
      validate(need(nrow(dataset()) > 0, "No data found for these filter settings."))
      dataset() |> fv_p_chart()
    }
  )

  output$graph1 = plotly::renderPlotly(plot1())
}
