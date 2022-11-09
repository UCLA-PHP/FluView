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
  setBookmarkExclude(c("bookmark1", "goButton"))

  observeEvent(
    input$bookmark1,
    {
      session$doBookmark()

    })

  onBookmarked(
    function(url) {
      # updateQueryString(url)
      showBookmarkUrlModal(url)
    }
  )


  years =
    seq(
      from = input$dates[1] |> lubridate::year(),
      to = input$dates[2] |> lubridate::year()
    ) |>
    reactive()

  dataset0 =
    cdcfluview::who_nrevss(
      # years = 2015:year(today()),
      "state"
    )$clinical_labs

  dataset =
    dataset0 |>
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
      `TOTAL A&B` = sum(
        total_a |> as.numeric(),
        total_b |> as.numeric(),
        na.rm = TRUE)
    ) |>
    reactive()

  output$downloadData <- downloadHandler(
    filename = function() {
      paste('PH_chart-', Sys.time() |> gsub(pattern = "\\:", replacement = "-"), '.csv', sep= '')
    },
    content = function(con) {
      write.csv(chart1(), con)
    }
  )

  chart1 = eventReactive(
    input$goButton,
    {
      validate(need(nrow(dataset()) > 0, "No data found for these filter settings."))
      chart =
        dataset() |>
        dplyr::rename(
          n = `TOTAL A&B`,
          N = `total_specimens`,
          date = wk_date) |>
        shewhart.hybrid::PH_Chart(Lim_Min = input$Lim_Min)


    }
  )

  plot1 =
    reactive(
      {
        validate(need(nrow(dataset()) > 0, "No data found for these filter settings."))
        chart1() |> shewhart.hybrid::plot_run_chart()
      }
    )

  plot2 = eventReactive(
    input$goButton,
    {
      validate(need(nrow(dataset()) > 0, "No data found for these filter settings."))
      dataset() |> test_volume_chart()

    }
  )

  output$graph1 = plotly::renderPlotly(plot1())
  output$graph2 = plotly::renderPlotly(plot2())
}
