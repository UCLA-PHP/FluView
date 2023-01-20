#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#' @importFrom cdcfluview who_nrevss
#' @importFrom dplyr summarize group_by filter rename
#' @importFrom lubridate year today %within% int_diff
#' @importFrom plotly renderPlotly
#' @importFrom shewhart.hybrid PH_Chart plot_run_chart
#' @importFrom shiny Progress updateDateRangeInput
#' @importFrom shinyWidgets updatePickerInput
#' @importFrom stringr str_replace_all
app_server <- function(input, output, session) {
  # Your application server logic
  setBookmarkExclude(c("bookmark1", "goButton"))

  observeEvent(
    input$bookmark1,
    session$doBookmark())

  onBookmarked(
    function(url) {
      # updateQueryString(url)
      showBookmarkUrlModal(url)
    }
  )

  data_source =
    paste("Presaved CDC Data, downloaded ",  attr(presaved_CDC_data, "date")) |>
    reactiveVal()

  last_load_attempt = reactiveVal(NA)
  last_load_result = reactiveVal(NA)

  dataset_prelab = reactiveVal(presaved_CDC_data)

  observeEvent(
    eventExpr = input$reloadingCDC,
    ignoreNULL = TRUE,
    # ignoreNULL = FALSE, #IgnoreNull makes it run initially,
    ignoreInit = FALSE, # ignoreInit makes it so it wont run twice (oddly)
    {
      progress <- shiny::Progress$new()
      progress$set(message = "Loading in Data", value = 0)

      last_load_attempt(Sys.time())

      cli::cli_alert('About to Load CDC Data')
      cdc =
        cdcfluview::who_nrevss("state") |>
        combine_labs(
          lab_name = c(
            "clinical_labs",
            "combined_prior_to_2015_16")) |>
        try()

      if(inherits(cdc, "try-error"))
      {
        last_load_result("failed")
        message('CDC is Not Available, Using Backed Up Data')
        progress$set(message = "CDC server unavailable", value = 1)
        Sys.sleep(1)
      } else
      {
        message('CDC Data is Loaded')
        last_load_result("succeeded")
        data_source(paste("CDC database: downloaded", Sys.time()))
        attr(cdc, "date") = Sys.time()
        dataset_prelab(cdc)
      }

      progress$set(message = "Ready to Analyze", value = 1)
      Sys.sleep(1)
      progress$close()
    })

  shiny::updateDateRangeInput(
    session,
    inputId = "dates",
    start = if("combined_prior_to_2015_16" %in% input$lab) "2010-10-01" else "2015-10-01",
    min = if("combined_prior_to_2015_16" %in% input$lab) "2010-10-01" else "2015-10-01",
    end = if("clinical_labs" %in% input$lab) lubridate::today() else "2015-09-27"
  ) |>
    observeEvent(eventExpr = input$lab)



  observeEvent(
    input$lab, {

      choices =
        c(
          "Influenza A Virus" = "a",
          "Influenza B Virus" = "b",
          "H3N2 Virus" = if("combined_prior_to_2015_16" %in% input$lab) "h3n2v"
        )

      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "variant",
        choices = choices,
        selected = input$variant
      )
    }
  )

  output$data_source = renderText(data_source())
  output$last_connection_time =
    glue::glue(
      # "<b>Last CDC Server Connection Attempt:</b> ",
      as.character(last_load_attempt()),
      " ({last_load_result()})") |>
    renderText()


  # output$reloadCDC = renderUI({actionButton(inputId = "reloadingCDC",label = "Reload Live CDC", class = "btn-success")})
  # eventReactive(
  #   input$reloadingCDC,
  #   {
  #     message(trigger)
  #     trigger = "On"
  #   }
  # )


  dataset =
    dataset_prelab() |>
    merge_data(
      lab_types = input$lab,
      regions = input$states,
      dates = input$dates,
      variants = input$variant,
      verbose = TRUE
    ) |>
    reactive()

  output$downloadData <- downloadHandler(
    filename = function() {
      paste(
        'PH_chart-',
        Sys.time() |> gsub(pattern = "\\:", replacement = "-"),
        '.csv',
        sep = '')
    },
    content = function(con) {
      write.csv(chart1(), con)
    }
  )

  chart1 = eventReactive(
    input$goButton,
    {

      progressData <- shiny::Progress$new(session = session, min = 0, max = 1 )
      progressData$set(message = "Creating Chart", value = 0)

      on.exit(progressData$set(message = "Displaying Chart", value = 1))
      on.exit(Sys.sleep(1), add = TRUE)
      on.exit(progressData$close(), add = TRUE)

      validate(need(nrow(dataset()) > 0, "No data found for these filter settings."))
      validate(need(input$Lim_Min > 0, "Need a minimum phase length > 0."))

      chart =
        dataset() |>
        dplyr::rename(
          n = `TOTAL POSITIVE`,
          N = `total_specimens`,
          date = wk_date) |>
        shewhart.hybrid::PH_Chart(Lim_Min = input$Lim_Min |> ceiling()) |>
        suppressWarnings()

    }
  )

  plot1 =
    eventReactive(
      input$goButton,
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
