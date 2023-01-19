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


  years =
    seq(
      from = input$dates[1] |> lubridate::year(),
      to = input$dates[2] |> lubridate::year()
    ) |>
    reactive()


  #dataset0 = cdcfluview::who_nrevss("state")$clinical_labs
  # progress <- shiny::Progress$new()
  # progress$set(message = "Loading in Data", value = 0)

  #THE ISSUE IN WHY IT IS SO SLOW IS ON CDC'S END
  data_placeHolder = "placeHolder"
  #trigger = "On"
  #message(trigger)
  #trigger = "Off"
    #Observeevent here
dataset_prelab = NULL
makeReactiveBinding("dataset_prelab")
observeEvent(eventExpr = input$reloadingCDC,  ignoreNULL = FALSE, ignoreInit = TRUE, #IgnoreNull makes it run initially, ignoreInit makes it so it wont run twice (oddly)
               {
                 message("DID THIS RUN")
                 progress <- shiny::Progress$new()
                 progress$set(message = "Loading in Data", value = 0)
  # dataset_prelab <<- tryCatch( #<<- symbol will make it in the parent scope (be able to use outside of observeEvent)
  #   expr =
  #     {
  #       message('About to Load CDC Data')
  #       #stop()
  #       temp1 =
  #         cdcfluview::who_nrevss("state") |>
  #         combine_labs(
  #           lab_name = c(
  #             "clinical_labs",
  #             "combined_prior_to_2015_16"))
  #
  #       message('CDC Data is Loaded')
  #       data_placeHolder <<- "CDC"
  #
  #       temp1
  #     },
  #   error = function(e)
  #   {
  #
  #     temp2 = presaved_CDC_data
  #     message('CDC is Not Available, Using Backed Up Data')
  #     #message(temp2[1,1])
  #     temp2
  #
  #   }
  #
  # )
                 dataset_prelab <<- try({
                   message('About to Load CDC Data')
                   #stop()
                   temp1 =
                     cdcfluview::who_nrevss("state") |>
                     combine_labs(
                       lab_name = c(
                         "clinical_labs",
                         "combined_prior_to_2015_16"))

                   message('CDC Data is Loaded')


                   temp1
                 }
                 )
                 if(inherits(dataset_prelab, "try-error")){
                   dataset_prelab = presaved_CDC_data
                   message('CDC is Not Available, Using Backed Up Data')
                   data_placeHolder <<- "PRESAVED"
                 }else{data_placeHolder <<- "CDC"}

  progress$set(message = "Ready to Analyze", value = 1)
  Sys.sleep(1)
  progress$close()
               })
#  message(trigger)

  #Wrap in observe Event
# data_placeHolder = "empty"
# dataset_prelab = c()
# observeEvent(eventExpr = input$reloadingCDC, ignoreNULL = FALSE,
#             {

#
# })
  # progress$set(message = "Ready to Analyze", value = 1)
  # Sys.sleep(1)
  # progress$close()


  shiny::updateDateRangeInput(
    session,
    inputId  = "dates",
    start = if("combined_prior_to_2015_16" %in% input$lab) "2010-10-01" else "2015-10-01",
    min = if("combined_prior_to_2015_16" %in% input$lab) "2010-10-01" else "2015-10-01",
    end = if("clinical_labs" %in% input$lab) lubridate::today() else "2015-09-27"
  ) |> observeEvent(eventExpr = input$lab)



  observeEvent(
    input$lab, {

      choices = if("combined_prior_to_2015_16" %in% input$lab) setNames(c("a", "b", "h3n2v"), c("A Virus", "B Virus", "H3N2 Virus")) else setNames(c("a", "b"), c("A Virus", "B Virus"))

      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "variant",
        choices = choices,
        selected = input$variant
      )
    }
  )

  output$data_text = renderText({paste0("Data Source:\n", if(data_placeHolder == "CDC") "Live CDC" else "Presaved CDC Data" )})
  message(data_placeHolder)
  message("ran")
    output$reloadCDC = renderUI({actionButton(inputId = "reloadingCDC",label = "Reload Live CDC", class = "btn-success")})
    # eventReactive(
    #   input$reloadingCDC,
    #   {
    #     message(trigger)
    #     trigger = "On"
    #   }
    # )


  dataset =
    {
      message("before merging data")
      #message(dataset_prelab[1,1])
      temp =
        dataset_prelab |>
        dplyr::filter(
          labType %in% input$lab,
          complete.cases(total_specimens),
          region %in% input$states,
          wk_date %within% (input$dates |> lubridate::int_diff())) |>
        dplyr::group_by(wk_date) |>
        dplyr::summarize(
          .groups = "drop",
          total_specimens =
            total_specimens |>
            as.numeric() |>
            sum(na.rm = TRUE),
          `TOTAL POSITIVE` =
            input$variant |>
            stringr::str_replace_all(
              c("a" = "total_a", "b" = "total_b")) |>
            dplyr::across() |>
            sum(na.rm = TRUE))

      message("after merging data")
      temp
    } |> reactive()

  #message("Ready to Analyze", Sys.time())
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
  #     wk_date %within% (input$dates |> lubridate::int_diff())) |>
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
        shewhart.hybrid::PH_Chart(Lim_Min = input$Lim_Min |> ceiling())

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
