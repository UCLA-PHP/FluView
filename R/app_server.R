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
#<<<<<<< HEAD
#=======
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

#>>>>>>> 32e6f8596e117ff2d197f952d3bb5a70f8a82b17

  years =
    seq(
      from = input$dates[1] |> lubridate::year(),
      to = input$dates[2] |> lubridate::year()
    ) |>
    reactive()

  #dataset0 = cdcfluview::who_nrevss("state")$clinical_labs
  progress <- shiny::Progress$new()
  progress$set(message = "Loading in Data", value = 0)

  #THE ISSUE IN WHY IT IS SO SLOW IS ON CDC'S END
  dataset_prelab = tryCatch(
    expr =
      {
        message('About to Load CDC Data')

        temp1 =
          cdcfluview::who_nrevss("state") |>
          combine_labs(
            lab_name = c(
              "clinical_labs",
              "combined_prior_to_2015_16"))

        message('CDC Data is Loaded')
        temp1
      },
    error = function(e)
    {

      temp2 = presaved_CDC_data
      message('CDC is Not Available, Using Backed Up Data')
      temp2
    }

  )




  progress$set(message = "Ready to Analyze", value = 1)
  Sys.sleep(1)
  progress$close()


  shiny::updateDateRangeInput(
    session,
    inputId  = "dates",
    start = if("combined_prior_to_2015_16" %in% input$lab) "2010-10-01" else "2015-10-01",
    min = if("combined_prior_to_2015_16" %in% input$lab) "2010-10-01" else "2015-10-01",
    end = if("clinical_labs" %in% input$lab) lubridate::today() else "2015-09-27"
  ) |> observeEvent(eventExpr = input$lab)



  observeEvent(
    input$lab, {

      choices = if("combined_prior_to_2015_16" %in% input$lab) c("a", "b", "h3n2v") else c("a","b")

      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "variant",
        choices = choices,
        selected = input$variant
      )
    }
  )


  dataset =
    {
#<<<<<<< HEAD
      print("before")
    temp =  dataset_prelab |>
    dplyr::filter(
      labType %in% input$lab,
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
      `TOTAL POSITIVE` = if(input$variant[1] == "a" &
                            length(input$variant) == 1) {
        sum(total_a |> as.numeric(), na.rm = TRUE)
      }
      else if (input$variant[1] == "b" &
               length(input$variant) == 1) {
        sum(total_b |> as.numeric(),  na.rm = TRUE)
      }
      else if (input$variant[1] == "h3n2v" &
               length(input$variant) == 1) {
        sum(h3n2v |> as.numeric(),  na.rm = TRUE)
      }
      else if ("a" %in% input$variant &
               "b" %in% input$variant & length(input$variant) == 2) {
        sum(total_a |> as.numeric(),
            total_b |> as.numeric(),  na.rm = TRUE)
      }
      else if ("a" %in% input$variant &
               "h3n2v" %in% input$variant & length(input$variant) == 2) {
        sum(total_a |> as.numeric(),
            h3n2v |> as.numeric(),
            na.rm = TRUE)
      }

      else if ("h3n2v" %in% input$variant &
               "b" %in% input$variant & length(input$variant) == 2) {
        sum(total_b |> as.numeric(),
            h3n2v |> as.numeric(),
            na.rm = TRUE)
      }
      else {
        sum(total_a |> as.numeric(),
            total_b |> as.numeric(),
            h3n2v |> as.numeric(),
            na.rm = TRUE)
      }
    )
    print("after")
    temp
#=======
      message("before merging data")

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
#>>>>>>> 32e6f8596e117ff2d197f952d3bb5a70f8a82b17
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

#<<<<<<< HEAD
  plot1 = eventReactive(
#=======
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
)
  chart1 = eventReactive(
#>>>>>>> 32e6f8596e117ff2d197f952d3bb5a70f8a82b17
    input$goButton,
    {

      progressData <- shiny::Progress$new(session = session, min = 0, max = 1 )
      progressData$set(message = "Creating Chart", value = 0)

      on.exit(progressData$set(message = "Displaying Chart", value = 1))
      on.exit(Sys.sleep(1), add = TRUE)
      on.exit(progressData$close(), add = TRUE)

      validate(need(nrow(dataset()) > 0, "No data found for these filter settings."))
#<<<<<<< HEAD
      dataset() |> fv_p_chart()
#=======
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

#>>>>>>> 32e6f8596e117ff2d197f952d3bb5a70f8a82b17

    }
  )
  output$graph1 = plotly::renderPlotly(plot1())
}
