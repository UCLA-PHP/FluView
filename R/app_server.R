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
  progress <- shiny::Progress$new()
  progress$set(message = "Loading in Data", value = 0)

  #THE ISSUE IN WHY IT IS SO SLOW IS ON CDC'S END
  dataset_prelab = tryCatch(
    expr = {
      message('About to Load CDC Data')
      temp1 = cdcfluview::who_nrevss("state") |> combine_labs(lab_name = c("clinical_labs", "combined_prior_to_2015_16"))
      message('CDC Data is Loaded')
      temp1
    },
    error = function(e){
      temp2 = presavedCDCdata_20221117
      message('CDC is Not Available, Using Backed Up Data')
      temp2
    }

  )




  progress$set(message = "Ready to Analyze", value = 1)
  Sys.sleep(1)
  progress$close()

  observeEvent(
    input$lab,{
    shiny::updateDateRangeInput(session, inputId  = "dates",
                                start = if("combined_prior_to_2015_16" %in% input$lab) {"2010-10-01"}else{"2015-10-01"},
                                min = if("combined_prior_to_2015_16" %in% input$lab) {"2010-10-01"} else{"2015-10-01"},
                                end = if("clinical_labs" %in% input$lab){lubridate::today()} else{"2015-09-27"}
                               )

  }
  )

  observeEvent(
    input$lab, {

    shinyWidgets::updatePickerInput(
      session,
      inputId = "variant",
      choices = if("combined_prior_to_2015_16" %in% input$lab) {c("a", "b", "h3n2v")}else{c("a","b")},
      selected = input$variant
    )
    }
  )


  dataset =
    {
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
      progressData <- shiny::Progress$new(session = session, min = 0, max = 1 )
      progressData$set(message = "Creating Chart", value = 0)
      on.exit(progressData$set(message = "Displaying Chart", value = 1))
      on.exit(Sys.sleep(1), add = TRUE)
      on.exit(progressData$close(), add = TRUE)

      validate(need(nrow(dataset()) > 0, "No data found for these filter settings."))
      dataset() |> fv_p_chart()

    }
  )
  output$graph1 = plotly::renderPlotly(plot1())
}
