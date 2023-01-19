library(shiny)

ui <- fluidPage(
actionButton("go", "Go"),
textOutput("test"),
tableOutput("data")
)

server <- function(input, output, session) {
test = reactiveVal(FALSE)
dataset = reactiveVal(mtcars)
observeEvent(input$go,
             ignoreNULL = FALSE,
             ignoreInit = FALSE,
             {
               message("running")
               test(TRUE)
               dataset(iris)
             })
output$test = renderText(test())
output$data = renderTable(dataset()|> head())

}

shinyApp(ui, server)
