library(DiagrammeR)
library(shiny)


ui <- fluidPage(
  fluidPage(
    fluidRow(
      column(10, grVizOutput("dg")),
      column(2, verbatimTextOutput("print"))
    )
  )
)

server <- function(input, output, session) {
  output$dg <- renderGrViz({
    render_graph(test_4, layout = "nicely")
  })
  user_txt <- reactive({
    req(input$dg_click)
    nodeval <- input$dg_click$nodeValues[[1]]
    user <- output_nodes_attr[output_nodes_attr[,3] == nodeval,4]
    
    return(paste("Created by: ", user))
           
  })
  
  time_txt <- reactive({
    req(input$dg_click)
    nodeval <- input$dg_click$nodeValues[[1]]
    time <- output_nodes_attr[output_nodes_attr[,3] == nodeval,5] 

    
    return(paste("Last modified time: ", time))
                 
  })
  
  path_txt <- reactive({
    req(input$dg_click)
    nodeval <- input$dg_click$nodeValues[[1]]
    path <- output_nodes_attr[output_nodes_attr[,3] == nodeval,6]
    
    return(paste("File path: ", path))
  })
  
  output$print <- renderText({
    req(user_txt())
    paste0(user_txt(), "\n",
           time_txt(), "\n",
           path_txt(), "\n")
  })
  
}

shinyApp(ui, server)
