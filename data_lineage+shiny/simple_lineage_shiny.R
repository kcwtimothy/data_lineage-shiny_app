library(DiagrammeR)
library(shiny)
source("simple_lineage.R", local = TRUE)


ui <- fluidPage(
  fluidPage(
    fluidRow(
      column(10, grVizOutput("dg")),
      column(2, verbatimTextOutput("print")),
      column(2, uiOutput("url"))
    ),
    verbatimTextOutput("script")
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
    
    return(paste(nodeval, "Created by: ", user))
           
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
    
    return(path)
    # return(paste("File path: ", url(path)))
  })
  
  output$print <- renderText({
    req(user_txt())
    paste0(user_txt(), "\n",
           time_txt(), "\n",
           path_txt(), "\n")
  })
  
  output$url <- renderUI({
    req(input$dg_click)
    url <- a("Script path", href = path_txt())
    includeScript(path_txt())
    tagList("Click to script: ", url)
  })
  
  output$script <- renderText({
    req(input$dg_click)
    # includeScript(path_txt())
    # con <- file(path_txt())
    readLines(path_txt())
  })
  
  # observeEvent(output$print$click)
  
}

shinyApp(ui, server)
