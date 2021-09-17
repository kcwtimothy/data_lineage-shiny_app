
overviewUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Current tables in database"),
    uiOutput(ns("tables")),
    
    #Moved reading tables to Overview
    box(title = "Table and columns", width = 5, solidHeader = TRUE, status = "primary",
        selectInput(ns("tableName"), "Choose a table", character(0)),
        checkboxGroupInput(ns("select"), "Choose columns to read")
    ),
    box(title = "Rows (optional)", width = 3, solidHeader = TRUE, status = "info",
        selectInput(ns("filter"), "Choose column to filter on", NULL),
        checkboxGroupInput(ns("vals"), "Choose values to include")
    ),
    box(tableOutput(ns("res")), width = 12),
    box(title = "Data Model", width = 5, solidHeader = TRUE, status = "primary",
        selectInput(ns("tableName_M"), "Choose a Master table", character(0))),
    box(tableOutput(ns("dm")), width = 12),
    box(verbatimTextOutput(ns("node_txt")), width = 4)
  )
}

overview <- function(input, output, session, pool,
                     reqTable, reqColInTable) {
  output$tables <- renderUI({
    all_tables <- tbls()
    for (i in seq_len(length(all_tables))) {
      tblName <- all_tables[[i]]
      fieldNames <- db_query_fields(pool, tblName)
      query <- sqlInterpolate(pool, 
                              "SELECT count(*) FROM sqlite_master AS tables 
                              WHERE type='table'")
      count <- dbGetQuery(pool, query)
      statement <- tags$li(paste0(
        "There are currently ", count, " tables in the database."))
    }
    tags$ul(statement)
  })
  
  #Moved reading tables to overview
  observeEvent(tbls(), {
    updateSelectInput(session, "tableName", choices = tbls())
  })
  
  observeEvent(tbls(), {
    updateSelectInput(session, "tableName_M", choices = tbls())
  })
  
  observe({
    reqTable(input$tableName)
    cols <- db_query_fields(pool, input$tableName)
    updateCheckboxGroupInput(session, "select", 
                             choices = cols, selected = cols, inline = TRUE)
  })
  
  observe({
    reqTable(input$tableName)
    req(input$select)
    updateSelectInput(session, "filter", choices = input$select)
  })
  
  observe({
    reqColInTable(input$tableName, input$filter)
    df <- as_data_frame(pool %>% tbl(input$tableName) %>% select(input$filter))
    allUniqueVals <- unique(df[[input$filter]])
    updateCheckboxGroupInput(session, "vals", 
                             choices = allUniqueVals, 
                             selected = allUniqueVals, inline = TRUE)
  })
  
  output$res <- renderTable({
    reqColInTable(input$tableName, input$filter)
    
    filterVar <- sym(input$filter)
    vals <- input$vals
    
    pool %>% tbl(input$tableName) %>% select(input$select) %>% 
      filter(filterVar %in% vals)
  })
  
  output$dm <- renderGrViz({
    #reqTable(input$tableName_M)
    df <- as_data_frame(pool %>% tbls(input$tableName_M))
    dm_graph <- function(n, input_name, output_name,
                         transformation = NULL,
                         input_type = NULL, output_type = NULL){
      require(DiagrammeR)
      
      #set up a table of nodes for every observation in the table
      input_nodes <- create_node_df(n = n, label = input_name, type = input_type)
      output_nodes <- create_node_df(n = n, label = output_name, type = output_type)
      all_nodes <- combine_ndfs(input_nodes, output_nodes)
      nodes_name <- c(input_name, output_name)
      
      #set up their linkage e.g from A to B, could also specify transformation
      if (missing(transformation)){
        
        edges <- create_edge_df(from = match(input_name, nodes_name),
                                to = match(output_name, nodes_name))
      } else {
        edges <- create_edge_df(from = match(input_name, nodes_name),
                                to = match(output_name, nodes_name),
                                label = transformation)
      }
      
      #remove duplicates
      unique_nodes <- unique(c(edges$from, edges$to))
      #create a graph object 
      diagram <- create_graph(all_nodes[unique_nodes,], edges, attr_theme = "lr")
    }
    
    output_nodes_2 <- create_node_df(nrow(df), label = df$output, 
                                        value = df$created_by,
                                        time = df$last_modified,
                                        path = df$script_path)
    diagram <- dm_graph(nrow(df), df$table_name, df$output, df$transformation)
    
    render_graph(diagram, layout = "nicely")
  })
  
  user_txt <- reactive({
    req(input$dm_click)
    nodeval <- input$dm_click$nodeValues[[1]]
    user <- output_nodes_2[output_nodes_2[,3] == nodeval,4]
    
    return(paste("Created by: ", user))
    
  })
  
  time_txt <- reactive({
    req(input$dm_click)
    nodeval <- input$dg_click$nodeValues[[1]]
    time <- output_nodes_2[output_nodes_2[,3] == nodeval,5] 
    
    
    return(paste("Last modified time: ", time))
    
  })
  
  path_txt <- reactive({
    req(input$dm_click)
    nodeval <- input$dg_click$nodeValues[[1]]
    path <- output_nodes_2[output_nodes_2[,3] == nodeval,6]
    
    return(paste("File path: ", path))
  })
  
  output$node_txt <- renderText({
    req(user_txt())
    paste0(user_txt(), "\n",
           time_txt(), "\n",
           path_txt(), "\n")
  })
}
