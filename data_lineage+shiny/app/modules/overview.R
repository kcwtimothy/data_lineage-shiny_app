
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
        textInput(ns("vals"), "Choose values to include", "")
    ),
    box(tableOutput(ns("res")), width = 12),
    box(textInput(ns("locateOutput"), "Locate Table", ""),
        grVizOutput(ns("dg")), width = 9),
    box(verbatimTextOutput(ns("node_txt")), width = 3),
    box(verbatimTextOutput(ns("script")), width = 6)
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
  
  # observe({
  #   reqColInTable(input$tableName, input$filter)
  #   df <- as_data_frame(pool %>% tbl(input$tableName) %>% select(input$filter))
  #   allUniqueVals <- unique(df[[input$filter]])
  #   updateSelectInput(session, "vals", 
  #                            choices = allUniqueVals)
  # })
  
  output$res <- renderTable({
    reqColInTable(input$tableName, input$filter)
    
    filterVar <- sym(input$filter)
    vals <- input$vals
    
    if (vals == ""){
      pool %>% tbl(input$tableName) %>% select(input$select)
    } else {
      pool %>% tbl(input$tableName) %>% select(input$select) %>% 
        filter(filterVar %in% vals)
    }
  })
  
  dm_graph <- reactive({
    df <- as_data_frame(pool %>% tbl(input$tableName))
    input_nodes <- create_node_df(n = nrow(df), label = df$input)
    output_nodes <- create_node_df(n = nrow(df), label = df$output)
    all_nodes <- combine_ndfs(input_nodes, output_nodes)
    nodes_name <- c(df$input, df$output)
    
    edges <- create_edge_df(from = match(df$input, nodes_name),
                              to = match(df$output, nodes_name),
                              label = df$transformation)
    
    #remove duplicates
    unique_nodes <- unique(c(edges$from, edges$to))
    #create a graph object 
    diagram <- create_graph(all_nodes[unique_nodes,], edges)
    
  })
  
  dm_graph_scope <- reactive({
    df <- as_data_frame(pool %>% tbl(input$tableName))
    t1 <- data.frame()
    for (i in 1:nrow(df)){
      if(df$output[i] == input$locateOutput){
        t1 <- bind_rows(t1, df[i, ])
      } 
    }
    t1 #t1 is the anchor table with target output

    for (i in 1:nrow(df)){
      sub_t <- get(paste0("t", i))
      vector <- sub_t[,2] #create a vector of input
      new_val <- data.frame()
      for(j in 1:nrow(df)){ #simple logic to include all the tables with duplicates
        if(any(vector %in% df$output[j])){
          #check whether these input exist in other output rows and repeat itself
          new_val <- bind_rows(new_val, df[j,])
        } 
      }
      if (dim(new_val)[1] == 0){
        t1
        break
      } else {
        assign(paste0("t", i+1), new_val)
      }
    }
    #rbind these duplicate rows hence extract only the unique ones
    final <-unique(do.call(rbind,
                             mget(ls(pattern="^t"))))


    input_nodes_scope <- create_node_df(n = nrow(final), label = final$input)
    output_nodes_scope <- create_node_df(n = nrow(final), label = final$output)
    all_nodes_scope <- combine_ndfs(input_nodes_scope, output_nodes_scope)
    nodes_name_scope <- c(final$input, final$output)

    edges_scope <- create_edge_df(from = match(final$input, nodes_name_scope),
                            to = match(final$output, nodes_name_scope),
                            label = final$transformation)

    #remove duplicates
    unique_nodes_scope <- unique(c(edges_scope$from, edges_scope$to))
    #create a graph object
    diagram_scope <- create_graph(all_nodes_scope[unique_nodes_scope,], edges_scope)

  })
  
  output$scope <- renderTable({
    df <- as_data_frame(pool %>% tbl(input$tableName))
    t1 <- data.frame()
    for (i in 1:nrow(df)){
      if(df$output[i] == input$locateOutput){
        t1 <- bind_rows(t1, df[i, ])
      } 
    }
    t1 #t1 is the anchor table with target output
    
    for (i in 1:nrow(df)){
      sub_t <- get(paste0("t", i))
      vector <- sub_t[,2] #create a vector of input
      new_val <- data.frame()
      for(j in 1:nrow(df)){ #simple logic to include all the tables with duplicates
        if(any(vector %in% df$output[j])){
          #check whether these input exist in other output rows and repeat itself
          new_val <- bind_rows(new_val, df[j,])
        } 
      }
      if (dim(new_val)[1] == 0){ #break the loop if there's only single layer relation
        t1
        break
      } else {
        assign(paste0("t", i+1), new_val)
      }
    }
    #rbind these duplicate rows hence extract only the unique ones
    final <-unique(do.call(rbind,
                           mget(ls(pattern="^t"))))
    input_nodes_scope <- create_node_df(n = nrow(final), label = final$input)
    output_nodes_scope <- create_node_df(n = nrow(final), label = final$output)
    all_nodes_scope <- combine_ndfs(input_nodes_scope, output_nodes_scope)
  })
  
  output_nodes_attr <- reactive({
    df <- as_data_frame(pool %>% tbl(input$tableName))
    create_node_df(nrow(df), label = df$output, 
                                      value = df$created_by,
                                      time = df$last_modified,
                                      path = df$script_path)
  })
    
  
   output$dg <- renderGrViz({
     if (input$locateOutput == ""){
       render_graph(dm_graph(), layout = "nicely")
     } else {
       render_graph(dm_graph_scope(), layout = "lr")
     }
   })
  
  
  user_txt <- reactive({
    req(input$dg_click)
    nodeval <- input$dg_click$nodeValues[[1]]
    user <- output_nodes_attr()[output_nodes_attr()[,3] == nodeval,4]
    
    return(paste("Created by: ", user))
    
  })
  
  time_txt <- reactive({
    req(input$dg_click)
    nodeval <- input$dg_click$nodeValues[[1]]
    time <- output_nodes_attr()[output_nodes_attr()[,3] == nodeval,5] 
    
    return(paste("Last modified time: ", time))
    
  })
  
  path_txt <- reactive({
    req(input$dg_click)
    nodeval <- input$dg_click$nodeValues[[1]]
    path <- output_nodes_attr()[output_nodes_attr()[,3] == nodeval,6]
    
    return(path)
  })
  
  output$node_txt <- renderText({
    req(input$dg_click)
    paste0("\n",
           user_txt(), "\n",
           time_txt(), "\n",
           "Script path: ", path_txt(), "\n")
  })
  
  output$script <- renderText({
    req(input$dg_click)
    includeScript(path_txt())
    if(length(path_txt()) > 1){
      #sapply(path_txt(), readLines)
      print("test")
    } else {
      for (i in 1:length(readLines(path_txt()))){
        lines <- paste0(lines, readLines(path_txt())[i], "\n")
      }
      lines
    }
  })
  
}
