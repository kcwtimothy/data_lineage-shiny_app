get_table_fields <- function(pool, table_name) {
  con <- pool::poolCheckout(pool)
  on.exit(pool::poolReturn(con))
  
  fields <- DBI::dbListFields(con, table_name)
  return(fields)
}

overviewUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
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
      box(title = "Data Lineage Table", solidHeader = TRUE, status = "primary",
          style = "overflow-y:scroll; max-height: 600px; position:relative; align: centre;overflow-x:scroll; max-width: 1200;",
          tableOutput(ns("res")), width = 6),
      box(title = "Metadata", solidHeader = TRUE, status = "primary",
          style = "overflow-y:scroll; max-height: 600px; position:relative; align: centre;overflow-x:scroll; max-width: 1200;",
          tableOutput(ns("meta")), width = 6),
      setZoom(ns("dg"), scale = 1.25),
      box(style = "overflow-y:scroll; max-height: 1000px; position:relative; align: centre;overflow-x:scroll; max-width: 900;",
          textInput(ns("locateOutput"), "Locate Table", ""),
          grVizOutput(ns("dg")), width = 9),
      box(verbatimTextOutput(ns("node_txt")), width = 3),
      # box(verbatimTextOutput(ns("script")), width = 6),
      box(title = "Object Overview", width = 5, solidHeader = TRUE, status = "primary",
          selectInput(ns("tableName_2"), "Choose a table", character(0)),
          checkboxGroupInput(ns("select_2"), "Choose columns to read")
      ),
      box(title = "Rows (optional)", width = 3, solidHeader = TRUE, status = "info",
          selectInput(ns("filter_2"), "Choose column to filter on", NULL),
          textInput(ns("vals_2"), "Choose values to include", "")
      ),
      box(title = "Object description", solidHeader = TRUE, status = "primary",
          style = "overflow-y:scroll; max-height: 600px; position:relative; align: centre;overflow-x:scroll; max-width: 1200;",
          tableOutput(ns("res_2")), width = 12)
      )
    )
}

overview <- function(input, output, session, pool, pool_2,
                     reqTable, reqColInTable, reqTable_2, reqColInTable_2, get_table_fields) {
  output$tables <- renderUI({
    all_tables <- tbls_2()
    for (i in seq_len(length(all_tables))) {
      tblName <- all_tables[[i]]
      fieldNames <- get_table_fields(pool_2, tblName)
      query <- sqlInterpolate(pool_2, 
                              "SELECT count(*) FROM sqlite_master AS tables 
                              WHERE type='table'")
      count <- dbGetQuery(pool_2, query)
      statement <- tags$li(paste0(
        "There are currently ", count, " tables in the database."))
    }
    tags$ul(statement)
  })
  
  #Moved reading tables to overview
  observeEvent(tbls(), {
    updateSelectInput(session, "tableName", choices = tbls())
  })
  
  observeEvent(tbls_2(), {
    updateSelectInput(session, "tableName_2", choices = tbls_2())
  })
  
  observeEvent(input$dg_click$nodeValues[[1]], {
    nodeval <- input$dg_click$nodeValues[[1]]
    if (nodeval %in% tbls_2()){
      updateSelectInput(session, "tableName_2", choices = tbls_2(), selected = nodeval)
    }
  })
  
  observe({
    reqTable(input$tableName)
    cols <- get_table_fields(pool, input$tableName)
    updateCheckboxGroupInput(session, "select", 
                             choices = cols, selected = cols, inline = TRUE)
  })
  
  observe({
    reqTable_2(input$tableName_2)
    cols <- get_table_fields(pool_2, input$tableName_2)
    updateCheckboxGroupInput(session, "select_2", 
                             choices = cols, selected = cols, inline = TRUE)
  })
  
  observe({
    reqTable(input$tableName)
    req(input$select)
    updateSelectInput(session, "filter", choices = input$select)
  })
  
  observe({
    reqTable_2(input$tableName_2)
    req(input$select_2)
    updateSelectInput(session, "filter_2", choices = input$select_2)
  })
  
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
  
  output$meta <- renderTable({
    pool_2 %>% tbl("metadata") 
  })
  
  output$res_2 <- renderTable({
    reqColInTable_2(input$tableName_2, input$filter_2)
    
    filterVar <- sym(input$filter_2)
    vals <- input$vals_2
    
    if (vals == ""){
      pool_2 %>% tbl(input$tableName_2) %>% select(input$select_2)
    } else {
      pool_2 %>% tbl(input$tableName_2) %>% select(input$select_2) %>% 
        filter(filterVar %in% vals)
    }
  })
  
  dm_graph <- reactive({
    df <- dplyr::as_tibble(pool %>% tbl(input$tableName))
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
    df <- dplyr::as_tibble(pool %>% tbl(input$tableName))
    t1 <- data.frame()
    for (i in 1:nrow(df)){
      if (df$output[i] == input$locateOutput){
        t1 <- bind_rows(t1, df[i, ])
        }
    }
    if (dim(t1)[1] == 0){
      stop("No output objects found.")
    } else {
      t1 #t1 is the anchor table with target output
    }
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
    df <- dplyr::as_tibble(pool %>% tbl(input$tableName))
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
    df <- dplyr::as_tibble(pool_2 %>% tbl("metadata"))
    create_node_df(nrow(df), label = df$object_name,
                   value = df$object_summary,
                   path = df$script_path)
  })
    
  
   output$dg <- renderGrViz({
     if (input$locateOutput == ""){
       render_graph(dm_graph(), layout = "nicely")
     } else {
       render_graph(dm_graph_scope(), layout = "lr")
     }
   })
  
  
  summary_txt <- reactive({
    req(input$dg_click)
    nodeval <- input$dg_click$nodeValues[[1]]
    sum <- output_nodes_attr()[output_nodes_attr()[,3] == nodeval,4]
    
    return(paste("Brief Summary: ", sum))
    
  })
  
  path_txt <- reactive({
    req(input$dg_click)
    nodeval <- input$dg_click$nodeValues[[1]]
    path <- output_nodes_attr()[output_nodes_attr()[,3] == nodeval,5]
    
    return(path)
  })
  
  output$node_txt <- renderText({
    req(input$dg_click)
    paste0("\n",
           summary_txt(), "\n",
           "Script path: ", path_txt(), "\n")
  })
}
