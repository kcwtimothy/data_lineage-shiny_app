get_table_fields <- function(pool, table_name) {
  con <- pool::poolCheckout(pool)
  on.exit(pool::poolReturn(con))
  
  fields <- DBI::dbListFields(con, table_name)
  return(fields)
}

createTableUI <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    box(title = "Create ordinary tables", status = "primary",
        solidHeader = TRUE, width = 8,
        textInput(ns("tableName"), "Table name"),
        numericInput(ns("ncols"), "Number of columns", 1, min = 1)),
    box(width = 12,
        style = "overflow-y:scroll; max-height: 800px; position:relative; align: centre;overflow-x:scroll; max-width: 1200;",
        uiOutput(ns("cols")),
        actionButton(ns("create"), "Create table", class = "pull-right btn-info"),
        actionButton(ns("create_multiple"), "Create Without reset", class = "pull-right btn-info")),
    box(title = "Create Data Lineage Tables", status = "primary",
        solidHeader = TRUE, width = 8,
        textInput(ns("masterName"), "Table name"),
        actionButton(ns("create_master"), "Create Table",
                 class = "pull-right btn-info"))
  )
}

createTable <- function(input, output, session, pool, pool_2, get_table_fields) {
  
  output$cols <- renderUI({
    cols <- vector("list", input$ncols)
    for (i in seq_len(input$ncols)) {
      cols[[i]] <- box(
        title = paste("Column", i), width = 4, 
        solidHeader = TRUE, status = "primary",
        textInput(session$ns(paste0("colName", i)), "Column name"),
        selectInput(session$ns(paste0("colType", i)), "Column type", 
          c(Character = "VARCHAR", Integer = "INT")
        )
      )
    }
    cols
  })
  
  observeEvent(input$create, {
    if (input$ncols < 1) {
      showModal(modalDialog(
        title = "No columns", 
        "Each table must have one or more columns",
        easyClose = TRUE, footer = NULL
      ))
      return()
    }
    
    if (input$tableName %in% c(tbls_2(), "")) {
      if (input$tableName == "") {
        msg <- "All tables must be named"
      } else {
        msg <- "There's already a table with this name in the DB"
      }
      showModal(modalDialog(
        title = "Invalid table name", msg,
        easyClose = TRUE, footer = NULL
      ))
      return()
    }
    
    finalCols <- character(0)
    for (i in seq_len(input$ncols)) {
      colNameID <- paste0("colName", i)
      colTypeID <- paste0("colType", i)
      colName <- input[[colNameID]]
      colType <- input[[colTypeID]]
      finalCols[colName] <- colType
    }
    
    if (any(names(finalCols) == "")) {
      showModal(modalDialog(
        title = "Invalid column name",
        "All columns must be named",
        easyClose = TRUE, footer = NULL
      ))
      return()
    }
    
    tryCatch({
      dbCreateTable(pool_2, input$tableName, finalCols)
      showModal(modalDialog(
        title = "Successfully Created", "An Object Description Table is created",
        easyClose = TRUE, footer = NULL
      ))
      reset("cols")
      reset("ncols")
      reset("tableName")
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        paste("Failed to create table:", e$message),
        easyClose = TRUE, footer = NULL
      ))
    })
  })
  
  observeEvent(input$create_multiple, {
    if (input$ncols < 1) {
      showModal(modalDialog(
        title = "No columns", 
        "Each table must have one or more columns",
        easyClose = TRUE, footer = NULL
      ))
      return()
    }
    
    if (input$tableName %in% c(pool::dbListTables(pool_2), "")) {
      if (input$tableName == "") {
        msg <- "All tables must be named"
      } else {
        msg <- "There's already a table with this name in the DB"
      }
      showModal(modalDialog(
        title = "Invalid table name", msg,
        easyClose = TRUE, footer = NULL
      ))
      return()
    }
    
    finalCols <- character(0)
    for (i in seq_len(input$ncols)) {
      colNameID <- paste0("colName", i)
      colTypeID <- paste0("colType", i)
      colName <- input[[colNameID]]
      colType <- input[[colTypeID]]
      finalCols[colName] <- colType
    }
    
    if (any(names(finalCols) == "")) {
      showModal(modalDialog(
        title = "Invalid column name",
        "All columns must be named",
        easyClose = TRUE, footer = NULL
      ))
      return()
    }
    
    tryCatch({
      dbCreateTable(pool_2, input$tableName, finalCols)
      showModal(modalDialog(
        title = "Successfully Created", "An Object Description Table is created",
        easyClose = TRUE, footer = NULL
      ))
      reset("tableName")
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        paste("Failed to create table:", e$message),
        easyClose = TRUE, footer = NULL
      ))
    })
  })
  
  observeEvent(input$create_master, {
    tryCatch({
      df <- data.frame(id = character(),
                       input = character(),
                       transformation = character(),
                       output = character(),
                       stringsAsFactors = FALSE)
      
      dbWriteTable(pool, input$masterName, df)
      showModal(modalDialog(
        title = "Successfully Created", "A Lineage Table is created",
        easyClose = TRUE, footer = NULL
      ))
      reset("masterName")
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        paste("Failed to create master table:", e$message),
        easyClose = TRUE, footer = NULL
      ))
    })
  })
}