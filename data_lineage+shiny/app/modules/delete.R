get_table_fields <- function(pool, table_name) {
  con <- pool::poolCheckout(pool)
  on.exit(pool::poolReturn(con))
  
  fields <- DBI::dbListFields(con, table_name)
  return(fields)
}

deleteUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          selectInput(ns("tableName"), "Choose a lineage table", character(0)),

          selectInput(ns("col"), "Choose column to filter on", NULL),
          selectInput(ns("vals"), "Choose values to filter on (will remove whole row)", NULL)
        ),
        mainPanel(
          box(title = "Selected rows (to be deleted)", status = "warning", 
              solidHeader = TRUE, width = 12,
              style = "overflow-y:scroll; max-height: 300px; position:relative; align: centre;overflow-x:scroll; max-width: 300;",
              tableOutput(ns("current")),
              actionButton(ns("deleteRows"), 
                           "Remove selected rows", class = "pull-right btn-warning",
                           style = "material-flat",
                           color = "danger")),
          actionButton(ns("deleteTable"), "Remove whole table", 
                       class = "pull-right btn-danger",
                       style = "material-flat",
                       color = "danger")
          )
        ),
      sidebarLayout(
        sidebarPanel(
          selectInput(ns("tableName_2"), "Choose a table", character(0)),
          
          selectInput(ns("col_2"), "Choose column to filter on", NULL),
          selectInput(ns("vals_2"), "Choose values to filter on (will remove whole row)", NULL)
        ),
        mainPanel(
          box(title = "Selected rows (to be deleted)", status = "warning", 
              solidHeader = TRUE, width = 12,
              style = "overflow-y:scroll; max-height: 300px; position:relative; align: centre;overflow-x:scroll; max-width: 300;",
              tableOutput(ns("current_2")),
              actionButton(ns("deleteRows_2"), 
                           "Remove selected rows", class = "pull-right btn-warning",
                           style = "material-flat",
                           color = "danger")),
          actionButton(ns("deleteTable_2"), "Remove whole table", 
                       class = "pull-right btn-danger",
                       style = "material-flat",
                       color = "danger")
          )
        )
      )
    )
}

delete <- function(input, output, session, pool, pool_2, reqTable, reqTable_2, reqColInTable, reqColInTable_2, get_table_fields) {
  
  observeEvent(pool::dbListTables(pool), {
    updateSelectInput(session, "tableName", choices = pool::dbListTables(pool))
  })
  
  observeEvent(pool::dbListTables(pool_2), {
    updateSelectInput(session, "tableName_2", choices = pool::dbListTables(pool_2))
  })
  
  observe({
    reqTable(input$tableName)
    cols <- get_table_fields(pool, input$tableName)
    updateSelectInput(session, "col", choices = cols)
  })
  
  observe({
    reqTable_2(input$tableName_2)
    cols <- get_table_fields(pool_2, input$tableName_2)
    updateSelectInput(session, "col_2", choices = cols)
  })
  
  observe({
    reqColInTable(input$tableName, input$col)
    query <- sqlInterpolate(pool, 
                            "SELECT COUNT(*) FROM ?table",
                            .dots = list(table = input$tableName)
    )
    req(dbGetQuery(pool, query) > 0)
    
    sql <- "SELECT ?col FROM ?table"
    query <- sqlInterpolate(pool, sql, .dots = c(
      list(table = input$tableName,
           col = sym(input$col))
    ))
    df <- dbGetQuery(pool, query)
    allUniqueVals <- unique(df[[input$col]])
    updateSelectInput(session, "vals", choices = allUniqueVals)
  })
  
  observe({
    reqColInTable_2(input$tableName_2, input$col_2)
    query <- sqlInterpolate(pool_2, 
                            "SELECT COUNT(*) FROM ?table",
                            .dots = list(table = input$tableName_2)
    )
    req(dbGetQuery(pool_2, query) > 0)
    
    sql <- "SELECT ?col FROM ?table"
    query <- sqlInterpolate(pool_2, sql, .dots = c(
      list(table = input$tableName_2,
           col = sym(input$col_2))
    ))
    df <- dbGetQuery(pool_2, query)
    allUniqueVals <- unique(df[[input$col_2]])
    updateSelectInput(session, "vals_2", choices = allUniqueVals)
  })
  
  output$current <- renderTable({
    reqTable(input$tableName)
    req(input$col)
    req(input$vals)
    
    sql <- "SELECT * FROM ?table WHERE ?col IN (?vals)"
    vals <- lapply(input$vals, function(x) dbQuoteString(pool, x))
    query <- sqlInterpolate(pool, sql, .dots = c(
      list(table = input$tableName,
           col = sym(input$col),
           vals = sym(toString(vals)))
    ))
    dbGetQuery(pool, query)
  })
  
  output$current_2 <- renderTable({
    reqTable_2(input$tableName_2)
    req(input$col_2)
    req(input$vals_2)
    
    sql <- "SELECT * FROM ?table WHERE ?col IN (?vals)"
    vals <- lapply(input$vals_2, function(x) dbQuoteString(pool_2, x))
    query <- sqlInterpolate(pool_2, sql, .dots = c(
      list(table = input$tableName_2,
           col = sym(input$col_2),
           vals = sym(toString(vals)))
    ))
    dbGetQuery(pool_2, query)
  })
  
  deleteTableModal <- function() {
    ns <- session$ns
    modalDialog(
      "Do you wish to proceed and delete the entire table?",
      title = "WARNING", 
      footer = tagList(actionButton(ns("confirm"), "Confirm"),
                       modalButton("No")))
  }
  
  deleteTableModal_2 <- function() {
    ns <- session$ns
    modalDialog(
      "Do you wish to proceed and delete the entire table?",
      title = "WARNING", 
      footer = tagList(actionButton(ns("confirm_2"), "Confirm"),
                       modalButton("No")))
  }
  
  observeEvent(input$deleteTable, {
    showModal(deleteTableModal())
  })
  
  observeEvent(input$deleteTable_2, {
    showModal(deleteTableModal_2())
  })
  
  observeEvent(input$confirm, {
    removeModal()
    dbRemoveTable(pool, input$tableName)
  })
  
  observeEvent(input$confirm_2, {
    removeModal()
    dbRemoveTable(pool_2, input$tableName_2)
  })
  
  deleteRowModal <- function() {
    ns <- session$ns
    modalDialog(
      "Confirm to delete this row?",
      title = "Confirmation",
      footer = tagList(actionButton(ns("yes"), "Yes"),
                       modalButton("No"))
    )
  }
  
  deleteRowModal_2 <- function() {
    ns <- session$ns
    modalDialog(
      "Confirm to delete this row?",
      title = "Confirmation",
      footer = tagList(actionButton(ns("yes_2"), "Yes"),
                       modalButton("No"))
    )
  }
  
  observeEvent(input$deleteRows, {
    showModal(deleteRowModal())
  })
  
  observeEvent(input$deleteRows_2, {
    showModal(deleteRowModal_2())
  })
  
  observeEvent(input$yes, {
    removeModal()
    sql <- "DELETE FROM ?table WHERE ?col IN (?vals)"
    vals <- lapply(input$vals, function(x) dbQuoteString(pool, x))
    query <- sqlInterpolate(pool, sql, .dots = c(
      list(table = input$tableName,
           col = sym(input$col),
           vals = sym(toString(vals)))))
                            
    dbExecute(pool, query)
  })
  
  observeEvent(input$yes_2, {
    removeModal()
    sql <- "DELETE FROM ?table WHERE ?col IN (?vals)"
    vals <- lapply(input$vals_2, function(x) dbQuoteString(pool, x))
    query <- sqlInterpolate(pool_2, sql, .dots = c(
      list(table = input$tableName_2,
           col = sym(input$col_2),
           vals = sym(toString(vals)))))
    
    dbExecute(pool_2, query)
  })
}
