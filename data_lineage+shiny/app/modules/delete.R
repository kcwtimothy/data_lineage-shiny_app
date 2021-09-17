
deleteUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          selectInput(ns("tableName"), "Choose a table", character(0)),

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
        )
      )
    )
}

delete <- function(input, output, session, pool, reqTable, reqColInTable) {
  
  observeEvent(tbls(), {
    updateSelectInput(session, "tableName", choices = tbls())
  })
  
  observe({
    reqTable(input$tableName)
    cols <- db_query_fields(pool, input$tableName)
    updateSelectInput(session, "col", choices = cols)
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
  
  deleteTableModal <- function() {
    ns <- session$ns
    modalDialog(
      "Do you wish to proceed and delete the entire table?",
      title = "WARNING", 
      footer = tagList(actionButton(ns("confirm"), "Confirm"),
                       modalButton("No")))
  }
  
  observeEvent(input$deleteTable, {
    showModal(deleteTableModal())
  })
  
  observeEvent(input$confirm, {
    removeModal()
    dbRemoveTable(pool, input$tableName)
  })
  
  deleteRowModal <- function() {
    ns <- session$ns
    modalDialog(
      "Confirm to delete this row?",
      title = "Confirmation",
      footer = tagList(actionButton(ns("yes"), "yes"),
                       modalButton("No"))
    )
  }
  
  observeEvent(input$deleteRows, {
    showModal(deleteRowModal())
  })
  
  observeEvent(input$yes, {
    removeModal()
    col <- if (input$col %in% dbListFields(pool, input$tableName)) {
      input$col
    } else {
      showModal(modalDialog(
        title = "Invalid column name",
        "The selected column must be a column of the DB table",
        easyClose = TRUE, footer = NULL
      ))
      return()
    }
    
    df <- as_data_frame(pool %>% tbl(input$tableName) %>% select(col))
    allUniqueVals <- unique(df[[col]])
    results <- lapply(as_list(input$vals), `%in%`, allUniqueVals)
    
    vals <- if (all(results)) {
      input$vals
    } else {
      showModal(modalDialog(
        title = "Invalid column values",
        "The selected values do not exist in the selected table column",
        easyClose = TRUE, footer = NULL
      ))
      return()
    }
    
    sql <- paste0("DELETE FROM ?table WHERE ", col, " IN (",
                  paste0(vals, collapse = ", "), ");")
    
    query <- sqlInterpolate(pool, sql, table = input$tableName)
    
    dbExecute(pool, query)
  })
}
