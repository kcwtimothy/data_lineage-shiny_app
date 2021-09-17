
updateUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          selectInput(ns("tableName"), "Choose a table", character(0)),
          checkboxGroupInput(ns("col"), "Choose columns to update", ""),
          selectInput(ns("val"), "Select ID of entry for filter", NULL)
        ),
        mainPanel(
           box(title = "Current entry", status = "primary", 
               solidHeader = TRUE, width = 12,
               style = "overflow-y:scroll; max-height: 300px; position:relative; align: centre;overflow-x:scroll; max-width: 300;",
               tableOutput(ns("current"))),
           fluidRow(
              box(title = "New proposed entry", status = "success", 
               solidHeader = TRUE, width = 12,
               style = "overflow-y:scroll; max-height: 300px; position:relative; align: centre;overflow-x:scroll; max-width: 300;",
               uiOutput(ns("fields")),
               actionButton(ns("update"), "Update entry", 
                            class = "pull-right btn-info"))
              )
           )
        )
      )
    )
}

update <- function(input, output, session, pool, reqTable, reqColInTable) {
  
  observeEvent(tbls(), {
    updateSelectInput(session, "tableName", choices = tbls())
  })
  
  fields <- reactive({
    reqTable(input$tableName)
    pool %>% tbl(input$tableName) %>% select(input$col) %>% 
      head %>% collect %>% lapply(type_sum) %>% unlist
  })
  
  observe({
    reqTable(input$tableName)
    cols <- dbListFields(pool, input$tableName)
    updateCheckboxGroupInput(session, "col", choices = cols, inline = TRUE)
  })
  
  observe({
    reqColInTable(input$tableName, input$col)
    query <- sqlInterpolate(pool, 
                            "SELECT COUNT(*) FROM ?table",
                            .dots = list(table = input$tableName)
    )
    req(dbGetQuery(pool, query) > 0)
    
    df <- as_data_frame(pool %>% tbl(input$tableName))
    col_id <- dbListFields(pool, input$tableName)[1]
    allUniqueVals <- df[[col_id]]
    updateSelectInput(session, "val", choices = allUniqueVals)
  })
  
  output$current <- renderTable({
    reqColInTable(input$tableName, input$col)
    req(input$val)
    col_1 <- dbListFields(pool, input$tableName)[1]
    query <- sqlInterpolate(pool, 
                            "SELECT * FROM ?table WHERE ?col = ?val",
                            .dots =list(table = input$tableName,
                                        val = input$val,
                                        col = sym(col_1))
    )
    dbGetQuery(pool, query)
  })
  
  output$fields <- renderUI({
    fieldNames <- names(fields())
    fieldTypes <- unname(fields())
    selections <- vector("list", length(fieldNames))
    for (i in seq_len(length(fieldNames))) {
      nm <- fieldNames[i]
      id <- paste0("field", nm)
      selections[[i]] <- box(width = 3,
        switch(fieldTypes[i],
          int = numericInput(session$ns(id), nm, NULL),
          chr = textInput(session$ns(id), nm, NULL)
        )
      )
    }
    selections
  })
  
  observeEvent(input$update, {
    entryValues <- data.frame(stringsAsFactors = FALSE,
      lapply(fields(), type.convert)
    )
    
    for (name in names(entryValues)) {
      id <- paste0("field", name)
      
      if (!isTruthy(input[[id]])) {
        showModal(modalDialog(
          title = "NULL value",
          "NULL values are not allowed for new entries",
          easyClose = TRUE, footer = NULL
        ))
        return()
      }
      
      entryValues[name] <- input[[id]]
    }
    
    col <- if (input$col %in% dbListFields(pool, input$tableName)) {
      input$col
    } else {
       showModal(modalDialog(
          title = "Invalid ID column name",
          "The ID column must be a column of the DB table",
          easyClose = TRUE, footer = NULL
        ))
        return()
    }
    
    sql <- paste0("UPDATE ?table SET ", 
      paste0(names(entryValues), " = ?", names(entryValues), collapse = ", "), 
      " WHERE ", col, " = ?idVal;")
    
    query <- sqlInterpolate(pool, sql, .dots = c(
      list(table = input$tableName), 
      as_list(entryValues),
      list(idVal = input$val)
    ))

    dbExecute(pool, query)
  })
}
