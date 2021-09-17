
createEntryUI <- function(id, tables) {
  ns <- NS(id)
  tagList(
    selectInput(ns("tableName"), "Choose a table", character(0)),
    box(title = "Current entry", status = "primary", 
        solidHeader = TRUE, width = 12,
        style = "overflow-y:scroll; max-height: 300px; position:relative; align: centre;overflow-x:scroll; max-width: 300;",
        tableOutput(ns("current"))),
    uiOutput(ns("fields")),
    actionButton(ns("create"), "Create entry", class = "pull-right btn-info")
  )
}

createEntry <- function(input, output, session, pool, reqTable, goHome) {
  
  observeEvent(tbls(), {
    updateSelectInput(session, "tableName", choices = tbls())
  })
  
  output$current <- renderTable({
    req(input$tableName)
    query <- sqlInterpolate(pool, 
                            "SELECT * FROM ?table",
                            .dots =list(table = input$tableName)
    )
    dbGetQuery(pool, query)
    pool %>% tbl(input$tableName)
  })
  
  fields <- reactive({
    req(input$tableName)
    pool %>% tbl(input$tableName) %>% head %>% collect %>% 
      lapply(type_sum) %>% unlist
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
  
  observeEvent(input$create, {
    entryValues <- data.frame(stringsAsFactors = FALSE,
      lapply(fields(), type.convert, as.is = FALSE)
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

    dbAppendTable(pool, input$tableName, entryValues)
    goHome()
  })
  
}
