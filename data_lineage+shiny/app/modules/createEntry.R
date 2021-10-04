
createEntryUI <- function(id, tables) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    selectInput(ns("tableName"), "Choose a Lineage table", character(0)),
    box(title = "Current entry", status = "primary", 
        solidHeader = TRUE, width = 12,
        style = "overflow-y:scroll; max-height: 300px; position:relative; align: centre;overflow-x:scroll; max-width: 300;",
        tableOutput(ns("current"))),
    box(width = 12,
       style = "overflow-y:scroll; max-height: 800px; position:relative; align: centre;overflow-x:scroll; max-width: 1200;",
       uiOutput(ns("fields")),
       actionButton(ns("create"), "Create entry", class = "pull-right btn-info")),
    selectInput(ns("tableName_2"), "Choose a table", character(0)),
    box(title = "Current entry", status = "primary", 
        solidHeader = TRUE, width = 12,
        style = "overflow-y:scroll; max-height: 300px; position:relative; align: centre;overflow-x:scroll; max-width: 300;",
        tableOutput(ns("current_2"))),
    box(width = 12,
        style = "overflow-y:scroll; max-height: 800px; position:relative; align: centre;overflow-x:scroll; max-width: 1200;",
        uiOutput(ns("fields_2")),
        actionButton(ns("create_2"), "Create entry", class = "pull-right btn-info"))
  )
}

createEntry <- function(input, output, session, pool, pool_2, reqTable, reqTable_2) {
  
  observeEvent(tbls(), {
    updateSelectInput(session, "tableName", choices = tbls())
  })
  
  observeEvent(tbls_2(), {
    updateSelectInput(session, "tableName_2", choices = tbls_2())
  })
  
  cur_tbl_update <- eventReactive(input$create, {
    req(input$tableName)
    query <- sqlInterpolate(pool, 
                            "SELECT * FROM ?table",
                            .dots =list(table = input$tableName)
    )
    dbGetQuery(pool, query)
  })
  
  output$current <- renderTable({
    if (!input$create){
      req(input$tableName)
      query <- sqlInterpolate(pool, 
                              "SELECT * FROM ?table",
                              .dots =list(table = input$tableName)
      )
      dbGetQuery(pool, query)
    } else {
      cur_tbl_update()
    }
  })
  
  cur_tbl_update2 <- eventReactive(input$create_2, {
    req(input$tableName_2)
    query <- sqlInterpolate(pool_2, 
                            "SELECT * FROM ?table",
                            .dots =list(table = input$tableName_2)
    )
    dbGetQuery(pool_2, query)
  })
  output$current_2 <- renderTable({
    if (!input$create_2){
      req(input$tableName_2)
    query <- sqlInterpolate(pool_2, 
                            "SELECT * FROM ?table",
                            .dots =list(table = input$tableName_2)
    )
    dbGetQuery(pool_2, query)
    } else {
      cur_tbl_update2()
    }
  })
  
  fields <- reactive({
    req(input$tableName)
    pool %>% tbl(input$tableName) %>% head %>% collect %>% 
      lapply(type_sum) %>% unlist
  })
  
  fields_2 <- reactive({
    req(input$tableName_2)
    pool_2 %>% tbl(input$tableName_2) %>% head %>% collect %>% 
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
  
  output$fields_2 <- renderUI({
    fieldNames <- names(fields_2())
    fieldTypes <- unname(fields_2())
    selections <- vector("list", length(fieldNames))
    for (i in seq_len(length(fieldNames))) {
      nm <- fieldNames[i]
      id_2 <- paste0("field_2", nm)
      selections[[i]] <- box(width = 3,
                             switch(fieldTypes[i],
                                    int = numericInput(session$ns(id_2), nm, NULL),
                                    chr = textInput(session$ns(id_2), nm, NULL)
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
      
      if(name == "id"){
        entryValues[name] <- uuid::UUIDgenerate(use.time = TRUE)
      } else  {
        entryValues[name] <- input[[id]]
      }
    }

    dbAppendTable(pool, input$tableName, entryValues)
    reset("fields")
  })
  
  observeEvent(input$create_2, {
    entryValues_2 <- data.frame(stringsAsFactors = FALSE,
                              lapply(fields_2(), type.convert, as.is = FALSE)
    )
    
    for (name in names(entryValues_2)) {
      id_2 <- paste0("field_2", name)
      
      if (!isTruthy(input[[id_2]])) {
        showModal(modalDialog(
          title = "NULL value",
          "NULL values are not allowed for new entries",
          easyClose = TRUE, footer = NULL
        ))
        return()
      }
        entryValues_2[name] <- input[[id_2]]
    }
    
    dbAppendTable(pool_2, input$tableName_2, entryValues_2)
    reset("fields_2")
  })
}
