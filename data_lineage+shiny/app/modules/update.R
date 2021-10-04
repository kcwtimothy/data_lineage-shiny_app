
updateUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          selectInput(ns("tableName"), "Choose a Lineage Table", character(0)),
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
        ), sidebarLayout(
          sidebarPanel(
            selectInput(ns("tableName_2"), "Choose a table", character(0)),
            checkboxGroupInput(ns("col_2"), "Choose columns to update", ""),
            selectInput(ns("val_2"), "Select ID of entry for filter", NULL)
          ),
          mainPanel(
            box(title = "Current entry", status = "primary", 
                solidHeader = TRUE, width = 12,
                style = "overflow-y:scroll; max-height: 300px; position:relative; align: centre;overflow-x:scroll; max-width: 300;",
                tableOutput(ns("current_2"))),
            fluidRow(
              box(title = "New proposed entry", status = "success", 
                  solidHeader = TRUE, width = 12,
                  style = "overflow-y:scroll; max-height: 300px; position:relative; align: centre;overflow-x:scroll; max-width: 300;",
                  uiOutput(ns("fields_2")),
                  actionButton(ns("update_2"), "Update entry", 
                               class = "pull-right btn-info"))
            )
          )
        )
      )
    )
}

update <- function(input, output, session, pool, pool_2, reqTable, reqTable_2, reqColInTable, reqColInTable_2) {
  
  observeEvent(tbls(), {
    updateSelectInput(session, "tableName", choices = tbls())
  })
  
  observeEvent(tbls_2(), {
    updateSelectInput(session, "tableName_2", choices = tbls_2())
  })
  
  fields <- reactive({
    reqTable(input$tableName)
    pool %>% tbl(input$tableName) %>% select(input$col) %>% 
      head %>% collect %>% lapply(type_sum) %>% unlist
  })
  
  fields_2 <- reactive({
    reqTable_2(input$tableName_2)
    pool_2 %>% tbl(input$tableName_2) %>% select(input$col_2) %>% 
      head %>% collect %>% lapply(type_sum) %>% unlist
  })
  
  observe({
    reqTable(input$tableName)
    cols <- dbListFields(pool, input$tableName)
    updateCheckboxGroupInput(session, "col", choices = cols, inline = TRUE)
  })
  
  observe({
    reqTable_2(input$tableName_2)
    cols <- dbListFields(pool_2, input$tableName_2)
    updateCheckboxGroupInput(session, "col_2", choices = cols, inline = TRUE)
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
  
  observe({
    reqColInTable_2(input$tableName_2, input$col_2)
    query <- sqlInterpolate(pool_2, 
                            "SELECT COUNT(*) FROM ?table",
                            .dots = list(table = input$tableName_2)
    )
    req(dbGetQuery(pool_2, query) > 0)
    
    df <- as_data_frame(pool_2 %>% tbl(input$tableName_2))
    col_id <- dbListFields(pool_2, input$tableName_2)[1]
    allUniqueVals <- df[[col_id]]
    updateSelectInput(session, "val_2", choices = allUniqueVals)
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
  
  output$current_2 <- renderTable({
    reqColInTable_2(input$tableName_2, input$col_2)
    req(input$val_2)
    col_1 <- dbListFields(pool_2, input$tableName_2)[1]
    query <- sqlInterpolate(pool_2, 
                            "SELECT * FROM ?table WHERE ?col = ?val",
                            .dots =list(table = input$tableName_2,
                                        val = input$val_2,
                                        col = sym(col_1))
    )
    dbGetQuery(pool_2, query)
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
    col_1 <- dbListFields(pool, input$tableName)[1]
    
    sql <- paste0("UPDATE ?table SET ", 
      paste0(names(entryValues), " = ?", names(entryValues), collapse = ", "), 
      " WHERE ", sym(col_1), " = ?idVal;")
    
    query <- sqlInterpolate(pool, sql, .dots = c(
      list(table = input$tableName), 
      as_list(entryValues),
      list(idVal = input$val)
    ))

    dbExecute(pool, query)
  })
  
  observeEvent(input$update_2, {
    entryValues_2 <- data.frame(stringsAsFactors = FALSE,
                              lapply(fields_2(), type.convert)
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

    col_1 <- dbListFields(pool_2, input$tableName_2)[1]
    
    sql <- paste0("UPDATE ?table SET ", 
                  paste0(names(entryValues_2), " = ?", names(entryValues_2), collapse = ", "), 
                  " WHERE ", sym(col_1), " = ?idVal;")
    
    query <- sqlInterpolate(pool, sql, .dots = c(
      list(table = input$tableName_2), 
      as_list(entryValues_2),
      list(idVal = input$val_2)
    ))
    
    dbExecute(pool_2, query)
  })
}
