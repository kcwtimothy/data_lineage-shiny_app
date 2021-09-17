editRelationUI <- function(id) {
  ns <- NS(id)
  tagList(
    numericInput(ns("ntables"), "Number of tables", 2, min = 2),
    textInput(ns("dmName"), "DM Object name"),
    uiOutput(ns("tables")),
    actionButton(ns("create"), "Create Data Model", class = "pull-right btn_info")
  )
}

editRelation <- function(input, output, session, pool, conn){
  
  output$tables <- renderUI({
    input$dmName
    tables <- vector("list", input$ntables)
    for (i in seq_len(input$ntables)) {
      tables[[i]] <- box(
        title = paste("table", i), width = 4, solidHeader = TRUE, status = "primary",
        selectInput(session$ns(paste0("tableName", i)), "Select Table", choices = tbls())
      )
    }
    tables
  })
  
  observeEvent(input$create, {
    for (i in seq(input$ntables)) {
      tableNameID <- paste0("tableName", i)
      tableName <- input[[tableNameID]]
      data_list <- list(as_data_frame(tbl(pool, tableName)))
    }
    dm_obj <- do.call(dm, data_list)
    dm_local <- dm_obj %>% collect()
    deploy <- copy_dm_to(conn, dm_local, table_names = ~paste0("dmName", .x))
  })
}