library(shiny)
library(shinyjs)
library(shinyEffects)
library(shinydashboard)
library(dplyr)
library(dbplyr)
library(tibble)
library(pool)
library(rlang)
library(DBI)
library(lubridate) #For future use
library(DiagrammeR)

source("modules/overview.R", local = TRUE)
source("modules/createTable.R", local = TRUE)
source("modules/createEntry.R", local = TRUE)
source("modules/update.R", local = TRUE)
source("modules/delete.R", local = TRUE)

pool <- dbPool(RSQLite::SQLite(), dbname = "db.sqlite")
pool_2 <- dbPool(RSQLite::SQLite(), dbname = "db2.sqlite")

tbls <- reactiveFileReader(500, NULL, "db.sqlite",
                           function(x) dbListTables(pool)
                           )
tbls_2 <- reactiveFileReader(500, NULL, "db2.sqlite",
                             function(x) dbListTables(pool_2)
                             )
onStop(function(){
  poolClose(pool)
})
onStop(function(){
  poolClose(pool_2)
})

ui <- dashboardPage(
  dashboardHeader(title = "TEST"),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
      menuItem("Overview", tabName = "overview"),
      menuItem("Create table", tabName = "createTable"),
      menuItem("Create entry", tabName = "createEntry"),
      menuItem("Update entry", tabName = "update"),
      menuItem("Delete", tabName = "delete")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("overview", overviewUI("overview-module")),
      tabItem("createTable", createTableUI("createTable-module")),
      tabItem("createEntry", createEntryUI("createEntry-module")),
      tabItem("update", updateUI("update-module")),
      tabItem("delete", deleteUI("delete-module"))
    )
  )
)

server <- function(input, output, session) {
  reqTable <- function(tableName) {
    tbls()
    req(tableName)
    req(tableName %in% dbListTables(pool))
  }
  
  reqColInTable <- function(tableName, colName) {
    reqTable(tableName)
    req(colName)
    req(colName %in% dbListFields(pool, tableName))
  }
  
  reqTable_2 <- function(tableName) {
    tbls_2()
    req(tableName)
    req(tableName %in% dbListTables(pool_2))
  }
  
  reqColInTable_2 <- function(tableName, colName) {
    reqTable_2(tableName)
    req(colName)
    req(colName %in% dbListFields(pool_2, tableName))
  }
  
  callModule(overview, "overview-module", pool, pool_2, reqTable, reqColInTable, reqTable_2, reqColInTable_2)
  callModule(createTable, "createTable-module", pool, pool_2)
  callModule(createEntry, "createEntry-module", pool, pool_2, reqTable, reqColInTable_2)
  callModule(update, "update-module", pool, pool_2, reqTable, reqTable_2, reqColInTable, reqColInTable_2)
  callModule(delete, "delete-module", pool, pool_2, reqTable, reqTable_2, reqColInTable, reqColInTable_2)
}

shinyApp(ui, server)
