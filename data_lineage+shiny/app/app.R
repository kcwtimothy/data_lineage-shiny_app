library(shiny)
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
source("modules/editRelation.R", local = TRUE)

pool <- dbPool(RSQLite::SQLite(), dbname = "db.sqlite")
#conn <- DBI::dbConnect(RSQLite::SQLite(), dbname = "db.sqlite")

tbls <- reactiveFileReader(500, NULL, "db.sqlite",
                           function(x) dbListTables(pool)
                           )

onStop(function(){
  poolClose(pool)
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
      #menuItem("Edit Relation", tabName = "editRelation")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("overview", overviewUI("overview-module")),
      tabItem("createTable", createTableUI("createTable-module")),
      tabItem("createEntry", createEntryUI("createEntry-module")),
      tabItem("update", updateUI("update-module")),
      tabItem("delete", deleteUI("delete-module"))
      #tabItem("editRelation", editRelationUI("editRelation-module"))
    )
  )
)

server <- function(input, output, session) {
  goHome <- function() updateTabItems(session, "tabs", "overview")
  
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
  
  callModule(overview, "overview-module", pool, reqTable, reqColInTable)
  callModule(createTable, "createTable-module", pool, goHome)
  callModule(createEntry, "createEntry-module", pool, reqTable, goHome)
  callModule(update, "update-module", pool, reqTable, reqColInTable)
  callModule(delete, "delete-module", pool, reqTable, reqColInTable)
  #callModule(editRelation, "editRelation-module", pool, conn)
}

shinyApp(ui, server)
