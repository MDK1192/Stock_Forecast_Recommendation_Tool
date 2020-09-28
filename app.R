#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
source("modules/module1.R")
source("modules/module2.R")
source("modules/module3.R")


ui <- dashboardPage(
    #Header Content
    dashboardHeader(title = "Stock Recommendation Dashboard"),
    #Sidebar Content
    dashboardSidebar(
        sidebarMenu(
            menuItem("Tab1", tabName = "tab1", icon = icon("th")),
            menuItem("Tab2", tabName = "tab2", icon = icon("th")),
            menuItem("Tab3", tabName = "tab3", icon = icon("th")),
            menuItem("Tab4", tabName = "tab4", icon = icon("th"))
        )
    ),
    #Body Content
    dashboardBody(
        tabItems(
            tabItem(tabName = "tab1",
                    h2("Placeholder")
            ),
            tabItem(tabName = "tab2",
                    h2("Placeholder")
            ),
            tabItem(tabName = "tab3",
                    h2("Placeholder")
            ),
            tabItem(tabName = "tab4",
                    h2("Placeholder")
            )
        )
    )
)

#server/logic content
server <- function(input, output, session) {
   
}

shinyApp(ui, server)