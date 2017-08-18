library(shiny)
library(plotly)
library(rpivotTable)
library(DT)
library(shinydashboard)


dashboardPage(
    dashboardHeader(title = "IBM - Trello Report"),
    dashboardSidebar(
        textInput("TrelloURL", "Please provide your Trello URL"),
        textInput("key","Please provide your Key"),
        passwordInput("secret", "Please provide your secret"),
        textInput("fileNameF", "Please provide the file name."),
        radioButtons("varTime", "Please select Cycle Time Format:",
                     c("Days" = "days","Hours" = "hours", 
                       "Minutes" = "mins")),
        checkboxInput("closed", "Archived", value = FALSE),
        actionButton("do", "Get Report"),
        uiOutput("choose_year"),
        uiOutput("choose_week"),
        #uiOutput("choose_listName"),
        downloadButton("downloadData", "Download Report"),
        downloadButton("downloadTP", "Download Agile Metrics")
        
    ),
    dashboardBody(
        # Boxes need to be put in a row (or column)
        fluidRow(
            box(plotlyOutput("ct", height = 250)),
            box(plotlyOutput("tp", height = 250)),
            box(plotlyOutput("pt3", height = 250)),
            box(plotlyOutput("pt4", height = 250)),
            box(plotlyOutput("pt5", height = 250)),
            box(plotlyOutput("pt6", height = 250))
        ),

        fluidRow(
        dataTableOutput("c4")
        )
        
    )
)