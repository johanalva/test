library(httr)
library(plyr)
library(magrittr)
library(lubridate)
library(trelloR)
library(jsonlite)
library(rpivotTable)
library(dplyr)
library(stringr)
library(plotly)
library(ggplot2)
library(shiny)
library(rsconnect)
library(BH)
library(DT)
library(yaml)
library(stringi)
#install.packages(c("trelloR", "httr", "jsonlite"))

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    observeEvent(input$do, {
        urlT <- renderText({
            paste0(input$TrelloURL)
        })
        
        c4 <- renderTable(
            c2
        )
        
        ae <- urlT()
        apic <- oauth_app("trello", input$key, input$secret)
        
        rq <- "https://trello.com/1/OAuthGetRequestToken"
        au <- "https://trello.com/1/OAuthAuthorizeToken"
        ac <- "https://trello.com/1/OAuthGetAccessToken"
        otep <- oauth_endpoint(rq, au, ac, ae)
        options(httr_oauth_cache=FALSE)
        my_token <- oauth1.0_token(otep, apic)
        ae <- paste0(ae, ".json")
        req <- httr::GET(ae, my_token, paging = TRUE)
        json <- httr::content(req, paging = TRUE)
        ibd2 <- get_id_board(ae, my_token)
        
        #--------------------------
        cards <- sapply(json$cards, function(x) c(x$id,x$name,x$dateLastActivity,
                                                  x$idList, x$closed, x$desc))
        #class(cards)
        cards <- data.frame(t(cards), stringsAsFactors=FALSE)
        names(cards) <- c('idCard','cardName','dateLastActivity','idList','closed',
                          'description')
        
        cards$dateLastActivity <- as.POSIXct(strptime(paste0(cards$dateLastActivity), 
                                                      tz = "UTC", "%Y-%m-%dT%H:%M:%OSZ"))-(6*3600)
        
        # Cycle Time Calculation ----------------------------------------
        cards$creationDate <- as.POSIXct(strtoi(paste0('0x',substr(cards$idCard,1,8))), 
                                         origin="1970-01-01")
        
        # Important: pass dates to same metric unit to create the cycle time
        
        cards$creationDate <- as.character(cards$creationDate)
        cards$dateLastActivity <- as.character(cards$dateLastActivity)
        
        #Cycle time calcualation
        cards$cycleTime <- round(difftime(cards$dateLastActivity, cards$creationDate, 
                                          units = input$varTime), digits = 2)
        
        cards$WeekEnding <- week(cards$dateLastActivity)
        cards$Year <- year(cards$dateLastActivity)
        
        list <- sapply(json$lists, function(x) c(x$id,x$name))
        listdf <- as.data.frame(t(list))
        names(listdf) <- c("idList","Listname")
        c1 <- merge(listdf,cards, by = "idList")
        c1 <- c1[,-1]
        
        
        membersList <- get_board_members(ibd2,my_token)
        membersList <- as.data.frame(membersList)
        
        members <- sapply(json$cards, function(x) c(x$id, x$idMembers[1:5]))
        members <- data.frame(t(members), stringsAsFactors = FALSE)
        names(members) <- c('idCard','Assignee1', 'Assignee2','Assignee3','Assignee4',
                            'Assignee5')
        
        membersAssignation <- function(x,b){
            if(!is.null(x[[b]])){
                j <- intersect(x[[b]], membersList$id)
                k <- filter(membersList, id==j)
                k$fullName
            }else{
                "-"
            }
        }
        
        membersTest <- members
        
        for(i in 1:nrow(members)){
            membersTest$Assignee1[[i]] <- membersAssignation(members$Assignee1,i)
            membersTest$Assignee2[[i]] <- membersAssignation(members$Assignee2,i)
            membersTest$Assignee3[[i]] <- membersAssignation(members$Assignee3,i)
            membersTest$Assignee4[[i]] <- membersAssignation(members$Assignee4,i)
            membersTest$Assignee5[[i]] <- membersAssignation(members$Assignee5,i)
        }
        
        c2 <- merge(c1,membersTest, by = "idCard")
        
        c2$Assignee1 <- as.character(c2$Assignee1)
        c2$Assignee2 <- as.character(c2$Assignee2)
        c2$Assignee3 <- as.character(c2$Assignee3)
        c2$Assignee4 <- as.character(c2$Assignee4)
        c2$Assignee5 <- as.character(c2$Assignee5)
        
        output$trUrl <- urlT
        output$c4 <- c4
        c5 <<- c2
        
        c5 <<- c5[order(c5$Year, c5$WeekEnding),]
        
        ## Graphics:
        c6 <- aggregate(cycleTime ~ WeekEnding + Year + closed, c5, FUN = mean)
        c7 <- aggregate(cycleTime ~ WeekEnding + Year + closed + Assignee1,
                        c5, FUN = mean)
        
        totCardsC5 <- count_(c5, c("Year", "WeekEnding", "closed"))
        totAssignee1 <- unique(select(c5, Year, WeekEnding, closed, Assignee1))
        totAssignee1 <- count_(totAssignee1, 
                               c("Year", "WeekEnding", "closed"))
        
        output$choose_year <- renderUI({
            selectInput("dataset", "Year", as.list(c5$Year))
        })
        
        output$choose_week <- renderUI({
            selectInput("weekSel", "Week Ending for Dinamic Table", 
                        as.list(subset(c5,Year == input$dataset)$WeekEnding),
                        multiple = FALSE)
        })
        
        
        ##-----------------
        tp <- select(c2, closed, cycleTime, WeekEnding, Year, Assignee1)
        tp <- filter(c2, closed == TRUE)
        tp1 <- aggregate(cycleTime ~ WeekEnding + Year, tp, FUN = mean)
        totCards <- count_(tp, c("Year","WeekEnding"))
        names(totCards) <- c("Year", "WeekEnding", "TotalCards_Throughput")
        totuniqueSelect <- select(tp, Assignee1, WeekEnding, Year)
        totunique <- (unique(totuniqueSelect))
        totuniquet <- count_(totunique, c('WeekEnding', 'Year'))
        names(totuniquet) <- c('WeekEnding', 'Year', 'TotalAssignee')
        throguhput <- merge(tp1, totCards, by = c("Year", "WeekEnding"))
        throguhput <- merge(throguhput, totuniquet, by = c("Year", "WeekEnding"))
        throguhput$cycleTime <- as.numeric(throguhput$cycleTime)
        throguhput <- mutate(throguhput, Delivery_Rate = 
                                 round((TotalCards_Throughput / cycleTime), digits = 2))
        
        
        throguhput <- throguhput[order(throguhput$Year, throguhput$WeekEnding),]
        throguhput1 <<- throguhput
        
        output$pivote <- renderRpivotTable({
            rpivotTable(data = c5, width = 20, height = 300, rows = 5)
        })  
        
        output$ct <- renderPlotly({
            p1 <- plot_ly(data = filter(throguhput1, Year==input$dataset), x = ~WeekEnding, y = ~cycleTime, 
                          name = 'Cycle Time', line = list(shape = 'spline'))%>%
                layout(title = "Agile Metrics - Cycle time per week")
            return(p1)
        })
        
        output$tp <- renderPlotly({
            p2 <- plot_ly(data = filter(throguhput1, Year==input$dataset), x = ~WeekEnding, y = ~Delivery_Rate,
                          name = 'Cycle Time', line = list(shape = 'spline')) %>%
                layout(title = "Agile Metrics - Delivery Rate per week")
            return(p2)
        })
        
        output$pt3 <- renderPlotly({
            p3 <- plot_ly(data = filter(c6, closed == input$closed, 
                                        Year == input$dataset), 
                          x = ~WeekEnding, y = ~cycleTime,
                          name = 'Cycle Time', line = list(shape = 'spline')) %>%
                layout(title = "Cycle time in average week")
            return(p3)
        })
        
        output$pt4 <- renderPlotly({
            p4 <- plot_ly(data = filter(totCardsC5, closed == input$closed, 
                                        Year == input$dataset), 
                          x = ~WeekEnding, y = ~n,
                          name = 'Total Cards', type = "bar") %>%
                layout(title = "Cards volume per week - (Throughput)",
                       yaxis = list(title = "Volume"))
            return(p4)
        })
        
        output$pt5 <- renderPlotly({
            p5 <- plot_ly(data = filter(totAssignee1, closed == input$closed,
                                        Year == input$dataset),
                          x = ~WeekEnding, y = ~n,
                          name = 'Total Cards', type = "bar") %>%
                layout(title = "Total specialist assignee per week",
                       yaxis = list(title = "Volume"))
            return(p5)
        })
        
        
        output$pt6 <- renderPlotly({
            p6 <- plot_ly(data = filter(c7, closed == input$closed, 
                                        Year == input$dataset), 
                          x = ~WeekEnding, y = ~cycleTime,
                          name = 'Cycle Time', line = list(shape = 'spline'),
                          color = ~Assignee1) %>%
                layout(title = "Cycle time in average week")
        })
        
        output$c4 <-renderDataTable({
            
            dtc5 <- filter(c5, Year == input$dataset, WeekEnding == input$weekSel,
                           closed == input$closed)
            datatable(dtc5, filter = 'bottom', options = list(pageLenght = 5, 
                                                              autoWidth = TRUE,
                                                              scrollx = TRUE))
        })
        
    })
    output$downloadData <- downloadHandler(
        filename = function(){paste(input$fileNameF,'.csv')},
        content = function(file){
            write.csv(c5, file)
        }
    )
    
    output$downloadTP <- downloadHandler(
        filename = function(){paste(input$fileNameF,'-AgileMetric','.csv')},
        content = function(file){
            write.csv(throguhput1, file)
        }
    )
    
    #close the R session when Chrome closes
    # session$onSessionEnded(function() {
    #     stopApp()
    #     q("no")
    # })
    # 
})

#  ------------------------------------------------------------------------


