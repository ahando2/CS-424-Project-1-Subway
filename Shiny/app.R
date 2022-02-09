setwd("D:/ahando2/classes/SPRING 2022/CS 424/Project/Project 1/Shiny")

library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)
library(lubridate)

# assume all of the tsv files in this directory are data of the same kind that I want to visualize
temp = list.files(pattern="*.tsv")
allData2 <- lapply(temp, read.delim)
CTA_daily <- do.call(rbind, allData2)

# format the date
CTA_daily <- CTA_daily[complete.cases(CTA_daily), ]
CTA_daily$date <- ymd(CTA_daily$date)

# convert the station_id, rides, month, day, year from int to numbers
CTA_daily$station_id <- as.numeric(CTA_daily$station_id)
CTA_daily$rides <- as.numeric(CTA_daily$rides)

# Create the menu items to select the different years and the different stations
years<-c(2001:2021)
station_names <- c("UIC-Halsted","O'Hare Airport","Damen/Milwaukee")

# Create the shiny dashboard
ui <- dashboardPage(
  dashboardHeader(title = "CTA Daily Entries"),
  dashboardSidebar(disable = FALSE, collapsed = FALSE,
                   
                   sidebarMenu(
                     menuItem("About", tabName = "about", icon = NULL),
                     menuItem("Analytic Dashboard", tabName = "dashboard", icon = NULL, selected = TRUE)
                     )
                  
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
        fluidRow(
          mainPanel(
            h2(textOutput("Tab1"),style='margin-top: 4px; margin-bottom: 4px;'),
            fluidRow(
              column(2,selectInput("Year1", "Select the year to visualize", years, selected = 2021)),
              column(2,selectInput("StationName1", "Select the station to visualize", station_names, selected = "UIC-Halsted"))),
            tabsetPanel(
              tabPanel("All Year", 
                       (fluidRow(
                         column(8,
                                fluidRow(
                                  box( title = "Entries For Each Year", solidHeader = TRUE, status = "primary", width = 12,
                                       plotOutput("hist0", height = 220)
                                  )
                                )
                         ),
                         column(4,
                                fluidRow(
                                  box(title = "Entries For Each Year as Table", solidHeader = TRUE, status = "primary", width = 12,
                                      dataTableOutput("tab0", height = 220)
                                  )
                                )
                         )
                       ))),
              tabPanel("Each Date", 
                       (fluidRow(
                         column(8,
                                fluidRow(
                                  box( title = "Entries For Each Date", solidHeader = TRUE, status = "primary", width = 12,
                                       plotOutput("hist1", height = 220)
                                  )
                                )
                         ),
                         column(4,
                                fluidRow(
                                  box(title = "Entries For Each Date as Table", solidHeader = TRUE, status = "primary", width = 12,
                                      dataTableOutput("tab1", height = 220)
                                  )
                                )
                         )
                       ))),
              tabPanel("Each Month", 
                       (fluidRow(
                         column(8,
                                fluidRow(
                                  box( title = "Entries For Each Month", solidHeader = TRUE, status = "primary", width = 12,
                                       plotOutput("hist2", height = 220)
                                  )
                                )
                         ),
                         column(4,
                                fluidRow(
                                  box(title = "Entries For Each Month as Table", solidHeader = TRUE, status = "primary", width = 12,
                                      dataTableOutput("tab2", height = 220)
                                  )
                                )
                         )
                       ))),
              tabPanel("Each Day", 
                       (fluidRow(
                         column(8,
                                fluidRow(
                                  box( title = "Entries For Each Day", solidHeader = TRUE, status = "primary", width = 12,
                                       plotOutput("hist3", height = 220)
                                  )
                                )
                         ),
                         column(4,
                                fluidRow(
                                  box(title = "Entries For Each Day as Table", solidHeader = TRUE, status = "primary", width = 12,
                                      dataTableOutput("tab3", height = 220)
                                  )
                                )
                         )
                       ))),
            ),style='width: 100%;margin-bottom: -10px;'),
          mainPanel(
            hr(style = "border-top:1.5px solid #999;margin-top: 0px;margin-bottom: 8px;"),
            h2(textOutput("Tab2"),style='margin-top: 4px;margin-bottom: 4px;'),
            fluidRow(
              column(2,selectInput("Year2", "Select the year to visualize", years, selected = 2021)),
              column(2,selectInput("StationName2", "Select the station to visualize", station_names, selected = "O'Hare Airport"))),
            tabsetPanel(
              tabPanel("All Year", 
                       (fluidRow(
                         column(8,
                                fluidRow(
                                  box( title = "Entries For Each Year", solidHeader = TRUE, status = "primary", width = 12,
                                       plotOutput("hist4", height = 220)
                                  )
                                )
                         ),
                         column(4,
                                fluidRow(
                                  box(title = "Entries For Each Date as Table", solidHeader = TRUE, status = "primary", width = 12,
                                      dataTableOutput("tab4", height = 220)
                                  )
                                )
                         )
                       ))),
              tabPanel("Each Date", 
                       (fluidRow(
                         column(8,
                                fluidRow(
                                  box( title = "Entries For Each Date", solidHeader = TRUE, status = "primary", width = 12,
                                       plotOutput("hist5", height = 220)
                                  )
                                )
                         ),
                         column(4,
                                fluidRow(
                                  box(title = "Entries For Each Date as Table", solidHeader = TRUE, status = "primary", width = 12,
                                      dataTableOutput("tab5", height = 220)
                                  )
                                )
                         )
                       ))),
              tabPanel("Each Month", 
                       (fluidRow(
                         column(8,
                                fluidRow(
                                  box( title = "Entries For Each Month", solidHeader = TRUE, status = "primary", width = 12,
                                       plotOutput("hist6", height = 220)
                                  )
                                )
                         ),
                         column(4,
                                fluidRow(
                                  box(title = "Entries For Each Month as Table", solidHeader = TRUE, status = "primary", width = 12,
                                      dataTableOutput("tab6", height = 220)
                                  )
                                )
                         )
                       ))),
              tabPanel("Each Day", 
                       (fluidRow(
                         column(8,
                                fluidRow(
                                  box( title = "Entries For Each Day", solidHeader = TRUE, status = "primary", width = 12,
                                       plotOutput("hist7", height = 220)
                                  )
                                )
                         ),
                         column(4,
                                fluidRow(
                                  box(title = "Entries For Each Day as Table", solidHeader = TRUE, status = "primary", width = 12,
                                      dataTableOutput("tab7", height = 220)
                                  )
                                )
                         )
                       ))),
            ),style='width: 100%;margin-bottom: -10px;'
          )
        )
      ),
      tabItem(tabName = "about",
              fluidRow(
                h1("About"),
               div(
                 span("Written by Athalia Rochelle Handowo for CS 424 Project 1 Spring 2022 on Febuary. Data taken from Chicago Data Portal on February 4, 2022 ",
                  style = "white-space: pre-wrap"),
                 a(href="https://data.cityofchicago.org/Transportation/CTA-Ridership-L-Station-Entries-Daily-Totals/5neh-572f", "(link)"),
                 span("."),
                 style='display:flex;font-size:20px;')
              ),style='padding-right: 15px;padding-left: 15px;'
              )
    )
  ))

server <- function(input, output) {
  
  # increase the default font size
  theme_set(theme_grey(base_size = 14)) 
  
  output$Tab1 <- renderText({ input$StationName1 })
  output$Tab2 <- renderText({ input$StationName2 })
  # calculate the values one time and re-use them in multiple charts to speed things up
  stationNameReactive1 <- reactive({subset(CTA_daily, stationname == input$StationName1)})
  stationNameandYearsReactive1 <- reactive({subset(CTA_daily, year(date) == input$Year1 & stationname == input$StationName1)})
  
  stationNameYearsReactive1 <- reactive({
    data <- subset(CTA_daily, stationname == input$StationName1)
    tapply(data$rides, year(data$date), FUN=sum)
  })
  stationNameMonthsReactive1 <- reactive({
    data <- subset(CTA_daily, year(date) == input$Year1 & stationname == input$StationName1)
    tapply(data$rides, month(data$date,label = TRUE), FUN=sum)
    })
  stationNameDaysReactive1 <- reactive({
    data <- subset(CTA_daily, year(date) == input$Year1 & stationname == input$StationName1)
    tapply(data$rides, wday(data$date, label=TRUE), FUN=sum)
  })
  
  stationNameReactive2 <- reactive({subset(CTA_daily, stationname == input$StationName2)})
  stationNameandYearsReactive2 <- reactive({subset(CTA_daily, year(date) == input$Year2 & stationname == input$StationName2)})
  stationNameYearsReactive2 <- reactive({
    data <- subset(CTA_daily, stationname == input$StationName2)
    tapply(data$rides, year(data$date), FUN=sum)
  })
  stationNameMonthsReactive2 <- reactive({
    data <- subset(CTA_daily, year(date) == input$Year2 & stationname == input$StationName2)
    tapply(data$rides, month(data$date,label = TRUE), FUN=sum)
  })
  stationNameDaysReactive2 <- reactive({
    data <- subset(CTA_daily, year(date) == input$Year2 & stationname == input$StationName2)
    tapply(data$rides, wday(data$date, label=TRUE), FUN=sum)
  })
  
  # show a bar chart of enter per Date at StationName1
  output$hist0 <- renderPlot({
    oneYear <- stationNameReactive1()
    
    ggplot(oneYear, aes(x=year(date), y=rides)) + 
      labs(x="Years", y = "Rides") + 
      geom_bar(stat="identity", fill="steelblue") + scale_y_continuous()
  })
  
  # show a bar chart of enter per Date at StationName1
  output$hist1 <- renderPlot({
    oneYear <- stationNameandYearsReactive1()

    ggplot(oneYear, aes(x=date, y=rides)) + 
      labs(x=paste("Dates in", input$Year1), y = "Rides") + 
      geom_bar(stat="identity", fill="steelblue") + scale_y_continuous()
  })
  
  
  # show a bar chart of enter per Month at StationName1
  output$hist2 <- renderPlot({
    oneYear <- stationNameandYearsReactive1()

    ggplot(oneYear, aes(x=month(date,label = TRUE), y=rides)) + 
      labs(x=paste("Months in", input$Year1), y = "Rides") + 
      geom_bar(stat="identity", fill="steelblue") + scale_y_continuous()
  })

  
  # show a bar chart of entries per Day at StationName1
  output$hist3 <- renderPlot({
    oneYear <- stationNameandYearsReactive1()

    ggplot(oneYear, aes(x=wday(date,label = TRUE), y=rides)) + 
      labs(x=paste("Days in", input$Year1), y = "Rides") + 
      geom_bar(stat="identity", fill="steelblue") + scale_y_continuous()
  })
  
  

  # use DT to help out with the tables - https://datatables.net/reference/option/
  output$tab0 <- DT::renderDataTable(
    DT::datatable({
      rides <-  stationNameYearsReactive1()
      Dates <- data.frame(years = names(rides), rides=rides)
    },
    options = list(searching = FALSE, pageLength = 3, lengthChange = FALSE, order = list(list(0, 'asc'))
    ), rownames = FALSE
    )
  )
  
  output$tab1 <- DT::renderDataTable(
    DT::datatable({
      oneYear <-  stationNameandYearsReactive1()
      Dates <- data.frame(dates=oneYear$date, rides=oneYear$rides)
    },
    options = list(searching = FALSE, pageLength = 3, lengthChange = FALSE, order = list(list(0, 'asc'))
    ), rownames = FALSE
    )
  )
  
  output$tab2 <- DT::renderDataTable(
    DT::datatable({
      rides <-  stationNameMonthsReactive1()
      Months <- data.frame(months = names(rides), rides=rides)
    },
    options = list(searching = FALSE, pageLength = 3, lengthChange = FALSE
    ), rownames = FALSE
    )
  )
  
  output$tab3 <- DT::renderDataTable(
    DT::datatable({
      rides <-  stationNameDaysReactive1()
      Dates <- data.frame(days=names(rides), rides=rides)
    },
    options = list(searching = FALSE, pageLength = 3, lengthChange = FALSE
    ), rownames = FALSE
    )
  )
  
  # show a bar chart of enter per Date at StationName2
  output$hist4 <- renderPlot({
    oneYear <- stationNameReactive2()
    
    ggplot(oneYear, aes(x=year(date), y=rides)) + 
      labs(x='Years', y = "Rides") + 
      geom_bar(stat="identity", fill="steelblue") + scale_y_continuous()
  })
  
  
  # show a bar chart of enter per Date at StationName2
  output$hist5 <- renderPlot({
    oneYear <- stationNameandYearsReactive2()
    
    ggplot(oneYear, aes(x=date, y=rides)) + 
      labs(x=paste("Dates in", input$Year2), y = "Rides") + 
      geom_bar(stat="identity", fill="steelblue") + scale_y_continuous()
  })
  
  
  # show a bar chart of enter per Month at StationName2
  output$hist6 <- renderPlot({
    oneYear <- stationNameandYearsReactive2()
    
    ggplot(oneYear, aes(x=month(date,label = TRUE), y=rides)) + 
      labs(x=paste("Months in", input$Year2), y = "Rides") + 
      geom_bar(stat="identity", fill="steelblue") + scale_y_continuous()
  })
  
  
  # show a bar chart of entries per Day at UIC Halsted
  output$hist7 <- renderPlot({
    oneYear <- stationNameandYearsReactive2()
    
    ggplot(oneYear, aes(x=wday(date,label = TRUE), y=rides)) + 
      labs(x=paste("Days in", input$Year2), y = "Rides") + 
      geom_bar(stat="identity", fill="steelblue") + scale_y_continuous()
  })
  
  # use DT to help out with the tables - https://datatables.net/reference/option/
  output$tab4 <- DT::renderDataTable(
    DT::datatable({
      rides <-  stationNameYearsReactive2()
      Dates <- data.frame(years=names(rides), rides=rides)
    },
    options = list(searching = FALSE, pageLength = 3, lengthChange = FALSE, order = list(list(0, 'asc'))
    ), rownames = FALSE
    )
  )
  
  output$tab5 <- DT::renderDataTable(
    DT::datatable({
      oneYear <-  stationNameandYearsReactive2()
      Dates <- data.frame(dates=oneYear$date, rides=oneYear$rides)
    },
    options = list(searching = FALSE, pageLength = 3, lengthChange = FALSE, order = list(list(0, 'asc'))
    ), rownames = FALSE
    )
  )
  
  output$tab6 <- DT::renderDataTable(
    DT::datatable({
      rides <-  stationNameMonthsReactive2()
      Months <- data.frame(months = names(rides), rides=rides)
    },
    options = list(searching = FALSE, pageLength = 3, lengthChange = FALSE
    ), rownames = FALSE
    )
  )
  
  output$tab7 <- DT::renderDataTable(
    DT::datatable({
      rides <-  stationNameDaysReactive2()
      Dates <- data.frame(days=names(rides), rides=rides)
    },
    options = list(searching = FALSE, pageLength = 3, lengthChange = FALSE
    ), rownames = FALSE
    )
  )
  
  
}

shinyApp(ui = ui, server = server)

