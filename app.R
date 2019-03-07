#install.packages('google')
library(shiny)
library(shinydashboard)
library(ggplot2)
library(scales)


#options(browser = "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe")

ui <- fluidPage(
  dashboardPage(
    dashboardHeader(title = "My First Shiny Dashbaord"),
    dashboardSidebar(
      
      sidebarSearchForm("searchText", "buttonSearch", "Search"),
      sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon('dashboard')),
        menuSubItem("Sales Dashboard", tabName = "Sales", icon = icon('chart-line')),
        menuSubItem("Finance Dashboard", "Finance", icon = icon('chart-bar')),
        menuItem("Detailed Analysis", tabName = "Details", icon = icon('bar-chart-o')),
        menuItem("Raw Data", tabName = "Raw", icon = icon('table'))
      )
      
      
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "dashboard",
                h1("Executive Dashboard"),
                fluidRow(
                  infoBoxOutput("boxvals1"),
                  infoBoxOutput("boxvals2")

                ),
                fluidRow(
                  valueBoxOutput("valbox2"),
                  valueBoxOutput("valbox1")
                ),
                fluidRow(
                  tabBox(
                    title = "Tabbed Box",
                    tabPanel(title = "Histogram of Eruptions", status = "primary",solidHeader = T, plotOutput('Report1'), background = "light-blue"),
                    tabPanel(title = "Histogram of Waiting", status = "primary",solidHeader = T,plotOutput('Report2'),background = "light-blue")
                  )),
                fluidRow(
                  box(title = "Controls", solidHeader = T, status = "warning",
                      "This is a control box description",br(),
                      "This is the second linke of instruction using BR HTML feature",
                      sliderInput('bins','Number of Breaks',1,100,50),
                      textInput("txt_inpt","Search Store"))
                )
                
        ),
        tabItem(tabName = "Sales",
                h2("Sales Dashboard"),
                fluidRow(
                  column(4),
                  selectInput("FctSel", "Select Measure", choices = colnames(faithful))
                ),
                fluidRow(
                  box(title = "Density Curve", solidHeader = T,status = 'primary', plotOutput('Report6'))
                )
        ),
        tabItem(tabName = "Finance",
                h2("Finance Dashboard")
        ),
        tabItem(tabName = "Details",
                h1("Detailed Report"),
                box(status = "primary",solidHeader = T,plotOutput("Report4")),
                box(status = "primary",solidHeader = T,plotOutput("Report5"))
        ),
        tabItem(tabName = "Raw",
                h1("Raw Data"),
                fluidRow(
                  box(status = "primary",solidHeader = T,plotOutput('Report3'))
                )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  output$Report1 <- renderPlot({
    hist(faithful$eruptions, breaks = input$bins, ylab = 'Freq', xlab = 'Eruptions')
  })
  output$Report2 <- renderPlot({
    hist(faithful$waiting, breaks = input$bins, xlab = "Waiting")
  })
  output$Report3 <- renderPlot({
    ggplot(data = faithful) +
      geom_point(mapping = aes(x = eruptions, y = waiting, color = waiting))
  })
  output$Report4 <- renderPlot({
    ggplot(data = faithful) +
      geom_density(kernel = "gaussian", mapping = aes(x = eruptions)) +
      labs(title = "Guassian Density Curve: Eruptions")
  })
  output$Report5 <- renderPlot({
    ggplot(data = faithful) +
      geom_density(kernel = "gaussian", mapping = aes(x = waiting)) +
      labs(title = "Guassian Density Curve: Waiting")
  })
  output$Report6 <- renderPlot({
    ggplot(data = faithful) +
      geom_density(kernel = "gaussian", mapping = aes(x = faithful[,input$FctSel])) +
      labs(title = "Guassian Density Curve", x = toupper(input$FctSel))
  })
  output$boxvals1 <- renderInfoBox({
    infoBox("Average Eruptions",format(mean(faithful$eruptions),digits = 2),icon = icon('chart-line'))
  })
  output$boxvals2 <- renderInfoBox({
    infoBox("Average Waiting",format(mean(faithful$waiting),digits=3),icon = icon('chart-line'))
  })
  output$valbox1 <- renderValueBox({
    valueBox(number(mean(faithful$waiting)/mean(faithful$eruptions), accuracy = .01),"Faithful Index", icon = icon('phoenix-framework'))
  })
  ix = mean(faithful$waiting)/mean(faithful$eruptions) -5
  output$valbox2 <- renderValueBox({
    valueBox(number(ix, accuracy = .01),"Faithful Index", icon = icon('phoenix-framework'),
             if (ix <20) {
               color = 'red'
             }else{
               color = 'green'
             })
  })
}

# Run the app
shinyApp(ui, server)
0