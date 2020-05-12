library(shiny)
library(shinydashboard)
dashboard1<-readRDS("data.Rds")

dashboard1$year<- as.factor(dashboard1$year)

ui <- dashboardPage(
  dashboardHeader(title = "A Demographic Portrait of Poverty in Nebraska ",titleWidth = 900),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      selectizeInput("var", label="States:",
                     choices = dashboard1$states,
                     selected = c("Nebraska","United States"),
                     multiple=TRUE),
      
      column(width = 12,
      box(title = "Poverty Percentage",plotOutput("plot1", height = 400))
      ),
    column(width = 12,
      box(title = "child Poverty Percentage", plotOutput("plot2", height = 400)),
    
      box(title = "Female heads living under poverty",plotOutput("plot3", height = 400))
    
    )
    
    )
  )
    
)


server <- function(input, output) {
  
  
  plotInput1 <- reactive({
    
    v <- input$var
    dash1 <- subset(dashboard1,dashboard1$state %in% v)
    
  

  ggplot(subset( dash1, (dash1$variable == 'Poverty') , select = c( states, value,year ) ), aes(x=year, y=value, group=states,color= states )) +
  geom_line()+geom_point()+xlab("Year")+ylab("Poverty Percentage")+expand_limits( y = 0)
  


})


output$plot1 <- renderPlot({
  print(plotInput1())
})

plotInput2 <- reactive({
  
  v <- input$var
  dash1 <- subset(dashboard1,dashboard1$state %in% v)
  
  ggplot(subset( dash1, (dash1$variable == 'Child'), select = c( states, value,year ) ), aes(x=year, y=value, group=states,color= states )) +
    geom_line()+geom_point()+xlab("Year")+ylab("Child Percentage")+expand_limits( y = 0)
  
})


output$plot2 <- renderPlot({
  print(plotInput2())
})

plotInput3 <- reactive({
  
  v <- input$var
  dash1 <- subset(dashboard1,dashboard1$state %in% v)
  
  ggplot(subset( dash1, (dash1$variable == 'FemHead') , select = c( states, value,year ) ), aes(x=year, y=value, group=states,color= states )) +
  geom_line()+geom_point()+xlab("Year")+ylab("Female Head Poverty Percentage")+expand_limits( y = 0)
  
})


output$plot3 <- renderPlot({
  print(plotInput3())
})

}

shinyApp(ui, server)
