library(shiny)

ui <- fluidPage(
  
  titlePanel("Central Limit Theorem in Shiny"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      sliderInput(inputId = "n",
                  label = "Orneklem Genisligini Seciniz",
                  min = 1, max = 50, value = 15,
                  animate = T)
    ),
    
    mainPanel(
      plotOutput("hist"),
      
      textOutput("hipotez0"),
      
      textOutput("hipotez1"),
      
      textOutput("shapiro"),
      
      plotOutput("qq")
    )
  )
)

server <- function(input, output){
  
  sayi <- reactive({
    as.numeric(input$n)
  })
  
  data <- reactive({
    as.numeric(rpois(n = sayi(), lambda = 3))
  })
  
  output$hist <- renderPlot({
    hist(data())
  })
  
  output$hipotez0 <- renderText({
    paste("H0 : Dagilim Normaldir")
  })
  
  output$hipotez1 <- renderText({
    paste("H1 : Dagilim Normal Degildir")
  })
  
  output$shapiro <- renderPrint({
    shapiro.test(data())
  })
  
  output$qq <- renderPlot({
    qqnorm(data())
    qqline(data())
  })
  
}

shinyApp(ui, server)
