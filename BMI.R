library(shiny)

ui <- fluidPage(
  
  titlePanel("BMI in Shiny"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      textInput(inputId = "isim",
                label = "Bir isim Giriniz"),
      
      selectInput(inputId = "sehir",
                  label = "Bir Sehir Seciniz",
                  choices = c("Adana", "Ankara", "Antalya", "Istanbul"),
                  selected = "Adana"),
      
      textInput(inputId = "yas",
                label = "Yasiniz"),
      
      numericInput(inputId = "boy",
                   label = "Boy Uzunlugunuz",
                   value = 175),
      
      numericInput(inputId = "kilo",
                   label = "Kilonuz",
                   value =75),
      
      actionButton(inputId = "aksiyon",
                   label = "Hesapla")
      
      
    ),
    
    mainPanel(
      
      p(h3("Kisisel Bilgiler")),
        
      textOutput("isim_out"),
      
      textOutput("sehir_out"),
      
      textOutput("yas_out"),
      
      br(),
      
      p(h3("Analiz Sonuclari")),
      
      textOutput("bmi_out"),
      
      br(),
      
      br(),
      
      HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/LIVV0PEt61Q" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
      
    )
  )
)


server <- function(input, output){
  
  output$isim_out <- renderText({
    input$aksiyon
    isolate(paste("Isim : ", input$isim))
  })
  
  output$sehir_out <- renderText({
    input$aksiyon
    isolate(paste("Sehir : ", input$sehir))
  })
  
  output$yas_out <- renderText({
    input$aksiyon
    isolate(paste("Yas : ", input$yas))
  })
  
  bmi <- reactive({
    input$kilo / (input$boy^2) * 10000
  })
  
  output$bmi_out <- renderText({
    input$aksiyon
    isolate(paste("Vucut kitle indeksiniz : ", bmi()))
  })
}

shinyApp(ui, server)