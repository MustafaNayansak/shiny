library(shiny)

ui <- fluidPage(
  
  titlePanel(h1("Shiny'de Dosya Yukleme ve Basit Istatistiksel Analizler", align = "center")),
  
  sidebarLayout(
    
    sidebarPanel(
      
      fileInput(inputId = "dosya",
                label = "Bir Dosya Seciniz",
                multiple = TRUE),
      
      checkboxInput(inputId = "header",
                    label = "Header",
                    value = TRUE),
      
      radioButtons(inputId = "sep",
                   label = "Separator",
                   choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
                   selected = ";"),
      
      radioButtons(inputId = "disp",
                   label = "Display",
                   choices = c(Head = "head", All = "all"),
                   selected = "head"),
      
      br(),

      selectInput("var","CHD Veri Seti Degiskenleri",
                     choices = c("AGE" = 1,
                                 "CHD" = 2,
                                 "AGRP" = 3)),
      
      selectInput("color", "Grafik Icin Renk Seciniz",
                  choices = c("Red", "Blue", "Yellow"))
    ),
    
    mainPanel(
      
      tabsetPanel(
        
        tabPanel(title = "Data",
                 
                 tableOutput("data_out")
                 
                 ),
        
        tabPanel(title = "Structure",
                 
                verbatimTextOutput("str_out")),
        
        tabPanel(title = "Summary",
                 
                 verbatimTextOutput("sum_out")),
        
        tabPanel(title = "Histogram",
                 plotOutput("hist_out"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  
  data <- reactive({
    
    req(input$dosya)
    
      df <- read.csv(file = input$dosya$datapath,
                     header = input$header,
                     sep = input$sep)

      if(input$disp=="head"){
        return(head(df))
      }
      else
      {
        return(df)
      }
    
  })
  
  output$data_out <- renderTable({
    data()
  })
  
  output$str_out <- renderPrint({
    str(data())
  })
  
  output$sum_out <- renderPrint({
    summary(data())
  })
  
  output$hist_out <- renderPlot({
  colm <- as.numeric(input$var)
  col_n<- as.numeric(input$col)
  hist(data()[,colm], col = input$color)
  })
  
  
  

}

shinyApp(ui, server)