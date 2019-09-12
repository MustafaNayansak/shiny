library(shiny)
library(magrittr)
library(caret)

ui <- fluidPage(
  
  titlePanel(h1("Shiny ile Dogrusal Regresyon Analizi",align = "center")),
  
  sidebarLayout(
    
    sidebarPanel(
      
      fileInput("file1","Bir Dosya Seciniz",
                multiple = TRUE,
                accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
      
      checkboxInput("header", "Header", TRUE),
      radioButtons("sep","Separator",choices = c(Comma = ",",Semicolon = ";",Tab = "\t"), selected = ","),
      selectInput("file2","Hazir Veri Setleri", choices = c("rock","pressure","cars","Carseats","Arthritis","Hitters","Boston","USArrests","Default","Airpassanger","Airpassanger2"))
      
    ,width = 3),
    
    mainPanel(
      
      tabsetPanel(
        
        tabPanel("Veri", tabName = "data",icon = icon("table"),
                 verbatimTextOutput(outputId = "data_out")),
        
        tabPanel("Degisken Islemleri",tabName = "degisken",icon = icon("database"),
                 br(),
                 uiOutput('bagimli'),
                 br(),
                 uiOutput('bagimsiz')
                 
        ),
        
        tabPanel("Regresyon", tabName = "lr",icon = icon("atom"),
                 h3("Dogrusal Regresyon"),
                 p("- Residuals : Artik olarak bilinen degerlerin tanimlayici istatistiklerini verir"),
                 p("- Coefficients : Estimate, modeldeki degiskenlerin Beta katsayilaridir, bu katsayilara ait tanimlayici istatistikler ve anlamlilik icin p degerleri vardir."),
                 p("- R-squared bagimsiz degiskenlerin, bagimli degiskendeki degisimi/varyansi/karakteristigi yuzde kacini acikladigini anlatan bir istatistiktir."),
                 p("- F-statistic modelin anlamliligi icin elde edilen p degeridir, p-value ise bu degere karsilik gelen degerdir."),
                 
                 verbatimTextOutput("model")
        ),
        
        tabPanel("Train-Test", icon = icon("first-order-alt"),
                 br(),
                 p("- Modeli kurduktan sonra modelin tahmin performansini olcmek icin, modeli train ve test olarak ikiye ayiriyoruz.",col = "Red"),
                 p("- Egitim icin ayirdigimiz Train Data ile otomatik bir model kurma islemi yapiyoruz, bu modelin ozet istatistiklerini asagida gosteriyoruz"),
                 uiOutput("obs"),
                 verbatimTextOutput("new_model")),
        
        tabPanel("Sonuclar",icon = icon("poll"),
                 tableOutput("tahmin_out"),
                 verbatimTextOutput("mse"),
                 downloadButton('download',"Tahmin Verisini Kaydet")
                 
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  rv <- reactiveValues()
  
  data <- reactive({
    
    inFile <- input$file1
    
    if (is.null(inFile))
      
      return(switch(input$file2,
                    "rock" = rock,
                    "pressure" = pressure,
                    "cars" = cars,
                    "Carseats" = Carseats,
                    "Arthritis" = Arthritis,
                    "Hitters" = Hitters,
                    "Boston" = Boston,
                    "USArrests" = USArrests,
                    "Default" = Default,
                    "Airpassanger" = WDI(country = c("TR"), indicator = c("NY.GDP.MKTP.CD"), start=1960, end = 2018),
                    "Airpassanger2" = WDI(country = c("TR"), indicator = c("NY.GDP.MKTP.CD", "SP.DYN.LE00.IN"), start=1960, end = 2018)
                    
      ))
    
    read.csv(input$file1$datapath, header = input$header,sep = input$sep,quote = input$quote)
    
  })
  
  output$data_out <- renderPrint({
    data()
  })
  
  observe({rv$Train <- data()})
  
  output$bagimli <- renderUI({
    
    selectInput('bagimli', h4('Bagimli Degiskeni Seciniz'), 
                choices = names(rv$Train[]), 
                selected = names(rv$Train[]))
  })
  
  output$bagimsiz <- renderUI({
    checkboxGroupInput('bagimsiz', h4('Bagimsiz Degiskenleri Seciniz'), 
                       choices = names(rv$Train[]),
                       selected = names(rv$Train[]) ,inline = TRUE)
  })
  
  output$model <- renderPrint({
    input$bagimli
    input$bagimsiz
    veri <- data()
    
    form <- as.formula(paste(names(data())[names(data()) %in% input$bagimli], "~",
                             paste(names(data())[names(data()) %in% input$bagimsiz], collapse="+")))
    lm(form, data=veri)
    
    # glm() fonksiyonu ile modelimizi kuruyoruz ve bir degiskene kaydediyoruz  
    model <- lm(as.formula(form),data=veri)
    print(summary(model))
    
  })
  
  output$obs = renderUI({
    sliderInput('obs', label = "Test Icin Ayirmak Istediginiz Yuzdeligi Seciniz",min = 0, max = 1, value = 0.8,width = 400)
  })
  
  t_i<- reactive({
    createDataPartition(y = rv$Train[,input$bagimli], p = input$obs, list=F, times=1)
  })
  
  egt <- reactive({
    rv$Train[t_i(),]
  })
  
  test <- reactive({
    rv$Train[-t_i(),]
  })
  
  egt_x<- reactive({
    bagimli <- input$bagimli
    bagimsiz<- input$bagimsiz
    egt() %>% dplyr::select(-bagimli) %>% dplyr::select(bagimsiz)
  })
  
  egt_y<- reactive({
    bagimli <- input$bagimli
    egt() %>% dplyr::select(bagimli)
  })
  
  test_x<- reactive({
    bagimli <- input$bagimli
    bagimsiz<- input$bagimsiz
    test() %>% dplyr::select(-bagimli) %>% dplyr::select(bagimsiz)
  })
  
  test_y<- reactive({
    bagimli <- input$bagimli
    test() %>% dplyr::select(bagimli)
  })
  
  egt_tum<- reactive({
    data.frame(egt_x(), dv = egt_y())
  })
  
  new_formul<- reactive({
    as.formula(paste(input$bagimli, paste(input$bagimsiz, collapse=" + "), sep=" ~ "))
  })
  
  output$new_model <- renderPrint({
    
    veri <- egt_tum()
    egitim_x <- egt_x()
    egitim_y <- egt_y()
    
    form1 <- as.formula(paste(names(egt_y())[names(egt_y()) %in% input$bagimli], "~", 
                              paste(names(egt_x())[names(egt_x()) %in% input$bagimsiz],
                                    collapse="+")))
    
    model1 <- lm(as.formula(form1),data=veri)
    summary(model1)
    
  })
  
  new_model_rea <- reactive({
    
    veri <- egt_tum()
    egitim_x <- egt_x()
    egitim_y <- egt_y()
    
    form1 <- as.formula(paste(names(egt_y())[names(egt_y()) %in% input$bagimli], "~", 
                              paste(names(egt_x())[names(egt_x()) %in% input$bagimsiz],
                                    collapse="+")))
    
    model1 <- lm(as.formula(form1),data=veri)
    
  })
  
  pred <- reactive({
    round(predict(new_model_rea(), test_x()),digits = 3)
    })
  
  output$tahmin_out <- renderTable({
    data.frame( "Index" = seq(1:nrow(test_y())),
                "Gercek" =test_y(),
                "Tahmin" =pred())
  })
  
  tahmin <- reactive({
    pred <- as.double(unlist(pred()))
    test_y <- round(as.numeric(unlist(test_y())), digits = 3)
    
    data.frame( "Index" = seq(1:nrow(test_y())),
                "Gercek" = test_y,
                "Tahmin" = pred)
  })
  
  output$mse <- renderPrint({
    pred <- as.numeric(unlist(pred()))
    test_y <- round(as.numeric(unlist(test_y())), digits = 3)
    defaultSummary(data.frame(obs = test_y,
                              pred = as.vector(pred()))
    )
  })
  
  output$dto <- renderDataTable({tahmin()})
  output$download <- downloadHandler(
    filename = function(){"tahmin.csv"}, 
    content = function(fname){
      write.csv(tahmin(), fname)
    }
  )
  
}
shinyApp(ui, server)