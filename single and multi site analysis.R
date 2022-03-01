################################
#   Single and Multi Site Analysis  
#       Lme4 and Metan
################################


library(shiny)
library(ggplot2)
library(metan)
library(DT)
library(plyr)
library(cowplot)
library(readxl)
library(xlsx)

ui <- fluidPage(
  titlePanel("Multi-site Analysis"),
  sidebarLayout(
    sidebarPanel(
      
      width = 3,
      radioButtons(
        "fileType_Input",
        label = h4("Choose File type"),
        choices = list(".csv/txt" = 1, ".xlsx" = 2),
        selected = 1,
        inline = TRUE
      ),
      fileInput(
        'file1',
        h4('Upload Data'),
        accept = c(
          'text/csv',
          'text/comma-separated-values,text/plain',
          '.csv',
          '.xlsx'
        )),
      tags$hr(),
      checkboxInput('header', 'Header', TRUE),
      
      numericInput("sheet", "Sheet Number:", 1, min = 1, max = 10),
      verbatimTextOutput("value"),
      
      radioButtons('sep', 'Separator',
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t'),
                   ',', inline = TRUE),
      radioButtons('quote', 'Quote',
                   c(None='',
                     'Double Quote'='"',
                     'Single Quote'="'"),'"', inline = TRUE),
      
      
      selectInput(inputId = "Treatment", "Select Treatment", choices = NULL),
      selectInput(inputId ="Rep", "Select Rep", choices = NULL),
      selectInput(inputId ="Block", "Select Block", choices = NULL),
      selectInput(inputId ="Site", "Select Site", choices = NULL),
      selectInput(inputId ="Target", "Select Target", choices = NULL),
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data", DTOutput("dataa")),
        tabPanel("EDA", plotOutput("hist")),
        #tabPanel("EDA", plotOutput("box_10")),
        tabPanel("Single", verbatimTextOutput("single")),
        tabPanel("Multi", verbatimTextOutput("Multi")),
        tabPanel("Residuals", plotOutput("resids")),
        tabPanel("GxE", plotOutput("GE")),
      )
    )
  
  )
)


server <- function(input, output, session){
  myData <- reactive({
    req(input$file1)
    inFile <- input$file1
    #if (is.null(inFile)) return(NULL)
    #data <- read.csv(inFile$datapath, header = TRUE, sep = ";")
    if (is.null(inFile)) {
      return(NULL) }
    
    if (input$fileType_Input == 1) {
      data = read.csv(inFile$datapath,
                      #header = TRUE,
                      stringsAsFactors = FALSE, header=input$header, sep=input$sep, 
                      quote=input$quote)
    } else {
      data = read.xlsx(inFile$datapath,
                       #header = TRUE,
                       sheetIndex = input$sheet,
                       stringsAsFactors = FALSE)
    }
    data
  })
  
  ##############################################################
  output$dataa <- renderDT(
    myData(),
    filter = "top",
    options = list(
      pageLength = 10
    )
  )
  
  ###################################################################33
  observe({
    data <- myData()
    updateSelectInput(session, 'Rep', choices = names(data))
  }) 
  
  
  observe({
    data <- myData()
    updateSelectInput(session, 'Block', choices = names(data))
  }) 
  
  
  observe({
    data <- myData()
    updateSelectInput(session, 'Treatment', choices = names(data))
  }) 
  
  
  observe({
    data <- myData()
    updateSelectInput(session, 'Target', choices = names(data))
  }) 
  
  observe({
    data <- myData()
    updateSelectInput(session, 'Site', choices = names(data))
  }) 
  
  ####################################################################
  
  #df = myData()
  
  #mu <- ddply(df, "sex", summarise, grp.mean=mean(weight))
  #head(mu)
  
  output$hist <- renderPlot({
    df = myData()
    f <- ggplot(df, aes_string(x=input$Target)) +
      geom_histogram(fill="#69b3a2", color="#e9ecef", alpha=0.9) +
      ggtitle("Histogram Plot") + theme_bw()
    
    g <- ggplot(df, aes_string(x = as.factor(df[[input$Site]]), 
                               y = input$Target, fill = input$Site)) + 
      geom_boxplot() + geom_jitter() + theme_bw()
    
    plot_grid(f, g,  labels = c('A', 'B'))
  })
  
  
  ##########################################################################
  
  single_model <- reactive({
    if (input$Block == "NULL") {
      ind_model <- anova_ind(myData(), input$Site, input$Treatment, input$Rep, 
                       input$Target)
      return(ind_model)
    } else {
      ind_model <- anova_ind(myData(), input$Site, input$Treatment, input$Rep, 
                         block= input$Block, input$Target)
      return(ind_model)
    }
    
  })
  
  output$single <- renderPrint({
    req(single_model())
  })

  ####################################################################
  
  multi_model <- reactive({
    if (input$Block == "NULL") {
      joint_model <- anova_joint(myData(), input$Site, input$Treatment, input$Rep, 
                             input$Target)
      return(joint_model)
      return(gmd(joint_model, "h2"))
    } else {
      joint_model <- anova_joint(myData(), input$Site, input$Treatment, input$Rep, 
                             block= input$Block, input$Target)
      return(joint_model)
      return(gmd(joint_model, "h2"))
    }
    
  })
  
  output$Multi <- renderPrint({
    req(multi_model())
  })  
  
  ################################################
  # Residuals
 
   output$resids <- renderPlot({
    plot(multi_model)
  })
  
  
  
  ##############################################################
  # GXE
  
  
    
  output$GE <- renderPlot({
    df = myData()
    ge <- gge(df, input$Site, input$Treatment, input$Target, centering = "environment")
    a <- plot(ge())
    b <- plot(ge(), type = 3)
    c <- plot(ge(), type = 8)
    plot_grid(a, b, c, labels = c('A', 'B', 'C'))
  })  
  
  
  
  
  
  
}


shinyApp(ui = ui, server = server)