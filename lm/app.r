library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("BSGP 2024 LM Dashboard"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            # Input: Select a file ----
            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Checkbox if file has header ----
            checkboxInput("header", "Header", TRUE),
            
            # Input: Select separator ----
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            
            # Input: Select quotes ----
            radioButtons("quote", "Quote",
                         choices = c(None = "",
                                     "Double Quote" = '"',
                                     "Single Quote" = "'"),
                         selected = '"'),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Select number of rows to display ----
            radioButtons("disp", "Display",
                         choices = c(Head = "head",
                                     All = "all"),
                         selected = "head"),
            tags$hr(),
            actionButton("go", "Plot Linear Model")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("origPlot"),
           plotOutput("lmPlot"),
           tableOutput("contents"),
           verbatimTextOutput("modelSummary"),
           verbatimTextOutput("corrCoeff")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    lmdata <- reactiveValues()
    
    dataInput <- reactive({
        req(input$file1)
        
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
        return(df)
    })
    
    observeEvent(input$go, {
        update_lm()
    })
    
    update_lm <- function(){
        lmdata$model <- lm(y ~ x, data = dataInput())
    }
    
    output$origPlot <- renderPlot({
        plot(dataInput()$x, dataInput()$y, xlab = "X", ylab = "Y", main = "Original Data")
    })
    
    output$lmPlot <- renderPlot({
        plot(dataInput()$x, dataInput()$y, xlab = "X", ylab = "Y", main = "Linear Model")
        abline(lmdata$model, col = "red")
    })
    
    output$contents <- renderTable({
        if(input$disp == "head") {
            return(head(dataInput()))
        } else {
            return(dataInput())
        }
    })
    
    output$modelSummary <- renderText({
        req(lmdata$model)
        slope <- coef(lmdata$model)[2]
        intercept <- coef(lmdata$model)[1]
        paste("Slope:", round(slope, 3), "Intercept:", round(intercept, 3))
    })
    
    output$corrCoeff <- renderText({
        req(lmdata$model)
        corr_coeff <- cor(dataInput()$x, dataInput()$y)
        paste("Correlation Coefficient:", round(corr_coeff, 3))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
