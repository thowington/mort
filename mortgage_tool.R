#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Investigating Mortgage Payments"),
   
   sidebarLayout(
     sidebarPanel(
        h5("Enter the details of your prospective mortgage. Adjust the parameters of the mortgage to
           explore how your payments change."),
        sliderInput("PP",
                    "Purchase price in USD:",
                    min = 50000,
                    max = 1000000,
                    value = 400000),
        sliderInput("DP",
                    "Down payment percent:",
                    min = 0,
                    max = 50,
                    value = 20),
        sliderInput("IR",
                    "Interest rate:",
                    min = 2,
                    max = 10,
                    value = 3.25,
                    step = .25),
        sliderInput("L",
                    "Length of mortgage (years):",
                    min = 10,
                    max = 50,
                    value = 30),        
        sliderInput("TX",
                    "Annual property taxes in USD *:",
                    min = 500,
                    max = 30000,
                    value = 6000),
        h6("* If you don't know the taxes, try about 1.5% of purchase price, for starters."),
        sliderInput("PI",
                    "Annual property insurance in USD *:",
                    min = 200,
                    max = 20000,
                    value = 3000),
        h6("* If you aren't sure about property insurance, start with about 0.75% of the purchase price.")
     ),
  
  
      # Show payment info
      mainPanel(
              tags$head(tags$style("#TotalPayment{color: red;
                                 font-size: 20px;
                             font-style: italic;
                             }"
                         )
        ),
        tags$b(h1("Total Monthly Payment USD:")),
        textOutput("TotalPayment"),

        tags$ul(
        tags$li(h3("Monthly Principal and Interest Payment")),
        tags$li(textOutput("LoanPayment")),
        tags$li( h3("Monthly Property Tax Payment:")),
        tags$li(textOutput("TaxPayment")),
        tags$li(h3("Monthly Property Insurance Payment:")),
        tags$li(textOutput("InsPayment")))


        )

      )
   )
  




# Define server logic required to return payment info
server <- function(input, output) {
  
                    IR <- reactive({input$IR})
                    J <- reactive({IR()/(12*100)})
                    DP <- reactive({input$DP})
                    DPP <- reactive({DP()}/100)
                    
                    L<- reactive({input$L})
                    N <- reactive({L()*12})
                    
                    PP <- reactive({input$PP})
                    
                    TX <- reactive({input$TX})
                    PI <- reactive({input$PI})
                    
                    LoanPayment <- reactive({PP()*(1-DPP())*(J()/(1-(1+J())^(-N())))})
                    InsPayment <- reactive({PI() / 12})
                    TaxPayment <- reactive({TX() / 12})


                    #output$TotalPayment <- renderText(PP()*(1-DPP())*(J()/(1-(1+J())^(-N()))))
                    output$LoanPayment <- renderText(LoanPayment())
                    output$TaxPayment <- renderText(TaxPayment())
                    output$InsPayment <- renderText(InsPayment())
                    output$TotalPayment <- renderText(LoanPayment()+TaxPayment()+InsPayment())
               
                    
                    

                  }


# Run the application 
shinyApp(ui = ui, server = server)

