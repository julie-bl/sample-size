

library(shiny)
library(shinydashboard)
library(epiR)
library(shinythemes)
library(shinyjs)


#J:/4-Méthodologiste/Antoine/logo-chu-poitiers_numerique.png

ui <- fluidPage(
  useShinyjs(),
  shinythemes::themeSelector(),
  fluidRow(style = "height:100px",
           column(width = 3,
                  style = "height:100%",
                  tags$img(src = "logo-Horizontal-CHU-poitiers_Couleurs.png",
                    style = 'height: 100%')),
           column(width = 9,
                  style="height: 100%",
                  includeHTML("appName.html"))
           ),
  fluidRow(
  navbarPage(title = icon("users",class ="fa-solid fa-users"),
             id = "hypMenu",
             tabPanel(title = "Read Me",
                      includeHTML("ReadMe.html")),
             navbarMenu(title = "Study Design",
                        tabPanel(title = "Two means",
                                 fluidRow(
                                   column(
                                     width = 12,
                                     selectInput(inputId = "hypMean",
                                                 label = "Hypothesis",
                                                 choices = c(" ","Superiority", "Non-inferiority")),
                                     #when an item is selected, the next appear
                                     conditionalPanel(
                                       condition = "input.hypMean=='Superiority' || input.hypMean=='Non-inferiority'",
                                       numericInput(inputId = "puissanceMean",
                                                    label = "Power (%)",
                                                    value = NULL,
                                                    max = 100,
                                                    min = 0),
                                       conditionalPanel(
                                         condition = "input.puissanceMean",
                                         numericInput(inputId = "alphaMean",
                                                      label = "Alpha (%)",
                                                      value = NULL,
                                                      max = 20,
                                                      min = 0,
                                                      step = 0.5),
                                         conditionalPanel(
                                           condition = "input.alphaMean ",
                                           h5("Define the mean expected in the experimental group"),
                                           numericInput(inputId = "obsMean",
                                                        label = "Experimental",
                                                        value = NULL,
                                                        step = 0.005),
                                           h5("Define the mean expected in the control group"),
                                           numericInput(inputId = "prevMean",
                                                        label = "Control",
                                                        value = NULL,
                                                        step = 0.005)
                                         ),
                                         
                                       )
                                     )
                                   )
                                 )),
                        tabPanel(title = "Two proportions",
                                 fluidRow(
                                   column(
                                     width = 12,
                                     selectInput(inputId = "hypProp",
                                                 label = "Hypothesis",
                                                 choices = c(" ","Superiority", "Non-inferiority")),
                                     #when an item is selected, the next appear
                                     conditionalPanel(
                                       condition = "input.hypProp=='Superiority' || input.hypProp=='Non-inferiority'",
                                       numericInput(inputId = "puissanceProp",
                                                    label = "Power (%)",
                                                    value = NULL,
                                                    max = 100,
                                                    min = 0),
                                       conditionalPanel(
                                         condition = "input.puissanceProp",
                                         numericInput(inputId = "alphaProp",
                                                      label = "Alpha (%)",
                                                      value = NULL,
                                                      max = 20,
                                                      min = 0,
                                                      step = 0.5),
                                         conditionalPanel(
                                           condition = "input.alphaProp",
                                           h5("Define the proportion expected in the experimental group"),
                                           numericInput(inputId = "obsProp",
                                                        label = "Experimental",
                                                        value = NULL,
                                                        step = 0.005),
                                           h5("Define the proportion expected in the control group"),
                                           numericInput(inputId = "prevProp",
                                                        label = "Control",
                                                        value = NULL,
                                                        step = 0.005)
                                         ),
                                         
                                       )
                                     )
                                   )
                                 )))
             )),
    
    
  
  )

#         
#             menuItem("Sample Size Calculator", tabName = "SSC", icon = icon("users",class ="fa-solid fa-users"))
#         )
#     ),
#     dashboardBody(
#       
#         tabItems(
#             tabItem(tabName = "SSC",
#                     fluidRow(
#                         tabsetPanel(
#                             
#                             ##########################################
#                             ############## PRESENTATION ##############
#                             ##########################################
#                             tabPanel(includeHTML("ReadMe.html"))),
#                             
#                             ##########################################
#                             ############### CALCULATOR ###############
#                             ##########################################
#                             tabPanel("CALCULATOR", box(title = "Instructions",
#                                                        p("Define a power i.e. a percentage of times the null hypothesis will be correctly rejected :"),
#                                                        numericInput(inputId = "puissance",
#                                                                     label = "Power (%)",
#                                                                     value = 90,
#                                                                     max = 100,
#                                                                     min = 0),
#                                                        p("Define a level of significance :"),
#                                                        numericInput(inputId = "alpha",
#                                                                     label = "Alpha (%)",
#                                                                     value = 5,
#                                                                     max = 20,
#                                                                     min = 0,
#                                                                     step = 0.5),
#                                                        p("Define the prevalence observed of the principal criterion :"),
#                                                        numericInput(inputId = "obs",
#                                                                     label = "Observed",
#                                                                     value = 0.2,
#                                                                     step = 0.005),
#                                                        p("Define the prevalence expected of the principal criterion :"),
#                                                        numericInput(inputId = "hyp",
#                                                                     label = "Hypothesis",
#                                                                     value = 0.05,
#                                                                     step = 0.005),
#                                                        p("Define the number of patient group :"),
#                                                        numericInput(inputId = "grp",
#                                                                     label = "Number of groups",
#                                                                     value = 2,
#                                                                     min = 2),
#                                                        p("One-sided test or two sided-test ? :"),
#                                                        selectInput(inputId = "test",
#                                                                   label = "Test",
#                                                                   choices = c("one-sided", "two-sided")),
#                                                        width = 5),
#                                      box(title = "Number of individuals required per group : ",
#                                          actionButton("go", "Result"),
#                                          verbatimTextOutput("text"), width = 5),
#                                      box(title = "Table",
#                                          dataTableOutput("table_pred"))
#                                      )
#                             )
#                         )
#                     )
#             )
#         )
#     
#   
# 
# 
# 
# 
# Define server logic required to draw a histogram
server <- function(input, output) {

    t <- reactive( if (reactive(input$test)()=="one-sided") 1 else 2 )

    f <- eventReactive(input$go, {
        epi.sscohortc(N = NA, irexp1 = input$obs, irexp0 = input$hyp, pexp = NA, n = NA,
                      power = input$puissance/100, r = input$grp-1, design = 1, sided.test = t(),
                      finite.correction = FALSE, nfractional = FALSE, conf.level = (1-(input$alpha/100)))$n.exp1
    })
    output$text <- renderText(f())
}

# Run the application
shinyApp(ui = ui, server = server)
