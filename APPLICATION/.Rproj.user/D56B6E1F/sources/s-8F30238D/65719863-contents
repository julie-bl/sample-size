

library(shiny)
library(shinydashboard)
library(epiR)
library(shinythemes)
library(shinyjs)


#J:/4-Méthodologiste/Antoine/logo-chu-poitiers_numerique.png



#########################################################################################################################
###################################################   UI  ###############################################################
#########################################################################################################################

ui <- fluidPage(
  useShinyjs(),
  theme = shinytheme("flatly"),
  fluidRow(style = "height:100px",
           column(width = 3,
                  style = "height:100%",
                  tags$a(href='https://chu-poitiers.fr',
                         tags$img(src='LogoCHU.png', height = "100%" , align="left"))),
           column(width = 9,
                  style="height: 100%",
                  includeHTML("appName.html"))
           ),
  fluidRow(
  navbarPage(title = icon("users",class ="fa-solid fa-users"),
# PAGE COMPARING PROPORTIONS --------------------------------------------------------------------------------------------
             tabPanel(title = "COMPARING PROPORTIONS",
                      fluidRow(column(width = 12,
                                      h3("COMPARING PROPORTIONS"))),
                      br(),
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(inputId = "hypProp",
                                      label = "Hypothesis",
                                      choices = c(" ","superiority", "non-inferiority")),
                          #when an item is selected, the next appear
                          numericInput(inputId = "powerProp",
                                       label = "Power (%)",
                                       value = NULL,
                                       max = 100,
                                       min = 0),
                          numericInput(inputId = "alphaProp",
                                       label = "Alpha (%)",
                                       value = NULL,
                                       max = 20,
                                       min = 0,
                                       step = 0.5),
                          conditionalPanel(
                            condition = "input.hypProp == 'superiority' || input.hypProp == 'non-inferiority'",
                            h5("Proportion expected in the control group :"),
                            numericInput(inputId = "obsProp",
                                         label = "Control (%)",
                                         value = NULL,
                                         step = 0.005)),
                          conditionalPanel(
                            condition = "input.hypProp == 'superiority'",
                            h5("Proportion expected in the experimental group :"),
                            numericInput(inputId = "prevProp",
                                         label = "Experimental (%)",
                                         value = NULL,
                                         step = 0.005),
                            selectInput(inputId = "sidetestProp",
                                        label = "test",
                                        choices = c("two-sided", "one-sided"))),
                          conditionalPanel(
                            condition = "input.hypProp == 'non-inferiority'",
                            h5("Absolute margin :"),
                            numericInput(inputId = "deltaProp",
                                         label = "Delta",
                                         value = NULL,
                                         step = 0.005))
                          ),
                        mainPanel(
                          actionButton(inputId = "Prop", label =  "Results"),
                          br(),
                          htmlOutput("proportion"))
                        ) # sidebarLayout
                      ), # Comparing proportion


# PAGE COMPARING MEANS --------------------------------------------------------------------------------------------------

             tabPanel(title = "COMPARING MEANS",
                      fluidRow(column(width = 12,
                                      h3("COMPARING MEANS"))),
                      br(),
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(inputId = "hypMean",
                                      label = "Hypothesis",
                                      choices = c(" ","superiority", "non-inferiority")), 
                          #when an item is selected, the next appear
                          numericInput(inputId = "powerMean",
                                       label = "Power (%)",
                                       value = NULL),
                          numericInput(inputId = "alphaMean",
                                       label = "Alpha (%)",
                                       value = NULL,
                                       step = 0.5),
                          conditionalPanel(
                            condition = "input.hypMean == 'superiority' || input.hypMean == 'non-inferiority'",
                            h5("Mean expected in the control group :"),
                            numericInput(inputId = "obsMean",
                                         label = "Control",
                                         value = NULL,
                                         step = 0.005)),
                          conditionalPanel(
                            condition = "input.hypMean == 'superiority'",
                            h5("Mean expected in the experimental group :"),
                            numericInput(inputId = "prevMean",
                                         label = "Experimental",
                                         value = NULL,
                                         step = 0.005),
                            selectInput(inputId = "sidetestMean",
                                        label = "test",
                                        choices = c("two-sided", "one-sided"))),
                          numericInput(inputId = "sigmaMean",
                                       label = "Sigma",
                                       value = NULL,
                                       step = 0.005),
                          conditionalPanel(
                            condition = "input.hypMean == 'non-inferiority'",
                            h5("Absolute margin :"),
                            numericInput(inputId = "deltaMean",
                                         label = "Delta",
                                         value = NULL,
                                         step = 0,005))
                          ), # sidebar panel
                        mainPanel(
                          actionButton("Mean", "Results"),
                          br(),
                          htmlOutput("mean"))
                        ) # sidebarLayout
                      ), # Comparing means


# PAGE BINARY EVENT PREDICTION ------------------------------------------------------------------------------------------
            tabPanel(title = "BINARY EVENT PREDICTION",
                     fluidRow(column(width = 12,
                                     h3("BINARY EVENT PREDICTION"))),
                     br(),
                     sidebarLayout(
                       sidebarPanel(
                         numericInput(inputId = "predictorsToTest",
                                      label = "Number of potential predictors",
                                      value = NULL,
                                      step = 1),
                         numericInput(inputId = "shrinkageExpected",
                                      label = "Expected Shrinkage",
                                      value = NULL,
                                      step = 0.1),
                         numericInput(inputId = "R2",
                                      label = "anticipated R2 : expected predictive capacities",
                                      value = NULL,
                                      step = 0.01)
                         ),
                       mainPanel(
                         actionButton("Pred", "Results"),
                         br(),
                         htmlOutput("prediction"))
                       )
                     ) # BINARY EVENT PREDICTION

)
)
)





#########################################################################################################################
#################################################   SERVER   ############################################################
#########################################################################################################################

server <- function(input, output) {
  
  # convert to numeric value --------------------------------------
  # "one-sided" <- 1
  # "tow-sided" <- 2
  stMean <- reactive( if (reactive(input$sidetestMean)()=="one-sided") 1 else 2 )
  
  stProp <- reactive( if (reactive(input$sidetestProp)()=="one-sided") 1 else 2 )

  
  # sample size result of the computation -------------------------
  # MEAN
  resMean <- eventReactive(input$Mean,{
    if(reactive(input$hypMean)()=='superiority')
      epi.sscompc(N = NA, treat = input$prevMean, control = input$obsMean, 
                  sigma = input$sigmaMean, n = NA, power = input$powerMean/100, 
                  r = 1, design = 1, sided.test = stMean(), conf.level = 1-(input$alphaMean/100))$n.treat
    else
      epi.ssninfc(treat = input$obsMean, control = input$obsMean, sigma = input$sigmaMean, 
                  delta = input$deltaMean, n = NA, power = input$powerMean/100, alpha = input$alphaMean/100, r = 1)$n.treat
      })
    
      
  # PROPORTION
  resProp <- eventReactive(input$Prop,{
    if(reactive(input$hypProp)()=='superiority')
      epi.sscohortc(N = NA, irexp1 = input$prevProp/100, irexp0 = input$obsProp/100, pexp = NA, n = NA, 
                    power = input$powerProp/100, r = 1, design = 1, sided.test = stProp(), 
                    finite.correction = FALSE, nfractional = FALSE, conf.level = 1-(input$alphaProp/100))$n.exp1
    else
      epi.ssninfb(treat = input$obsProp/100, control = input$obsProp/100, delta = input$deltaProp/100, 
                  n = NA, r = 1, power = input$powerProp/100, alpha = input$alphaProp/100)$n.treat
    })
  
  # PREDICTIVE
  resPred <- eventReactive(input$Pred,{
    ceiling(input$predictorsToTest/((input$shrinkageExpected-1)*log(1-input$R2/input$shrinkageExpected)))
  })
  
  # reactive example sentence -------------------------
  a <- eventReactive(input$Mean,{
    if(reactive(input$hypMean)()=='superiority')
    paste0("This sample size is for a randomised controlled superiority trial in two parallel groups 
           experimental treatment versus control treatment with balanced randomisation (ratio 1 :1) for a binary 
           endpoint. The mean of the criteria is ",input$prevMean," with experimental treatment compared to
           ",input$obsMean," with control treatment. In order to highlight this absolute difference of 
           ",abs(input$prevMean-input$obsMean),", with a standard deviation of ",input$sigmaMean,", with a "
           ,input$sidetestMean," alpha risk of ",input$alphaMean,"% and a power of ",input$powerMean,"%,
           the needed sample size is ",resMean(), " patients in each group.")
    else
      paste0("This sample size for a randomised controlled non-inferiority trial in two parallel groups experimental 
             treatment versus control treatment with balanced randomisation (ratio 1 :1) for a binary endpoint. 
             The mean of the criteria is ",input$obsMean," with treatment A. Assuming an absolute non-inferiority margin 
             of ",input$deltaMean,", with a standard deviation of ",input$sigmaMean,", with a one-sided alpha risk of "
             ,input$alphaMean,"% and a power of ",input$powerMean,"%, the needed sample size is ",resMean(), "
              patients in each group.")
      
      })
  
  b <- eventReactive(input$Prop,{
    if(reactive(input$hypProp)()=='superiority')
    paste0("This sample size is for a randomised controlled superiority trial in two parallel groups experimental 
           treatment versus control treatment with balanced randomisation (ratio 1 :1) for a binary endpoint. The 
           proportion of patient with the criteria is ",input$prevProp,"% with experimental treatment compared to 
           ",input$obsProp,"% with control treatment. In order to highlight this absolute difference of 
           ",abs(input$prevProp-input$obsProp),"%, with a ",input$sidedtestProp," alpha risk of ",input$alphaProp,"% and 
           a power of ",input$powerProp,"%, the needed sample size is ",resProp(), " patients in each group.")
    else
      paste0("This sample size is for a randomised controlled non-inferiority trial in two parallel groups experimental 
             treatment versus control treatment with balanced randomisation (ratio 1 :1) for a binary endpoint. 
             The proportion of patients with the criteria is ",input$prevProp,"% with the control treatment. Assuming an 
             absolute non-inferiority margin of ",input$deltaProp,"%, with a one-sided alpha risk of 5% and a power 
             of ",input$powerProp,"%, the needed sample size is ",resProp(), " patients in each group.")
      })
  
  c <- eventReactive(input$Pred,{
    paste0("This sample size is for developing a logistic regression model based on up to ",input$input$predictorsToTest," candidate 
           predictors, with an anticipated R2 of at least ",input$R2,", and to target an expected 
           shrinkage of ",input$shrinkageExpected,"(equation 11 in Riley et al. Statistics in Medicine. 2019;38:1276–1296).")
    })
  
  
  # display the result -------------------------
  output$mean <- renderText(paste("<p>The needed sample size is <b>",resMean(), "</b> patients in each group. </p>", a()))
  output$proportion <- renderText(paste0("<p>The needed sample size is <b>",resProp(), "</b> patients in each group. </p>", b()))
  output$prediction <- renderText(paste0("<p>The needed sample size is <b>",resPred(), "</b> patients. </p>", c()))
}

# Run the application
shinyApp(ui = ui, server = server)
