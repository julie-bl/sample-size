library(shiny)
library(shinydashboard)
library(epiR)
library(shinythemes)
library(shinyjs)
library(drcarlate)
library(rpact)


se <- function(width, alpha) # The standard error associated with the 1-alpha confidence interval
{
  fun <- function(x) { exp( qnorm(1-alpha/2, mean=0, sd=1) * x ) - exp(-1* qnorm(1-alpha/2, mean=0, sd=1) * x ) - width } 
  return(uniroot(fun, lower = 0.001, upper = 100)$root)
} 

size.calib <- function(p0, width, alpha) # the minimum sample size to achieve this precision
{   
  (1-p0) / ((p0 * se(width=width, alpha=alpha)**2 ))
}


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
                  h1("SAMPLE SIZE CALCULATOR"))
           ),
  fluidRow(
    navbarPage(title = icon("users",class ="fa-solid fa-users"),
# DESCRIBE PROPORTION ---------------------------------------------------------------------------------------------------
             tabPanel(title = "DESCRIBE PROPORTION",
                      fluidRow(column(width = 12,
                                      h3("DESCRIBE PROPORTION"))),
                      br(),
                      sidebarLayout(
                        sidebarPanel(
                          numericInput(inputId = "descExpProp",
                                       label = "Expected proportion (%)",
                                       value = NULL,
                                       max = 100,
                                       min = 0),
                          numericInput(inputId = "widthDesc",
                                       label = "Confidence interval width (%)",
                                       value = NULL,
                                       max = 100,
                                       min = 0),
                          numericInput(inputId = "descAlpha",
                                       label = "Type I error rate, alpha (%)",
                                       value = 5)
                        ),
                        mainPanel(
                          actionButton(inputId = "Desc", label =  "Results"),
                          br(),
                          htmlOutput("description"))
                      )
             ),

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
                          selectInput(inputId = "meanDesign",
                                      label = "Design",
                                      choices = c("","No intermediate analysis", "Sequential design")),
                          numericInput(inputId = "powerProp",
                                       label = "Power (%)",
                                       value = 80,
                                       max = 100,
                                       min = 0),
                          numericInput(inputId = "alphaProp",
                                       label = "Type I error rate, alpha (%)",
                                       value = 5,
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
                          selectInput(inputId = "meanDesign",
                                      label = "Design",
                                      choices = c("","No intermediate analysis", "Sequential design")),
                          numericInput(inputId = "powerMean",
                                       label = "Power (%)",
                                       value = 80),
                          numericInput(inputId = "alphaMean",
                                       label = "Type I error rate, alpha (%)",
                                       value = 5,
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
                         selectInput(inputId = "typePred",
                                     label = "Design",
                                     choices = c(" ","Construction of predictive model", "External validation")), 
                         conditionalPanel(
                           condition = "input.typePred == 'Construction of predictive model'",
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
                         conditionalPanel(
                           condition = "input.typePred == 'External validation'",
                           numericInput(inputId = "P0",
                                        label = "Expected proportion of events in the validation study (%)",
                                        value = NULL,
                                        step = 1),
                           numericInput(inputId = "alphaExtVal",
                                        label = "Type I error rate, alpha (%)",
                                        value = 5,
                                        step = 0.5),
                           #The total length of the confidence of O/E
                           numericInput(inputId = "widthExtVal",
                                        label = "Confidence interval width",
                                        value = NULL,
                                        step = 0.1)
                           )
                        ),
                       mainPanel(
                         actionButton("Pred", "Results"),
                         br(),
                         htmlOutput("prediction"))
                      )
                    ), # BINARY EVENT PREDICTION

# PAGE COMPARING TWO ROC CURVES -----------------------------------------------------------------------------------------
            tabPanel(title = "COMPARING TWO ROC CURVES",
                     fluidRow(column(width = 12,
                                     h3("COMPARING TWO ROC CURVES"))),
                     br(),
                     sidebarLayout(
                       sidebarPanel(
                         numericInput(inputId = "AUC1",
                                      label = "First diagnostic test AUC",
                                      value = NULL,
                                      step = 0.001),
                         numericInput(inputId = "AUC2",
                                      label = "Second diagnostic test AUC",
                                      value = NULL,
                                      step = 0.001),
                         numericInput(inputId = "alphaAUC",
                                      label = "Type I error rate, alpha (%)",
                                      value = 5,
                                      step = 0.5),
                         numericInput(inputId = "powerAUC",
                                      label = "Power (%)",
                                      value = 80,
                                      min = 0,
                                      max = 100)
                       ),
                       mainPanel(
                         actionButton("AUC", "Results"),
                         br(),
                         htmlOutput("AUC"))
                       )
                     )
    )
  )
)





##################################################################################################################################
##########################################################   SERVER   ############################################################
##################################################################################################################################

server <- function(input, output) {
  
  # convert to numeric value -----------------------------------------------------------------------------------------------------
  # "one-sided" <- 1
  # "tow-sided" <- 2
  stMean <- reactive( if (reactive(input$sidetestMean)()=="one-sided") 1 else 2 )
  
  stProp <- reactive( if (reactive(input$sidetestProp)()=="one-sided") 1 else 2 )

  
  # sample size result of the computation ----------------------------------------------------------------------------------------
  
  #DESCRITPION -------------------------------------------------------------------------------------------------------------------
  resDesc <- eventReactive(input$Desc,{
    sampleSizeDesc <- function(p=input$descExpProp/100, alpha=input$descAlpha/100, width=input$widthDesc/100){
      Z <- qnorm(1-alpha/2)
      (((2*Z)**2)*(p*(1-p)))/(width**2)
    }
    
    ceiling(sampleSizeDesc())
  })
  
  # MEAN -------------------------------------------------------------------------------------------------------------------------
  resMean <- eventReactive(input$Mean,{
    if(reactive(input$hypMean)()=='superiority'){ # MEAN SUPERIORITY -------------------------------------------------------------
      if(reactive(input$meanDesign)()=='No intermediate analysis') # MEAN SUPERIORITY NO INTERMEDIATE DESIGN ----------------------
        epi.sscompc(N = NA, treat = input$prevMean, control = input$obsMean, 
                    sigma = input$sigmaMean, n = NA, power = input$powerMean/100, 
                    r = 1, design = 1, sided.test = stMean(), conf.level = 1-(input$alphaMean/100))$n.treat
      else # MEAN SUPERIORITY SEQUENTIAL DESIGN ----------------------------------------------------------------------------------
        design <- getDesignGroupSequential(typeOfDesign = "OF", informationRates = c(1/3, 2/3, 1),
                                           alpha = input$alpharMean/100, beta = 1-input$powerMean/100, sided = stMean())
        designPlan <- getSampleSizeMeans(design, alternative = abs(input$prevMean-input$obsMean), stDev = input$sigmaMean,
                                       allocationRatioPlanned = 1)
        designPlan
    }else{ # MEAN NON-INFERIORITY -----------------------------------------------------------------------------------------------
      if(reactive(input$meanDesign)()=='No intermediate analysis') # MEAN NON-INFERIORITY NO INTERMEDIATE DESIGN -----------------
        epi.ssninfc(treat = input$obsMean, control = input$obsMean, sigma = input$sigmaMean, 
                    delta = input$deltaMean, n = NA, power = input$powerMean/100, alpha = input$alphaMean/100, r = 1)$n.treat
      else # MEAN NON-INFERIORITY SEQUENTIAL DESIGN -----------------------------------------------------------------------------
        design <- getDesignGroupSequential(typeOfDesign = "OF", informationRates = c(1/3, 2/3, 1),
                                           alpha = input$alphaMean/100, beta = 1-input$powerMean/100, sided = 1)
        designPlan <- getSampleSizeMeans(design, alternative = 0, stDev = input$sigmaMean,
                                       allocationRatioPlanned = 1)
        designPlan
      }
    })
    
      
  # PROPORTION ------------------------------------------------------------------------------------------------------------------
  resProp <- eventReactive(input$Prop,{
    if(reactive(input$hypProp)()=='superiority')
      epi.sscohortc(N = NA, irexp1 = input$prevProp/100, irexp0 = input$obsProp/100, pexp = NA, n = NA, 
                    power = input$powerProp/100, r = 1, design = 1, sided.test = stProp(), 
                    finite.correction = FALSE, nfractional = FALSE, conf.level = 1-(input$alphaProp/100))$n.exp1
    else
      epi.ssninfb(treat = input$obsProp/100, control = input$obsProp/100, delta = input$deltaProp/100, 
                  n = NA, r = 1, power = input$powerProp/100, alpha = input$alphaProp/100)$n.treat
    })
  
  # PREDICTIVE ------------------------------------------------------------------------------------------------------------------
  resPred <- eventReactive(input$Pred,{
    if(reactive(input$typePred)()=='Construction of predictive model')
      ceiling(input$predictorsToTest/((input$shrinkageExpected-1)*log(1-input$R2/input$shrinkageExpected)))
    else
      ceiling(size.calib(p0=input$P0/100, width = input$widthExtVal/100, alpha = input$alphaExtVal/100))
  })
  
  # AUC -------------------------------------------------------------------------------------------------------------------------
  resAUC <- eventReactive(input$AUC,{
    varAUC <- function(auc){
      a <- norminv(auc)*1.414
      (0.0099*exp(-(a**2)/2))*(6*(a**2)+16)
    }
    
    compAUC <- function(auc1=input$AUC1, auc2=input$AUC2, alpha=input$alphaAUC/100, power=input$powerAUC/100){
      ((qnorm(1-alpha/2)*sqrt(2*varAUC((auc1+auc2)/2))+qnorm(power)*sqrt(varAUC(auc1)+varAUC(auc2)))**2)/((auc2-auc1)**2)
    }
    
    ceiling(compAUC())
  })
  
  # reactive example sentence ---------------------------------------------------------------------------------------------------
  z <- eventReactive(input$Desc,{
    paste0("This sample size is for a binary endpoint descriptive study. In order to demonstrate the expected proportion
           of event of 35% with a precision define by a 10% width confidence interval and a 5% two-sided type I error
           rate, the minimum sample size needed is ",resDesc()," patients")
  })
  
  a <- eventReactive(input$Mean,{
    if(reactive(input$hypMean)()=='superiority')
      if(reactive(input$meanDesign)()=='')
      paste0("This sample size is for a randomised controlled superiority trial in two parallel groups 
           experimental treatment versus control treatment with balanced randomisation (ratio 1 :1) for a continuous 
           endpoint. The mean of the criteria is ",input$prevMean," with experimental treatment compared to
           ",input$obsMean," with control treatment. In order to highlight this absolute difference of 
           ",abs(input$prevMean-input$obsMean),", with a standard deviation of ",input$sigmaMean,", with a "
           ,input$sidetestMean," alpha risk of ",input$alphaMean,"% and a power of ",input$powerMean,"%,
           the needed sample size is ",resMean(), " patients in each group.")
      else
        paste0("This sample size is for a RCT with an expected mean of ",input$prevMean," units in patients in the 
               experimental arm versus ",input$obsMean," units in the control arm. In order to demonstrate such a 
               difference of ",abs(input$prevMean-input$obsMean)," units, with a standard deviation of ",input$sigmaMean,"
               , a ",input$alphaMean,"% ",input$sidetestMean," type I error rate and a power of ",input$powerMean,"%, 
               the final analysis should be carried out on ",ceiling(resMean$numberOfSubjects[3])," patients 
               (",ceiling(resMean$numberOfSubjects[3]/2)," patients per group). The first and second intermediate 
               analyses would be performed on ",ceiling(resMean$numberOfSubjects[1])," and 316 patients respectively, i.e. 33% and 66% of the maximum number of included 
               patients if their is no decision of stopping the study")
    else
      if(reactive(input$meanDesign)()=='')
      paste0("This sample size for a randomised controlled non-inferiority trial in two parallel groups experimental 
             treatment versus control treatment with balanced randomisation (ratio 1 :1) for a continuous endpoint. 
             The mean of the criteria is ",input$obsMean," with treatment A. Assuming an absolute non-inferiority margin 
             of ",input$deltaMean,", with a standard deviation of ",input$sigmaMean,", with a one-sided alpha risk of "
             ,input$alphaMean,"% and a power of ",input$powerMean,"%, the needed sample size is ",resMean(), "
              patients in each group.")
    else
      paste0("")
      
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
    if(reactive(input$typePred)()=='Construction of predictive model')
    paste0("This sample size is for developing a logistic regression model based on up to ",input$predictorsToTest," candidate 
           predictors, with an anticipated R2 of at least ",input$R2,", and to target an expected 
           shrinkage of ",input$shrinkageExpected," (equation 11 in Riley et al. Statistics in Medicine. 2019;38:1276–1296).")
    else
      paste0("This sample size is for external validation of a logistic regression model based with an expected outcome 
             event proportions of ",input$P0,"%, with a alpha risk at ",input$alphaExtVal,"% and with a target confidence 
             interval width of ",input$widthExtVal,"% (Riley et al.  Statistics in Medicine. 2021;19:4230-4251).")
    })
  
  d <- eventReactive(input$AUC,{
    paste0("This sample size is for comparing accuracy of two diagnostic tests assuming a detection of ",(input$AUC2-input$AUC1)*100,"
           % difference in estimating two independent diagnostic systems (AUC1 = ",input$AUC1," and AUC2 = ",input$AUC2,") 
           with ",(100-input$alphaAUC),"% confidence and ",input$powerAUC,"% power.")
  })  
  
  # display the result ----------------------------------------------------------------------------------------------------------
  output$description <- renderText(paste("<p>The needed sample size is <b>",resDesc(), "</b> patients. </p>", z()))
  output$mean <- renderText(paste("<p>The needed sample size is <b>",resMean(), "</b> patients in each group. </p>", a()))
  output$proportion <- renderText(paste0("<p>The needed sample size is <b>",resProp(), "</b> patients in each group. </p>", b()))
  output$prediction <- renderText(paste0("<p>The needed sample size is <b>",resPred(), "</b> patients. </p>", c()))
  output$AUC <- renderText(paste0("<p>The needed sample size is <b>",resAUC(),"</b> patients.</p>", d()))
}

# Run the application
shinyApp(ui = ui, server = server)
