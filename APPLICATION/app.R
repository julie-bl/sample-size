library(shiny)
library(shinydashboard)
library(epiR)
library(shinythemes)
library(shinyjs)
library(drcarlate)
library(rpact)
library(shinycssloaders)
library(shinyjs)


se <- function(width, alpha) # The standard error associated with the 1-alpha confidence interval
{
  fun <- function(x) { exp( qnorm(1-alpha/2, mean=0, sd=1) * x ) - exp(-1* qnorm(1-alpha/2, mean=0, sd=1) * x ) - width } 
  return(uniroot(fun, lower = 0.001, upper = 100)$root)
} 

size.calib <- function(p0, width, alpha) # the minimum sample size to achieve this precision
{   
  (1-p0) / ((p0 * se(width=width, alpha=alpha)**2 ))
}



ui <- dashboardPage(
  dashboardHeader(title = "SAMPLE SIZE CALCULATOR",
                  titleWidth = "100%"), 
  dashboardSidebar(
    sidebarMenu(
      id = "sidebarID",
      menuItem("DESCRIBE A POPULATION", tabName = "dp", id = "DPid", expandedName = "DP",
               radioButtons(inputId = "typeDP",
                            label = "Design",
                            choices = "PROPORTION ENDPOINT")),
      hidden(menuItem("hiddenChartsDP", tabName = "hiddenChartsDP")),
      menuItem("COMPARING MEANS",tabName = "comparingmean",id = "CMid",expandedName = "COMPARINGMEAN",
               radioButtons(inputId = "hypMean",
                            label = "Hypothesis",
                            choices = c("superiority", "non-inferiority")), 
               radioButtons(inputId = "meanDesign",
                            label = "Design",
                            choices = c("No intermediate analysis", "Sequential design"))),
      hidden(menuItem("hiddenChartsMean", tabName = "hiddenChartsMean")),
      menuItem("COMPARING PROPORTION",tabName = "comparingprop",id = "CPid",expandedName = "COMPARINGPROP",
               radioButtons(inputId = "hypProp",
                            label = "Hypothesis",
                            choices = c("superiority", "non-inferiority")), 
               radioButtons(inputId = "propDesign",
                            label = "Design",
                            choices = c("No intermediate analysis", "Sequential design"))),
      hidden(menuItem("hiddenChartsProp", tabName = "hiddenChartsProp")),
      menuItem("PREDICTING A PROPORTION", tabName = "bep", id = "BEPid", expandedName = "BEP",
               radioButtons(inputId = "typePredBEP",
                            label = "Design",
                            choices = c("Construction of predictive model", "External validation"))),
      hidden(menuItem("hiddenChartsBEP", tabName = "hiddenChartsBEP")),
      menuItem("COMPARING TWO ROC CURVES", tabName = "CTRC")
    )
  ),
  dashboardBody(
    useShinyjs(),
    tabItems(
      
# DESCRIBE PROPORTION ---------------------------------------------------------------------------------------------------
tabItem(tabName = "hiddenChartsDP",
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
                         label = "Type I error rate (%)",
                         value = 5)
          ),
          mainPanel(
            box(
              htmlOutput("description")))
        )),
      
# PAGE COMPARING MEANS --------------------------------------------------------------------------------------------------
      tabItem(tabName = "hiddenChartsMean",
              fluidRow(column(width = 12,
                              h3("COMPARING MEANS"))),
              br(),
              sidebarLayout(
                sidebarPanel(
                  conditionalPanel(
                    conditionalPanel(
                      radioButtons(inputId = "sidetestMean",
                                   label = "Two sided test ?",
                                   choices = c("Yes", "No")),
                      condition = "input.hypMean == 'superiority'",
                      numericInput(inputId = "prevMean",
                                   label = "Experimental (%)",
                                   value = NULL,
                                   step = 0.005)),
                    condition = "input.hypMean == 'superiority' || input.hypMean == 'non-inferiority'",
                    numericInput(inputId = "obsMean",
                                 label = "Control (%)",
                                 value = NULL,
                                 step = 0.005)),
                  numericInput(inputId = "powerMean",
                               label = "Power (%)",
                               value = 80),
                  numericInput(inputId = "alphaMean",
                               label = "Type I error rate (%)",
                               value = 5,
                               step = 0.5),
                  numericInput(inputId = "sigmaMean",
                               label = "Sigma",
                               value = NULL,
                               step = 0.005),
                  conditionalPanel(
                    condition = "input.hypMean == 'non-inferiority'",
                    numericInput(inputId = "deltaMean",
                                 label = "Absolute margin",
                                 value = NULL,
                                 step = 0,005)),
                  conditionalPanel(
                    condition = "input.meanDesign == 'Sequential design'",
                    numericInput(inputId = "meanSlice",
                                 label = "Number of planned intermediate analysis",
                                 value = 2))
                ), # sidebar panel
                mainPanel(
                  htmlOutput("mean"))
              )
      ),

# PAGE COMPARING PROPORTIONS --------------------------------------------------------------------------------------------
      tabItem(tabName = "hiddenChartsProp",
               fluidRow(column(width = 12,
                               h3("COMPARING PROPORTIONS"))),
               br(),
               sidebarLayout(
                 sidebarPanel(
                   conditionalPanel(
                     radioButtons(inputId = "sidetestProp",
                                  label = "Two sided test ?",
                                  choices = c("Yes", "No")),
                     condition = "input.hypProp == 'superiority'",
                     numericInput(inputId = "prevProp",
                                  label = "Experimental (%)",
                                  value = NULL,
                                  step = 0.005)),
                   conditionalPanel(
                     condition = "input.hypProp == 'superiority' || input.hypProp == 'non-inferiority'",
                     numericInput(inputId = "obsProp",
                                  label = "Control (%)",
                                  value = NULL,
                                  step = 0.005)),
                   numericInput(inputId = "powerProp",
                                label = "Power (%)",
                                value = 80,
                                max = 100,
                                min = 0),
                   numericInput(inputId = "alphaProp",
                                label = "Type I error rate (%)",
                                value = 5,
                                max = 20,
                                min = 0,
                                step = 0.5),
                   conditionalPanel(
                     condition = "input.hypProp == 'non-inferiority'",
                     numericInput(inputId = "deltaProp",
                                  label = "Absolute margin (%)",
                                  value = NULL,
                                  step = 0.005)),
                   conditionalPanel(
                     condition = "input.propDesign == 'Sequential design'",
                     numericInput(inputId = "propSlice",
                                  label = "Number of planned intermediate analysis",
                                  value = 2))
                 ),
                 mainPanel(
                   htmlOutput("proportion"))
               ) # sidebarLayout
      ),# Comparing proportion

# PAGE BINARY EVENT PREDICTION ------------------------------------------------------------------------------------------
tabItem(tabName = "hiddenChartsBEP",
         fluidRow(column(width = 12,
                         h3("BINARY EVENT PREDICTION"))),
         br(),
         sidebarLayout(
           sidebarPanel(
             conditionalPanel(
               condition = "input.typePredBEP == 'Construction of predictive model'",
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
               condition = "input.typePredBEP == 'External validation'",
               numericInput(inputId = "P0",
                            label = "Expected proportion of events in the validation study (%)",
                            value = NULL,
                            step = 1),
               numericInput(inputId = "alphaExtVal",
                            label = "Type I error rate (%)",
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
tabItem(tabName = "CTRC",
         fluidRow(column(width = 12,
                         h3("COMPARING TWO ROC CURVES"))),
         br(),
         sidebarLayout(
           sidebarPanel(
             numericInput(inputId = "AUC1",
                          label = "First diagnostic test AUC",
                          value = NULL,
                          min = 0.5,
                          step = 0.001),
             numericInput(inputId = "AUC2",
                          label = "Second diagnostic test AUC",
                          value = NULL,
                          min = 0.5,
                          step = 0.001),
             numericInput(inputId = "alphaAUC",
                          label = "Type I error rate (%)",
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

server <- function(input, output, session) {
  
  # for the sidbar --------------------------------------------------------
  observeEvent(input$sidebarItemExpanded, {
    if(input$sidebarItemExpanded == "DP"){
      updateTabItems(session, "sidebarID", selected = "hiddenChartsDP")
    }
    if(input$sidebarItemExpanded == "COMPARINGMEAN"){
      updateTabItems(session, "sidebarID", selected = "hiddenChartsMean")
    }
    if(input$sidebarItemExpanded == "COMPARINGPROP"){
      updateTabItems(session, "sidebarID", selected = "hiddenChartsProp")
    }
    if(input$sidebarItemExpanded == "BEP"){
      updateTabItems(session, "sidebarID", selected = "hiddenChartsBEP")
    }
  })
  #------------------------------------------------------------------------
  
  sampleSizeDesc <- function(p,alpha,width){
    Z <- qnorm(1-alpha/2)
    (((2*Z)**2)*(p*(1-p)))/(width**2)
  }
  
  # convert to numeric value -----------------------------------------------------------------------------------------------------
  # "one-sided" <- 1
  # "tow-sided" <- 2
  stMean <- reactive( if (reactive(input$sidetestMean)()=="No") 1 else 2 )
  printStMean <- reactive( if (reactive(input$sidetestMean)()=="No") "one-sided" else "two-sided" )
  
  stProp <- reactive( if (reactive(input$sidetestProp)()=="No") 1 else 2 )
  printStProp <- reactive( if (reactive(input$sidetestProp)()=="No") "one-sided" else "two-sided" )
  
  
  # sample size result of the computation ----------------------------------------------------------------------------------------
  
  #DESCRITPION -------------------------------------------------------------------------------------------------------------------
  resDesc <- reactive(
    ceiling(sampleSizeDesc(p=input$descExpProp/100, alpha=input$descAlpha/100, width=input$widthDesc/100))
  )
  
  # MEAN -------------------------------------------------------------------------------------------------------------------------
  resMean <- reactive(
    if(reactive(input$hypMean)()=='superiority'){ # MEAN SUPERIORITY -------------------------------------------------------------
      if(reactive(input$meanDesign)()=='No intermediate analysis'){ # MEAN SUPERIORITY NO INTERMEDIATE DESIGN --------------------
        epi.sscompc(N = NA, treat = input$prevMean, control = input$obsMean, 
                    sigma = input$sigmaMean, n = NA, power = input$powerMean/100, 
                    r = 1, design = 1, sided.test = stMean(), conf.level = 1-(input$alphaMean/100))$n.treat
      }
      else{# MEAN SUPERIORITY SEQUENTIAL DESIGN --------------------------------------------------------------------------------
        design <- getDesignGroupSequential(typeOfDesign = "OF", informationRates = seq(from=1/(input$meanSlice+1), to=1, by=1/(input$meanSlice+1)),
                                         alpha = input$alphaMean/100, beta = 1-input$powerMean/100, sided = stMean())
        designPlan <- getSampleSizeMeans(design, alternative = abs(input$prevMean-input$obsMean), stDev = input$sigmaMean,
                                         allocationRatioPlanned = 1)
        ceiling(designPlan$numberOfSubjects[input$meanSlice+1])
      } 

    }else{ # MEAN NON-INFERIORITY -----------------------------------------------------------------------------------------------
      if(reactive(input$meanDesign)()=='No intermediate analysis'){# MEAN NON-INFERIORITY NO INTERMEDIATE DESIGN ----------------
        epi.ssninfc(treat = input$obsMean, control = input$obsMean, sigma = input$sigmaMean,
                    delta = input$deltaMean, n = NA, power = input$powerMean/100, alpha = input$alphaMean/100, r = 1)$n.treat
      } 
      else{# MEAN NON-INFERIORITY SEQUENTIAL DESIGN -----------------------------------------------------------------------------
        design <- getDesignGroupSequential(typeOfDesign = "OF", informationRates = seq(from=1/(input$meanSlice+1), to=1, by=1/(input$meanSlice+1)),
                                           alpha = input$alphaMean/100, beta = 1-input$powerMean/100, sided = 1)
        designPlan <- getSampleSizeMeans(design, alternative = 0, stDev = input$sigmaMean,
                                         allocationRatioPlanned = 1, thetaH0 = -input$deltaMean)
        ceiling(designPlan$numberOfSubjects[input$meanSlice+1]/2)
      } 
      
    })
  
  
  # PROPORTION ------------------------------------------------------------------------------------------------------------------
  resProp <- reactive(
    if(reactive(input$hypProp)()=='superiority'){ # PROPORTION SUPERIORITY ------------------------------------------------------
      if(reactive(input$propDesign)()=='No intermediate analysis'){# PROPORTION SUPERIORITY NO INTERMEDIATE DESIGN -------------
        epi.sscohortc(N = NA, irexp1 = input$prevProp/100, irexp0 = input$obsProp/100, pexp = NA, n = NA, 
                      power = input$powerProp/100, r = 1, design = 1, sided.test = stProp(), 
                      finite.correction = FALSE, nfractional = FALSE, conf.level = 1-(input$alphaProp/100))$n.exp1
      } 
      else{# PROPORTION SUPERIORITY SEQUENTIAL DESIGN -------------------------------------------------------------------------
        design <- getDesignGroupSequential(typeOfDesign = "OF", informationRates = seq(from=1/(input$propSlice+1), to=1, by=1/(input$propSlice+1)),
                                           alpha = input$alphaProp/100, beta = 1-input$powerProp/100, sided = stProp())
        designPlan <- getSampleSizeRates(design,  pi1 = input$prevProp/100, pi2 = input$obsProp/100,
                                         allocationRatioPlanned = 1)
        ceiling(designPlan$numberOfSubjects[input$propSlice+1]/2)
      }
      
    }else{ # PROPORTION NON-INFERIORITY -----------------------------------------------------------------------------------------
      if(reactive(input$propDesign)()=='No intermediate analysis'){# PROPORTION NON-INFERIORITY NO INTERMEDIATE DESIGN ---------
        epi.ssninfb(treat = input$obsProp/100, control = input$obsProp/100, delta = input$deltaProp/100, 
                    n = NA, r = 1, power = input$powerProp/100, alpha = input$alphaProp/100)$n.treat
      } 
      else{# PROPORTION NON-INFERIORITY SEQUENTIAL DESIGN ---------------------------------------------------------------------
        design <- getDesignGroupSequential(typeOfDesign = "OF", informationRates = seq(from=1/(input$propSlice+1), to=1, by=1/(input$propSlice+1)),
                                           alpha = input$alphaProp/100, beta = 1-input$powerProp/100, sided = 1)
        designPlan <- getSampleSizeRates(design, pi1 = input$obsProp/100, pi2 = input$obsProp/100, thetaH0 = input$deltaProp/100)
        ceiling(designPlan$numberOfSubjects[input$propSlice+1]/2)
      } 
          
        
    })
  
  # PREDICTIVE ------------------------------------------------------------------------------------------------------------------
  resPred <- eventReactive(input$Pred,{
    if(reactive(input$typePredBEP)()=='Construction of predictive model')
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
  z <- reactive(
    paste0("This sample size is for a binary endpoint descriptive study. In order to demonstrate the expected proportion
           of event of ",input$descExpProp,"% with a precision define by a ",input$widthDesc,"% width confidence interval 
           and a ",input$descAlpha,"% two-sided type I error rate, the minimum sample size needed is ",resDesc()," patients")
  )
  
  
  
  a <- reactive(
    if(reactive(input$hypMean)()=='superiority'){
      if(reactive(input$meanDesign)()=='No intermediate analysis')
        paste0("This sample size is for a randomised controlled superiority trial in two parallel groups 
               experimental treatment versus control treatment with balanced randomisation (ratio 1 :1) for a continuous 
               endpoint. The mean of the criteria is ",input$prevMean," with experimental treatment compared to
               ",input$obsMean," with control treatment. In order to highlight this absolute difference of 
               ",abs(input$prevMean-input$obsMean),", with a standard deviation of ",input$sigmaMean,", with a "
               ,printStMean()," alpha risk of ",input$alphaMean,"% and a power of ",input$powerMean,"%,
               the needed sample size is ",resMean(), " patients in each group (i.e., a total of ",resMean()*2," patients).")
      else
        paste0("This sample size is for a RCT with an expected mean of ",input$prevMean," units in patients in the 
               experimental arm versus ",input$obsMean," units in the control arm. In order to demonstrate such a 
               difference of ",abs(input$prevMean-input$obsMean)," units, with a standard deviation of ",input$sigmaMean,"
               , a ",input$alphaMean,"% ",printStMean()," type I error rate and a power of ",input$powerMean,"%, 
               the final analysis should be carried out on ",resMean()*2," patients (",resMean()," patients per group). 
               Intermediates analyses would be performed on ",for(i in input$meanSlice+1){ceiling(resMean()*i/(input$meanSlice+1))}," 
               and patients respectively, i.e. ",list(round(seq(from=1/(input$meanSlice+1), to =1, by=1/(input$meanSlice+1)),digits=2)),"% 
               of the maximum number of included patients if their is no decision of stopping the study")
    }else{
      if(reactive(input$meanDesign)()=='No intermediate analysis')
        paste0("This sample size for a randomised controlled non-inferiority trial in two parallel groups experimental 
               treatment versus control treatment with balanced randomisation (ratio 1 :1) for a continuous endpoint. 
               The mean of the criteria is ",input$obsMean," with treatment A. Assuming an absolute non-inferiority margin 
               of ",input$deltaMean,", with a standard deviation of ",input$sigmaMean,", with a one-sided alpha risk of "
               ,input$alphaMean,"% and a power of ",input$powerMean,"%, the needed sample size is ",resMean(), "
               patients in each group (i.e., a total of ",resMean()*2," patients).")
      else
        paste0("This sample size is for a randomised controlled non-inferiority trial in two parallel groups experimental 
               treatment versus control treatment with balanced randomisation (ratio 1 :1) for a continuous endpoint. Assuming 
               an absolute non-inferiority margin of ",input$deltaMean,", with a standard deviation of ",input$sigmaMean,", 
               with a one-sided alpha risk of ",input$alphaMean,"% and a power of ",input$powerMean,"%, the final analysis 
               should be carried out on ",resMean()," patients (",ceiling(resMean()/2)," patients per group).The first and 
               second intermediate analyses would be performed on ",ceiling(resMean()/(input$meanSlice+1))," and 316 patients 
               respectively, i.e. ",list(round(seq(from=1/(input$meanSlice+1), to =1, by=1/(input$meanSlice+1)),digits=2)),"% 
               of the maximum number of included patients if their is no decision of stopping the study")
      
    })
  
  
  
  b <- reactive(
    if(reactive(input$hypProp)()=='superiority'){
      if(reactive(input$propDesign)()=='No intermediate analysis')
        paste0("This sample size is for a randomised controlled superiority trial in two parallel groups experimental 
           treatment versus control treatment with balanced randomisation (ratio 1 :1) for a binary endpoint. The 
           proportion of patient with the criteria is ",input$prevProp,"% with experimental treatment compared to 
           ",input$obsProp,"% with control treatment. In order to highlight this absolute difference of 
           ",abs(input$prevProp-input$obsProp),"%, with a ",printStProp()," alpha risk of ",input$alphaProp,"% and 
           a power of ",input$powerProp,"%, the needed sample size is ",resProp(), " patients in each group.")
      else
        paste0("This sample size is for a RCT with an expected proportion of ",input$prevProp," units in patients in the 
               experimental arm versus ",input$obsProp," units in the control arm. In order to demonstrate such a 
               difference of ",abs(input$prevProp-input$obsProp)," units, with a standard deviation of ",input$sigmaProp,"
               , a ",input$alphaProp,"% ",printStProp()," type I error rate and a power of ",input$powerProp,"%, 
               the final analysis should be carried out on ",resProp()*2," patients 
               (",resProp()," patients per group). The first and second intermediate 
               analyses would be performed on ",ceiling(resProp()/(input$propSlice+1))," and 316 patients respectively, 
               i.e. ",list(round(seq(from=1/(input$propSlice+1), to =1, by=1/(input$propSlice+1)),digits=2)),"% of the 
               maximum number of included patients if their is no decision of stopping the study")
    }else{
      if(reactive(input$propDesign)()=='No intermediate analysis')
      paste0("This sample size is for a randomised controlled non-inferiority trial in two parallel groups experimental 
             treatment versus control treatment with balanced randomisation (ratio 1 :1) for a binary endpoint. 
             The expected percentage of events is ",input$obsProp,"% in patients in the control arm and no difference compared to the 
             experimental arm. Assuming an absolute non-inferiority margin of ",input$deltaProp,"%, the minimum sample 
             size per arm equals ",resProp()," (i.e., a total of ",resProp()*2," patients) to achieve a ",input$alphaProp,"% 
             one-sided type I error rate and a power of ",input$powerProp,"%.")
      else{
        paste0("This sample size is for a randomised controlled non-inferiority trial in two parallel groups experimental 
               treatment versus control treatment with balanced randomisation (ratio 1 :1) for a binary endpoint. 
               The expected percentage of events is ",input$obsProp,"% in patients in the control arm and no difference 
               compared to the experimental arm. Assuming an absolute non-inferiority margin of ",input$deltaProp,"%, 
               with a one-sided alpha risk of ",input$alphaProp,"% and a power of ",input$powerProp,"%, the final analysis 
               should be carried out on ",resProp()*2," patients(",resProp()," patients per group).The two intermediate 
               analyses would be performed on 242 and 484 patients respectively, i.e. 
               ",list(round(seq(from=1/(input$propSlice+1), to =1, by=1/(input$propSlice+1)),digits=2)),"% of the maximum 
               number of included patients if their is no decision of stopping the study")
      }
  })
  
  c <- eventReactive(input$Pred,{
    if(reactive(input$typePredBEP)()=='Construction of predictive model')
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
