## Initialisation of information for shiny demo
# 
# Built for presentation to Sydney Users of R Forum (SURF)
# Meetup group. Second generation of a similar app for the 
# Citizen Data Scientists Meetup group.
#
#

# Data ----
# Load the Boston data set
data( 'Boston', package='MASS' )
# Claim it
myData <- Boston
# Identify dependent ...
yName <- 'medv'
# ... and independent variables
xNames <- setdiff( names( Boston ), yName )

# Packages ----
# shiny for interactivity
library( shiny )
# shinythemes to configure via boostrap CSS
library( shinythemes )
# shinyBS for tooltip examples
library( shinyBS )
# tidyverse for general data wrangling
library( tidyverse )
# tidymodels for modelling options
# (and for me to practice!)
library( tidymodels )
# rmarkdown for documentation
library( rmarkdown )
# knitr for further compatibility and the kable function
library( knitr )
# kableExtra for knitr::kable configuration
# (this is in the rmarkdown templates)
library( kableExtra )

# Initial Values ----
# Initialise split parameters
SPLIT_SEED = 100
TRAIN_PERCENT = 75
# Initialise RF parameters
BASE_MTRY = 5
BASE_NTREE = 200
BASE_MIN_N = 10
# Initialise CV parameters
N_CV = 10

## Functions ----
# Fitting a model, including doing the "prep" requirement
fitModel <- function( modelObject
                      , recipeObject
                      , inputData ){
  # Prep the data according to the recipe
  prepData <- prep( recipeObject
                    , fresh=TRUE # For shiny - ensure fresh
                    , training=inputData 
  )
  
  # Fit the model
  modelOut <- modelObject %>%
    fit( formula( prepData ), juice( prepData ) )
  
  return( modelOut )
}

# Initialise reactiveValues ----
#
# These are used to control data flows throughout the algorithm
# I used reactives so if they're "obsolete" they'll trigger the 
# reactive functions.
#
# Data
# Set the seed for data splitting
set.seed( SPLIT_SEED )
splitData <- initial_split( myData )
# Put it all into a RV>..
dataRV <- reactiveValues( rawData=myData
                          , splitData=splitData
                          # Placeholders for data and metrics
                          , completeTrainData=NULL
                          , trainMetrics = NULL
                          , completeTestData=NULL
                          , testMetrics = NULL
                          , fullCVResults=NULL
)

# Models
# Default values
# Recipe
baseRecipe <- recipe( as.formula( 'medv~.' ), data=myData )
# Linear regression
baseLMModel <- linear_reg( mode='regression' ) %>%
  set_engine( 'lm' )
# Random forest
baseRFModel <- rand_forest( mode='regression'
                            , mtry=BASE_MTRY
                            , trees=BASE_NTREE
                            , min_n=BASE_MIN_N ) %>%
  set_engine( 'ranger' )

# Another RV for usage with tidymodels
modelRV <- reactiveValues( baseRecipe=baseRecipe
                           , lmRecipe=baseRecipe
                           , rfRecipe=baseRecipe
                           , lmPrep=prep( baseRecipe
                                          , training = splitData %>% 
                                            training() 
                           )
                           , rfPrep=prep( baseRecipe
                                          , training=splitData %>% 
                                            training()
                           )
                           , lmModel=baseLMModel
                           , rfModel=baseRFModel 
                           , lmFit=fitModel( baseLMModel
                                             , baseRecipe
                                             , splitData %>% training() )
                           , rfFit=fitModel( baseRFModel
                                             , baseRecipe
                                             , splitData %>% training() ) 
)


# Plots
# These will be passed to rmarkdown and shiny output functions
plotsRV <- reactiveValues( dataPlot=NULL
                           , lineTrainPlot=NULL
                           , residTrainPlot=NULL
                           , lineTestPlot=NULL
                           , residTestPlot=NULL
                           , cvBoxPlot=NULL )

# Documentation
docRV <- reactiveValues( makeSlides=FALSE # Triggered after rendering slides
                         , updateNumber=0 # Tracks filenames
                         # Initial write-up - changed by rendering
                         , fileName='./Documentation/initialWriteUp.html'
                         , reasons=NULL # Slide input from expert
)

## UI ----
uiSydney <- fluidPage(
  # Example theme - placeholder for customer/stakeholder 
  # preferences or CSS, etc.
  theme=shinytheme('darkly')
  # Application title
  , titlePanel("Demonstration of Real-time Feedback"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      # The sidebar is set up as a series of checkboxes and
      # conditional panels to configure individual parts, rather
      # than a very long set of inputs.
      ## Data splitting conditional ----
      h3('Input Data:' )
      , numericInput( inputId='splitSeed'
                      , label='Split seed:'
                      , value=SPLIT_SEED
                      , min=0, max=1000
                      , step=1 )
      , sliderInput( inputId='trainPercent'
                     , label='Select train percentage:'
                     , min=10
                     , max=90
                     , value=TRAIN_PERCENT )
      # Checkbox to configure data - makes the conditionalPanel
      , checkboxInput( 'configData'
                       , 'Configure Data:'
                       , value=FALSE )
      , conditionalPanel( 'input.configData'
                          # Example popover functionality
                          # Can be used to give instructions/help
                          , popify( selectInput( inputId='xNames'
                                                 , label='Select Variables:'
                                                 , multiple=TRUE
                                                 , choices=c(
                                                   'Crime rate' = 'crim'
                                                   , 'Large lots'='zn'
                                                   , 'Non-retail %'='indus'
                                                   , 'Charles river' = 'chas'
                                                   , 'NO_2 Conc' = 'nox'
                                                   , 'Rooms' = 'rm'
                                                   , 'Old homes'='age'
                                                   , 'Distance to Emp Centres'='dis'
                                                   , 'Access to radial highways' = 'rad'
                                                   , 'Property tax rate' = 'tax'
                                                   , 'Pupil-teacher ratio' = 'ptratio'
                                                   , 'Proportion African-American' = 'black'
                                                   , 'Lower status (%)' = 'lstat'
                                                 )
                                                 , selected=xNames ) 
                                    # Within popify, add info
                                    , title = 'Variable Translation:'
                                    , content =                  
                                      paste( 
                                        'Crime rate:crim'
                                        , 'Large lots:zn'
                                        , 'Non-retail %:indus'
                                        , 'Charles river:chas'
                                        , 'NO_2 Conc:nox'
                                        , 'Rooms:rm'
                                        , 'Old homes:age'
                                        , 'Distance to Emp Centres:dis'
                                        , 'Access to radial highways:rad'
                                        , 'Property tax rate:tax'
                                        , 'Pupil-teacher ratio:ptratio'
                                        , 'Proportion African-American:black'
                                        , 'Lower status (%):lstat'
                                        , sep='<br/>'
                                      ) 
                                      , placement='right'
                                     , options= list( container='body')
                                      )
                          
      )
      ## LM Configuration ----
      , checkboxInput( inputId='configLM', label='Configure LM', value=FALSE )
      , conditionalPanel( condition='input.configLM'
                          , h4( 'LM Configuration:' )
                          , checkboxInput( inputId='removeCorr'
                                           , label='Remove Correlated Variables'
                                           , value=TRUE ) 
                          , conditionalPanel( 'input.removeCorr'
                                              , sliderInput( 'threshCorr'
                                                             , label='Correlation threshold:'
                                                             , min=0
                                                             , max=1
                                                             , value=0.6
                                                             , step=0.05 ) 
                          )
      )
      ## RF Configuration ----
      , checkboxInput( 'configRF'
                       , label='Configure Random Forest:'
                       , value=FALSE )
      , conditionalPanel( condition='input.configRF'
                          , h4( 'RF Configuration:' )
                          , numericInput( inputId='numTrees'
                                          , label='Number of trees:'
                                          , value=BASE_NTREE )
                          , numericInput( inputId='mTry'
                                          , label='Number of predictors in each split:'
                                          , value=BASE_MTRY )
                          , numericInput( inputId='minN'
                                          , label='Number of points in a split:'
                                          , value=BASE_MIN_N )
      )
      
      ## Cross-validation conditional ----
      # Only appears from CV tab
      , conditionalPanel( condition='input.tabs == "CV"'
                          , h3('CV')
                          , numericInput( inputId='nCV'
                                          , label='Number of iterations:'
                                          , value=N_CV
                                          , min=5
                                          , max=50
                                          , step=1 )
                          , actionButton( inputId='goCV', label='Cross-Validate' ) 
      )
      ## Slides conditional ----
      # Only appears in Slides tab
      , conditionalPanel( condition='input.tabs=="Slides"'
                          , radioButtons( inputId='decision', label='Decision:', choices=c(
                            'Implement RF'='./Documentation/shinyRF.Rmd'
                            , 'Implement LM' = './Documentation/shinyLM.Rmd'
                            , 'Implement Both (A/B Test)'='./Documentation/shinyBoth.Rmd' 
                            , 'Implement Neither' = './Documentation/shinyNeither.Rmd' )
                          )
                          , textInput( inputId='decReasons', label='Reasons:' )
                          , verbatimTextOutput( outputId='listReasons' )
                          , actionButton( inputId='goReason', label='Submit Reason' )
                          , actionButton( inputId='resetReasons', label='Reset Reasons' )
                          , actionButton( inputId='goSlides', label='Build Slides' )
      )
      , width=3 )
    
    # Main Panel ----
    , mainPanel(
      tabsetPanel( id='tabs'
                   # Customise according to your wishes
                   # You might want more/less info on the model/data
                   # Documentation
                   , tabPanel( title='Summary'
                               , uiOutput( outputId='inDoc' ) )
                   # Data overview/EDA
                   , tabPanel( title='Data'
                               , plotOutput( outputId='trainSummary' ) )
                   # Training data/model
                   , tabPanel( title='Train'
                               , plotOutput( outputId='lineTrain' )
                               , plotOutput( outputId='residTrain' )
                               , tableOutput( outputId='metricTrain' ) )
                   # Test data/model
                   , tabPanel( title='Test'
                               , plotOutput( outputId='lineTest' )
                               , plotOutput( outputId='residTest' ) 
                               , tableOutput( outputId='metricTest' ) )
                   # Cross Validation
                   , tabPanel( title='CV'
                               , plotOutput( outputId='boxCV' )
                               , tableOutput( outputId='metricsCV' ) 
                   )
                   # Slides - doesn't do much as the reasons are in the
                   # sidebar rather than the main area
                   , tabPanel( title='Slides'
                               , htmlOutput( outputId='outDoc' ) )
      )
      , width=9 )
  ) # End sidebarLayout
) # End fluidPage
# Define server logic required to draw a histogram
serverSydney <- function(input, output, session) {
  # Updates to slides ---------
  observeEvent( input$goReason, {
    docRV$reasons <- rbind( docRV$reasons, input$decReasons )
    updateTextInput( session, inputId='decReasons', value='' )
  })
  # Wait to render slides for actionButton.
  observeEvent( input$goSlides,{
    
    # Note input$decision is a file name - not the display text!
    render( input$decision
            , output_file = 'slidesOut.ppt'
            , params=list(reasons=docRV$reasons
                          , testLine=plotsRV$lineTestPlot
                          , cvPlot=plotsRV$cvBoxPlot ) 
    )
    # Set the slides to TRUE to update the slides text
    docRV$makeSlides <- TRUE
  })
  ## Reactive functions ----
  # Functions are split into parts for overall modelling flow
  # Includes:
  # * Splitting data into train/test
  # * Changing the recipe/formula for the model types (2 reactives)
  # * Updating the models (though in this case, only the RF)
  # * Updating the train/test data to add predicted values
  #
  # The reactives have been structured to correspond to the input
  # panel, not for efficiency.
  #
  updateDataSplit <- reactive({
    # Update the random number seed to the stakeholder's value
    set.seed( input$splitSeed )
    # Re-split the data
    splitData <- initial_split( myData, prop = input$trainPercent/100 )
    # Update the RV
    dataRV$splitData <- splitData
  })  
  
  updateBaseRecipe <- reactive({
    baseFormula <- as.formula( paste( yName
                                      , '~'
                                      , paste( input$xNames, collapse='+' ) )
    )
    modelRV$baseRecipe <- recipe( baseFormula
                                  , data=dataRV$splitData %>% training() )
    return( modelRV$baseRecipe )
  })
  
  updateRecipes <- reactive({
    lmRecipe <- updateBaseRecipe()
    
    # Note that, for this demo, there is no modification to the RF data
    modelRV$rfRecipe <- updateBaseRecipe()
    
    modelRV$rfPrep <- prep( modelRV$rfRecipe
                            , fresh=TRUE
                            , training=dataRV$splitData %>% 
                              training() )
    
    # Check potential modifications to the LM data
    if( input$removeCorr ){
      lmRecipe <- lmRecipe %>% step_corr( all_predictors()
                                          ,threshold=input$threshCorr )
    }
    
    modelRV$lmRecipe <- lmRecipe
    modelRV$lmPrep <- prep( modelRV$lmRecipe
                            , fresh=TRUE
                            , training=dataRV$splitData %>%
                              training() )
    
  })
  updateModels <- reactive({
    modelRV$lmFit <- fitModel( modelRV$lmModel
                               , modelRV$lmRecipe
                               , dataRV$splitData %>% training() 
    )
    modelRV$rfModel <- rand_forest( mode='regression'
                                    , mtry=input$mTry
                                    , trees=input$numTrees
                                    , min_n=input$minN ) %>%
      set_engine( 'ranger' )
    # Before fitting the model, re-initialise the random number seed
    set.seed( input$splitSeed )
    modelRV$rfFit <- fitModel( modelRV$rfModel
                               , modelRV$rfRecipe
                               , dataRV$splitData %>% training() 
    )
    
  })
  
  updateTrainData <- reactive({
    # Get the prepped data
    trainDataLM <- juice( modelRV$lmPrep, all_predictors() )
    trainDataRF <- juice( modelRV$rfPrep, all_predictors() )
    # Augment the raw training data with predictions
    dataRV$completeTrainData <- dataRV$splitData %>% training() %>%
      bind_cols( predict( modelRV$lmFit, new_data=trainDataLM ) %>%
                   rename( lmPred=.pred ) ) %>%
      bind_cols( predict( modelRV$rfFit, new_data=trainDataRF ) %>%
                   rename( rfPred=.pred ) )
  })
  
  # Similar to training data, this reactive plays on the test data
  updateTestData <- reactive({
    testData <- dataRV$splitData %>% testing()
    testDataLM <- bake( modelRV$lmPrep, new_data=testData )
    testDataRF <- bake( modelRV$rfPrep, new_data=testData )
    dataRV$completeTestData <- dataRV$splitData %>% testing() %>%
      bind_cols( predict( modelRV$lmFit, new_data=testDataLM ) %>%
                   rename( lmPred=.pred ) ) %>%
      bind_cols( predict( modelRV$rfFit, new_data=testDataRF ) %>%
                   rename( rfPred=.pred ) )
    
  })
  
  # Output definitions ----
  output$inDoc <- renderUI({
    # Take a dependence to docRV$updateNumber
    docRV$updateNumber
    includeHTML( docRV$fileName )
  })
  
  output$trainSummary <- renderPlot({
    # Ensure data splits are up to date
    updateDataSplit()
    # Ensure recipes are up to date
    updateRecipes()
    
    # Get the data from the recipe
    # Note the recipe is from the modelRV, which is updated
    # from the updateRecipes (if needed)
    lmPlotData <- juice( modelRV$lmPrep, all_predictors() ) %>%
      # Put into a tidy format
      pivot_longer( cols=everything()
                    , names_to='Variable'
                    , values_to='Value' ) %>%
      mutate( Source='LM' )
    
    # ...same with RF data
    rfPlotData <- juice( modelRV$rfPrep, all_predictors() ) %>%
      pivot_longer( cols=everything()
                    , names_to='Variable'
                    , values_to='Value' ) %>%
      mutate( Source='RF')
    
    # Bind them together - note the variables may be 
    # different between the two model prototypes, but
    # because the data are tidy, it doesn't matter.
    fullPlotData <- bind_rows( lmPlotData, rfPlotData )
    
    # Plot the relevant variables
    # NB: the recipes are reactive, so changing the data 
    # input panel will change fullPlotData, and the plot updates.
    dataPlot <- ggplot( fullPlotData ) +
      facet_wrap( ~Variable, scales='free' ) +
      geom_histogram( aes( x=Value, fill=Source )
                      , alpha=0.4
                      , position='identity'
                      , bins=30 ) +
      scale_fill_brewer( type='qual' ) +
      theme( legend.position='bottom' )
    
    plotsRV$dataPlot <- dataPlot
    plotsRV$dataPlot
  })
  
  # Make a plot from the training data
  # An alternate means would be to create a function to make the
  # plot, and re-use the function on training and test data.
  output$lineTrain <- renderPlot({
    # Ensure all reactives are up to date
    updateDataSplit()
    updateRecipes()
    updateModels()
    updateTrainData() 
    
    lineTrainPlot <- ggplot( dataRV$completeTrainData ) +
      geom_point( aes( x=medv, y=lmPred, colour='LM' ), size=2 ) + 
      geom_point( aes( x=medv, y=rfPred, colour='RF' ), size=2 ) +
      geom_abline( slope=1, intercept = 0, linetype=3, size=1.5 ) +
      scale_colour_brewer( type='qual' ) +
      labs( x='Truth', y='Predicted' )
    
    plotsRV$lineTrainPlot <- lineTrainPlot
    plotsRV$lineTrainPlot
  })
  
  # Residual plot on training data
  # Again - having a generic "makeResidualPlot" function 
  # is another option.
  output$residTrain <- renderPlot({
    # Ensure everything is up-to-date
    updateDataSplit()
    updateRecipes()
    updateModels()
    updateTrainData()
    
    # Mutate variables for the errors
    residTrainData <- dataRV$completeTrainData %>%
      mutate( lmError=medv-lmPred
              , rfError=medv-rfPred )
    
    residTrainPlot <- ggplot( residTrainData ) +
      geom_histogram( aes(x=lmError, fill='LM' ), alpha=0.4, bins=30 ) +
      geom_histogram( aes( x=rfError, fill='RF'), alpha=0.4, bins=30 ) +
      scale_fill_brewer( type = 'qual' ) +
      labs( x='Error', y='Frequency' )
    
    # Save & Return the plot
    plotsRV$residTrainPlot <- residTrainPlot
    plotsRV$residTrainPlot
  })
  
  # Metrics for training data
  # Having a function for metrics is another option - but
  # the tidymodels::metrics is pretty close already!
  output$metricTrain <- renderTable({
    # Ensure everything is up-to-date
    updateDataSplit()
    updateRecipes()
    updateModels()
    updateTrainData()
    
    # Bind results from LM and RF models
    modelMetrics <- bind_rows( metrics( dataRV$completeTrainData
                                        , lmPred
                                        , medv ) %>%
                                 mutate( Source='LM')
                               , metrics( dataRV$completeTrainData
                                          , rfPred
                                          , medv ) %>% 
                                 mutate( Source='RF' ) ) %>%
      # Remove the estimator column
      select( -.estimator ) %>%
      # Widen for tabular display 
      pivot_wider( id_cols=Source
                   , names_from = .metric
                   , values_from= .estimate )
    
    dataRV$trainMetrics <- modelMetrics 
    
    # The tableOutput prefers data frames - return as such
    as.data.frame( modelMetrics )
  })
  
  # The outputs on test data are analogous to training data
  # They've been listed separately for illustration - 
  # it could be more efficient to leverage common bits
  output$lineTest <- renderPlot({
    updateDataSplit()
    updateRecipes()
    updateModels()
    updateTestData() 
    
    lineTestPlot <- ggplot( dataRV$completeTestData ) +
      geom_point( aes( x=medv, y=lmPred, colour='LM' ), size=2 ) + 
      geom_point( aes( x=medv, y=rfPred, colour='RF' ), size=2 ) +
      geom_abline( slope=1, intercept = 0, linetype=3, size=1.5 ) +
      scale_colour_brewer( type='qual' ) +
      labs( x='Truth', y='Predicted' )
    
    plotsRV$lineTestPlot <- lineTestPlot
    plotsRV$lineTestPlot
  })
  
  output$residTest <- renderPlot({
    updateDataSplit()
    updateRecipes()
    updateModels()
    updateTestData()
    
    residTestData <- dataRV$completeTestData %>%
      mutate( lmError=medv-lmPred
              , rfError=medv-rfPred )
    
    residTestPlot <- ggplot( residTestData ) +
      geom_histogram( aes(x=lmError, fill='LM' ), alpha=0.4, bins=30 ) +
      geom_histogram( aes( x=rfError, fill='RF'), alpha=0.4, bins=30 ) +
      scale_fill_brewer( type = 'qual' ) +
      labs( x='Error', y='Frequency' )
    
    plotsRV$residTestPlot <- residTestPlot
    plotsRV$residTestPlot
  })
  
  output$metricTest <- renderTable({
    updateDataSplit()
    updateRecipes()
    updateModels()
    updateTestData()
    
    testModelMetrics <- bind_rows( metrics( dataRV$completeTestData
                                            , lmPred
                                            , medv ) %>%
                                     mutate( Source='LM')
                                   , metrics( dataRV$completeTestData
                                              , rfPred
                                              , medv ) %>% 
                                     mutate( Source='RF' ) ) %>%
      select( -.estimator ) %>%
      pivot_wider( id_cols=Source
                   , names_from = .metric
                   , values_from= .estimate )
    
    dataRV$testMetrics <- testModelMetrics
    
    
    as.data.frame( testModelMetrics )
  })
  # Build a function to evaluate the parts - note the names in the function match the names in the list!
  cvFun <- function( cvSplit, cvID, cvRecipe, cvModel, modelName='Default' ){
    # Note that this takes a single recipe (at a time)
    prepRecipe <- cvSplit %>% training() %>% prep( x=cvRecipe, training=. )
    trainData <- cvSplit %>% training() %>% bake( prepRecipe, new_data=. )
    testData <- cvSplit %>% testing() %>% bake( prepRecipe, new_data=. )
    
    modelObject <- cvModel %>% fit( formula( prepRecipe ), data=trainData )
    
    testResults <- modelObject %>% predict( new_data=testData ) %>% bind_cols( testData  )
    
    testMetrics <- testResults %>%
      metrics( medv, .pred ) %>%
      dplyr::select( -.estimator ) %>%
      mutate( ID=cvID, Name=modelName )
    
    return( testMetrics )
  }
  # Only do cross-validation on button press
  observeEvent( input$goCV
                ,{
                  set.seed( input$splitSeed )
                  
                  crossValData <- vfold_cv( myData, v=input$nCV ) %>%
                    # Rename the values to specify for cross validation
                    rename( cvSplit=splits, cvID=id )
                  
                  # Build a list of test cases
                  fullCVData <- bind_rows(
                    crossValData %>% mutate( cvRecipe=list( modelRV$lmRecipe )
                                             , cvModel=list( modelRV$lmModel )
                                             , modelName='LM' )
                    , crossValData %>% mutate( cvRecipe=list( modelRV$rfRecipe )
                                               , cvModel=list( modelRV$rfModel )
                                               , modelName='RF' )
                  )
                  
                  # Get the results through mapping
                  dataRV$fullCVResults <- pmap_dfr( fullCVData, cvFun ) %>%
                    # Get rid of pesky dots
                    rename_all( list( ~sub( pattern='.', replacement='', fixed=TRUE, x=. ) ) )
                  
                  cvPlot <- ggplot( dataRV$fullCVResults ) +
                    geom_boxplot( aes( x=metric, y=estimate, colour=Name )
                                  , position='dodge' ) +
                    theme( legend.position='bottom' ) +
                    scale_colour_brewer( type='qual' )
                  
                  plotsRV$cvBoxPlot <- cvPlot
                  
                  # Sneaky update of documentation...
                  # The following updates the 
                  # documentation RV and renders the documentation
                  docRV$updateNumber <- docRV$updateNumber + 1
                  docRV$fileName <- paste0(
                    './Documentation/shinyUpdate'
                    , docRV$updateNumber
                    , '.html' 
                  )
                  
                  render( './Documentation/initialWriteUp.Rmd'
                          , output_file = docRV$fileName
                          , output_dir = './Documentation'  
                          , params=list(
                            data=Boston
                            , seed=input$splitSeed
                            , splitPc=input$trainPercent
                            , exclCorr=input$removeCorr
                            , exclCorrValue=input$threshCorr
                            , linModel=modelRV$lmFit
                            , numTrees=input$numTrees
                            , mTry=input$mTry
                            , minSize=input$minN
                            , rfModel=modelRV$rfFit
                            , trainLinePlot=plotsRV$lineTrainPlot   
                            , trainMetric=dataRV$trainMetrics
                            , testLinePlot=plotsRV$lineTestPlot
                            , testMetric=dataRV$testMetrics
                            , cvPlot=plotsRV$cvBoxPlot
                          )
                  )
                })  
  
  output$boxCV <- renderPlot( plotsRV$cvBoxPlot )
  # Metrics after CV
  output$metricsCV <- renderTable({
    if( is.null( dataRV$fullCVResults ) ) return( NULL )
    
    dataRV$fullCVResults %>%
      unite( col = 'Model_Metric'
             , c('Name', 'metric' ) ) %>%
      pivot_wider( id_cols=ID
                   , names_from = Model_Metric
                   , values_from=estimate ) 
  })
  # Input reasons to include in slides documentation
  output$listReasons <- renderPrint({
    if( is.null( docRV$reasons ) ) 'None' 
    else docRV$reasons
  })
  
  # Reset reasons if requested
  observeEvent( input$resetReasons, docRV$reasons<-NULL )
  ## Slides output  
  output$outDoc <- renderUI({
    
    # Check if the slides have been generated
    if( !docRV$makeSlides ){
      # If not, have a message indicating such
      h3('Slides not yet generated.' )
    }
    else{
      # If the slides have been made, message accordingly
      # I tried to have the slides as HTML in the window, but
      # they took over the window
      h3( 'Slides generated!' )
    }
    
  } )
  
}
## End application code


# Run the application 
shinyApp(ui = uiSydney, server = serverSydney)
