# #################################################################################################
# #################################################################################################
# #################################################################################################
# #################################################################################################
#' Linear Regression Application
#' @author The Data Science Collaboratory At Colgate University (datascience[at]colgate[dot]edu)
#' @description Server code for the Linear Regression app
# #################################################################################################
# #################################################################################################
# #################################################################################################
# #################################################################################################

server <- (function(input, output, session){
  options(scipen = 4)
  #############################################################################################
  # DATA OBJECTS
  #############################################################################################
  globalVars <- reactiveValues()
  globalVars$sample <- FALSE
  globalVars$dataset <- NULL
  globalVars$dataset.original <- NULL
  globalVars$fcts <- c()                 # Tracking Categorical Variables
  globalVars$transform.type <- "none"
  globalVars$interactions = c()
  
  globalVars$equation <- NULL
  globalVars$response <- NULL
  globalVars$model <- NULL
  globalVars$modelsummary <- NULL
  globalVars$prepare_model_interp <- NULL        
  
  globalVars$anova <- NULL
  globalVars$prepare_anova_interp <- NULL
  
  globalVars$anova_fctcomp <- NULL
  globalVars$make_anovafctcomp_plot <- NULL
  globalVars$make_anovafctcomp_num <- NULL
  globalVars$prepare_anova_fctcompinterp <- NULL
  
  globalVars$emmeans <- NULL
  globalVars$mod.emmeanscontrast <- NULL
  globalVars$mod.emtrends <- NULL
  globalVars$mod.emtrendcontrast <- NULL
  
  globalVars$make_ggpairs_plot <- NULL
  globalVars$make_ggpairs_summary <- NULL
  
  globalVars$make_ggpairs_plot <- NULL
  globalVars$make_ggpairs_summary <- NULL
  globalVars$make_assumptions_plot <- NULL

  globalVars$make_vif_num <- NULL
  globalVars$make_check_plot <- NULL
  
  globalVars$make_anova_num <- NULL
  globalVars$make_model_summary <- NULL
  
  globalVars$emmeans <- NULL
  globalVars$mod.emmeanscontrast <- NULL
  globalVars$mod.emtrends <- NULL
  
  globalVars$mod.emtrendcontrast <- NULL
  globalVars$make_interaction_emtrendscontrast_table <- NULL              
  globalVars$prepare_interaction_emtrendscontrast_interp <- NULL              
  globalVars$make_interaction_emtrends_tab <- NULL
  globalVars$prepare_interaction_emtrends_interp <- NULL
  
  globalVars$make_interaction_emmeans_tab <- NULL
  globalVars$prepare_interaction_emmeans_interp <- NULL            
  globalVars$make_interaction_contrasts_tab <- NULL
  globalVars$prepare_interaction_emmeanscontrast_interp <- NULL
  globalVars$ggemmeansplot <- NULL
  
  globalVars$jnplot <- NULL
  globalVars$prepare_jn_int <- NULL
  
  globalVars$modelmargins <- NULL
  globalVars$make_margins_summary <- NULL
  globalVars$prepare_margins_interp <- NULL
  
  globalVars$changed.input <- TRUE       # Changed input
  
  globalVars$scaleDataUpToDate <- FALSE
  
  shinyjs::hide("interaction_analysis")
  shinyjs::hide("var_inter")
  shinyjs::hide("var_moderator")
  shinyjs::hide("interaction.error")
  shinyjs::hide("ihs_button")
  shinyjs::hide("log_button")
  shinyjs::hide("lp1_button")
  
  hideTab(inputId="workPanel", target="summary")
  hideTab(inputId="workPanel", target="assumptions")
  hideTab(inputId="workPanel", target="checks")
  hideTab(inputId="workPanel", target="anova")
  hideTab(inputId="workPanel", target="interpretation")
  hideTab(inputId="workPanel", target="interaction")
  
  
  ##############################################
  # PROCESS UPLOADED DATA
  ##############################################
  upload_data<-reactive({
    req(input$file_upload)
    tryCatch({
      dat <- read.csv(input$file_upload$datapath)},
      error = function(e){
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    globalVars$dataset <- dat %>% mutate_if(is.character,as.factor)%>%
      mutate_if(is.integer,as.numeric)
    globalVars$dataset.original <- globalVars$dataset
    
    updateFactorsSelectize()
    emptyEquation()
  })
  
  ##############################################
  # DATASET PREVIEW
  ##############################################
  output$preview.data <- DT::renderDataTable({
    if(globalVars$sample){
      filename <- case_when(input$sample_data_choice=="Bracht et al. MFAP4"                             ~ "BrachtMFAP4Data",
                            input$sample_data_choice=="Palmer Penguins"                                 ~ "PalmerPenguin",
                            input$sample_data_choice=="U.S. News College Data"                          ~ "College",
                            input$sample_data_choice=="Cooley's Poor Beliefs Data"                      ~ "poorbeliefs"
      )
      
      DT::datatable(
        {globalVars$dataset},
        extensions = 'Buttons',
        options = list(
          dom = 'Bfrtip',
          buttons = list(list(extend="csv", text = "Download CSV", filename=filename,  exportOptions = list(modifier = list(page = "all"))))
        )
        , rownames = FALSE)
    }else{
      DT::datatable(globalVars$dataset, rownames = FALSE)
    }
    
  }, server = FALSE)
  
  #############################################################################################
  # When data is uploaded -- preview data and clear everything else
  #############################################################################################
  observeEvent(input$file_upload,{
    globalVars$changed.input <- TRUE
    #upload data and preview
    inFile <<-upload_data()
    #clear everything else
    hideInteractionInput()
    
    #UI resets
    updateTabsetPanel(session, "workPanel", selected = "Data Preview")
    globalVars$model <- NULL
    shinyjs::show("select_factors")
    uncheckAllAssumptions()
    globalVars$scaleDataUpToDate <- FALSE 
    updateCheckboxInput(session, "scalevars", value = FALSE)
    
  })
  
  read_data <- metaReactive2({
    if(globalVars$sample){
      if(input$sample_data_choice=="Bracht et al. MFAP4"){
        metaExpr({
          "# You can download the data on the Data Preview page"
          dat<-read_csv("BrachtMFAP4Data.csv")%>%
            mutate(Age=as.numeric(Age))
        })
      }else if(input$sample_data_choice=="Palmer Penguins"){
        metaExpr({
          "# You may need to install the palmerpenguins package: install.packages('palmerpenguins')"
          library(palmerpenguins)
          dat<-tibble(penguins)
        })
      }else if(input$sample_data_choice=="U.S. News College Data"){
        metaExpr({
          "# You may need to install the ISLR package: install.packages('ISLR')"
          library(ISLR)
          dat<-College
        })
      }else if(input$sample_data_choice=="Cooley's Poor Beliefs Data"){
        metaExpr({
          dat<-read_csv("poorbeliefs.csv") %>% mutate(Democrat = factor(Democrat))
        })
      }
    }else{
      metaExpr({
        quote(dat <- read_csv(..(input$file_upload$name)))
      }, quoted = T)
    }
  },inline=TRUE)
  
  #############################################################################################
  # When factors are selected -- update dataset
  #############################################################################################
  
  can.be.numeric <- function(x) {
    stopifnot(is.atomic(x) || is.list(x)) # check if x is a vector
    numNAs <- sum(is.na(x))
    numNAs_new <- suppressWarnings(sum(is.na(as.numeric(as.character(x))))) # if already a factor as.numeric works
    return(numNAs_new == numNAs)
  }
  
  observeEvent(input$select_factors,{
    globalVars$changed.input <- TRUE
    req(globalVars$dataset)
    updateTabsetPanel(session, "workPanel", selected = "data")
    globalVars$model <- NULL
    hideAllTabs()
    hideInteractionInput()
    uncheckAllAssumptions()
    
    #set factors in data
    factors<-input$select_factors
    dat<-globalVars$dataset.original
    
    factors.to.check <- setdiff(factors, globalVars$fcts)
    # are there factors to erase?
    factors.to.revert <- setdiff(globalVars$fcts, factors)
    
    for(var in union(factors, globalVars$fcts)){
      # Check if the factor is new
      if(var %in% factors.to.check){
        unique.categories <- unique(dat[[var]])
        if(can.be.numeric(unique.categories) & (length(unique.categories) >= 10)){ 
          shinyalert("Warning!", 
                     text = paste("You have specified", var, "as a categorical variable. The variable is numerically coded and has", length(unique.categories), "unique observations. If this was a mistake, simply remove", var, "from the list of categorical variables."), 
                     type = "warning")
        } 
      }
      
      # Change the variable type in the dataset
      if(var %in% factors.to.revert){
        if(can.be.numeric(dat[[var]])){
          dat[[var]] = as.numeric(as.character(dat[[var]]))
        }else{
          shinyalert("Warning!", 
                     text = paste("You have removed", var, "from the list of categorical variable(s), but it can not be converted to a numeric variable."), 
                     type = "warning")
          updateSelectizeInput(session, "select_factors",
                               "Specify Categorical Variables:",
                               choices = colnames(dat),
                               selected = c(factors, var))
        }
      }else{
        dat[[var]] = factor(dat[[var]])  
      }
      
    }
    
    #update dataset
    globalVars$dataset <- dat
    globalVars$fcts <- input$select_factors
    # CHECK THIS -- IF I CREATE THE REGRESSION FUNCTION AND THEN SELECT A NEW
    #               FACTOR IT APPEARS THIS WILL CHANGE THE SCALE INPUT
    globalVars$scaleDataUpToDate <- FALSE
    updateCheckboxInput(session, "scalevars", value = FALSE)
    
  }, ignoreNULL=FALSE)
  
  observeEvent(input$equation,{
    globalVars$changed.input <- TRUE
    updateTabsetPanel(session, "workPanel", selected = "data")
    globalVars$model <- NULL
    hideAllTabs()
    hideInteractionInput()
    uncheckAllAssumptions()

  })
  
  ##############################################################################
  # HANDLE TRANSFORMING DATA
  ##############################################################################
  prepare_transformed_data <- metaReactive2({
    
    dat <- globalVars$dataset
    var1 <- str_remove(globalVars$response, ".scaled")
    
    if(globalVars$transform.type == "ihs"){
      var1 <- str_remove(var1, "ihs.transformed.")
      metaExpr({
        "####################################"
        "# Transformation"
        "####################################"
        dat <- dat %>%
          mutate(x = !!sym(..(var1))) %>%
          mutate(!!..(paste("ihs.transformed.", var1, sep="")) := log(x + sqrt(x^2+1))) %>%
          dplyr::select(-x)
      })
    }else if(globalVars$transform.type == "log"){
      var1 <- str_remove(var1, "log.transformed.")
      metaExpr({
        "####################################"
        "# Transformation"
        "####################################"
        dat <- dat %>%
          mutate(!!..(paste("log.transformed.", var1, sep="")) := log(!!sym(..(var1))))
      })
    }else if(globalVars$transform.type == "lp1"){
      var1 <- str_remove(var1, "lp1.transformed.")
      metaExpr({
        "####################################"
        "# Transformation"
        "####################################"
        dat <- dat %>%
          mutate(!!..(paste("lp1.transformed.", var1, sep="")) := log(!!sym(..(var1))+1))
      })
    }
    
  }, inline=TRUE)
  
  ##############################################################################
  # IHS TRANSFORMATION
  ##############################################################################
  observeEvent(input$ihstransform, {
    globalVars$changed.input <- TRUE
    hideInteractionInput()
    updateTabsetPanel(session, "main", selected = "data")
    hideAllTabs()
    uncheckAllAssumptions()
    globalVars$transform.type = "ihs"
    globalVars$dataset <- prepare_transformed_data()
    
    # update equation
    # Do not adjust the prefix "ihs.transformed." unless also changing this pattern in the scale variables logic
    model.parts <- str_split(string=remove_response_scaling(input$equation), pattern="~", simplify = T)
    globalVars$equation <- paste("ihs.transformed.", trimws(model.parts[1]), " ~ ", model.parts[2], sep="")
    updateTextInput(session, "equation", value = globalVars$equation)
  })
  ##############################################################################
  # LOG TRANSFORMATION
  ##############################################################################
  observeEvent(input$logtransform, {
    
    globalVars$changed.input <- TRUE
    hideInteractionInput()
    updateTabsetPanel(session, "main", selected = "data")
    hideAllTabs()
    uncheckAllAssumptions()
    globalVars$transform.type = "log"
    globalVars$dataset <- prepare_transformed_data()
    
    # update equation
    # Do not adjust the prefix "log.transformed." unless also changing this pattern in the scale variables logic
    model.parts <- str_split(string=remove_response_scaling(input$equation), pattern="~", simplify = T)
    globalVars$equation <- paste("log.transformed.", trimws(model.parts[1]), " ~ ", model.parts[2], sep="")
    updateTextInput(session, "equation", value = globalVars$equation)
  })
  
  ##############################################################################
  # LOG(Y+1) TRANSFORMATION
  ##############################################################################
  observeEvent(input$logplus1transform, {
    globalVars$changed.input <- TRUE
    hideInteractionInput()
    updateTabsetPanel(session, "main", selected = "data")
    hideAllTabs()
    uncheckAllAssumptions()
    globalVars$transform.type = "lp1"
    globalVars$dataset <- prepare_transformed_data()
    
    # update equation
    # Do not adjust the prefix "lp1.transformed." unless also changing this pattern in the scale variables logic
    model.parts <- str_split(string=remove_response_scaling(input$equation), pattern="~", simplify = T)
    globalVars$equation <- paste("lp1.transformed.", trimws(model.parts[1]), " ~ ", model.parts[2], sep="")
    updateTextInput(session, "equation", value = globalVars$equation)
  })
  
  observeEvent(input$alpha,{
    globalVars$changed.input <- TRUE
    updateTabsetPanel(session, "workPanel", selected = "data")
    hideInteractionInput()
    hideAllTabs()
  })
  
  observeEvent(input$interaction.error,{
    if(!globalVars$changed.input & (input$var_inter!="None" & input$var_moderator!="Select...")){
      globalVars$ggemmeansplot <- ggemmeansplot()
    }
  })
  
  
  ##############################################################################
  # HANDLE SCALE VARIABLES AND TRANSFORMATION COLLISION
  ##############################################################################
  remove_response_scaling <- function(equation){
    
    # Try to parse variables safely
    vars <- try(all.vars(as.formula(equation)), silent = TRUE)
    
    # If equation is malformed:
    if (inherits(vars, "try-error")) return(NULL) #Exit early, can't process equation. ADD ERROR MODAL??
    
    suffix <- ".scaled"
    scaled <- equation
      
        # Only strip suffix from exact variable names
        pattern <- paste0("\\b", vars[1], "\\b")
        unsuffixed <- sub(paste0(suffix, "$"), "", vars[1])
        # won't change anything if pattern isn’t found
        scaled <- gsub(pattern, unsuffixed, scaled)
      
      globalVars$equation <- scaled
      return(scaled)
  }
  
  ##############################################################################
  # HANDLE SCALE VARIABLES DATSET CHANGE
  ##############################################################################
  prepare_scaled_data <- metaReactive2({
    dat <- globalVars$dataset
    if(input$scalevars==TRUE){
      metaExpr({
        "####################################"
        "# Create Scaled Data"
        "####################################"
        dat <- dat %>%
          mutate(across(where(is.numeric), 
                        ~ as.numeric(scale(.x)),
                        .names = "{.col}.scaled"))
      })
    }
  }, inline=TRUE)
  
  ##############################################################################
  # SCALE VARIABLES CHECKBOX STATE CHANGED
  ##############################################################################
  observeEvent(input$scalevars, {
    
    ### Kick the user back to the start
    globalVars$changed.input <- TRUE
    hideInteractionInput()
    updateTabsetPanel(session, "main", selected = "data")
    hideAllTabs()
    uncheckAllAssumptions()

    ### update dataset if needed
    if(!globalVars$scaleDataUpToDate){ # if Scaled Data isn't up to date
      if (!is.null(globalVars$dataset)){ # dataset exists
        globalVars$dataset <- prepare_scaled_data()
        globalVars$scaleDataUpToDate = TRUE
      }
    }
    
    
    ### UPDATE EQUATION
    equation <- input$equation
    
    # Try to parse variables safely
    vars <- try(all.vars(as.formula(equation)), silent = TRUE)
    
    # If equation is malformed:
    if (inherits(vars, "try-error")) return(NULL) #Exit early, can't process equation. ADD ERROR MODAL??
    
    suffix <- ".scaled"
    scaled <- equation
    
    checkEquationValidity()
    #We check the validity to get the error modals if something is wrong,
    #but we still scale things as normal because the code in this func is robust to malformed equations
    #Below we do not change variables that don't exist in the dataset
    
      if(input$scalevars){ ### user has checked the box
        
        for (v in vars){
          # if its not in the dataset, is categorical, already has a scaled tag,
          # or is a transformed variable, DO NOT ADD THE SCALED TAG
          if (is.null(globalVars$dataset[[v]]) || is.factor(globalVars$dataset[[v]]) || endsWith(v, suffix) || startsWith(v, "ihs.transformed.") || startsWith(v, "log.transformed.") || startsWith(v, "lp1.transformed.")){
            # Don't scale this variable, put it in without the suffix
            scaled <- gsub(paste0("\\b", v, "\\b"), v, scaled)
          } else {
            # Add suffix to the variable and put it in the scaled equation
            scaled <- gsub(paste0("\\b", v, "\\b"), paste0(v, suffix), scaled)
          }
        }
        
      } else { ### user has un-checked the box
          
          for (v in vars) {
            # Only strip suffix from exact variable names
            pattern <- paste0("\\b", v, "\\b")
            unsuffixed <- sub(paste0(suffix, "$"), "", v)
  
            # won't change anything if pattern isn’t found
            scaled <- gsub(pattern, unsuffixed, scaled)
          }
          
      }
      
      globalVars$equation <- scaled
      updateTextInput(session, "equation", value = globalVars$equation)
  })
  
  #############################################################################################
  # Add variables to interaction select input
  #############################################################################################
  updateInteractionSelect <- function(){
    req(input$equation)
    updateSelectizeInput(session, "var_inter",
                          "Select Interaction",
                          choices = c("None", globalVars$interactions),
                          selected = "None")
    
  }
  
  fitmodel<-metaReactive2({
    dat<-globalVars$data
    metaExpr({
      "####################################"
      "# Fit Model"
      "####################################"
      model<-lm(..(as.formula(globalVars$equation)), data=dat)
    })
  }, inline=TRUE)
  
  #############################################################################################
  # When interaction is selected -- update interaction selectize
  #############################################################################################
  observeEvent(input$var_inter,{
    interacts <- input$var_inter
    interact.vars <- strsplit(x = interacts, split = "[:]")[[1]]
    if(input$var_inter == "None" | is.null(input$var_inter)){
      shinyjs::hide("var_moderator")
      shinyjs::hide("interaction.error")
      if((input$var_inter=="None" | input$var_moderator=="Select...")){
        hideTab(inputId="workPanel", target="interaction")
      }
    }
    else{
      #######################################
      #Check for missing observations for interaction variables (both are categorical)
      ########################################
      var_inter1 <- str_split_1(input$var_inter, "\\:")[1]
      var_inter2 <- str_split_1(input$var_inter, "\\:")[2]
      stop <- FALSE
      if (!is.numeric(globalVars$dataset[[var_inter1]]) && !is.numeric(globalVars$dataset[[var_inter2]])){
        subbed <- str_replace_all(input$equation,fixed("+"),"~")
        subbed <- str_replace_all(subbed,fixed("*"),"~")
        subbed <- str_replace_all(subbed,fixed(":"),"~")
        variables <- trimws(str_split(string = subbed, pattern = "~", simplify = T))
        for(i in levels(globalVars$dataset[[var_inter1]])){
          for(j in levels(globalVars$dataset[[var_inter2]])){
            data.points <- globalVars$dataset %>%
              dplyr::filter(!!sym(var_inter1) == i & !!sym(var_inter2) == j & !is.na(!!sym(variables[1])))
            if(nrow(data.points) == 0){
              shinyalert("Error!", text = "There are missing combinations of the two interaction variables selected.", type = "error")
              return(NULL)
              stop <- TRUE
              break
            }
          }
          if(stop){break}
        }
      }
      shinyjs::show("var_moderator")
      updateSelectizeInput(session, "var_moderator",
                           "Select Moderator",
                           choices = c("Select...", interact.vars),
                           selected = "Select...")
      
      if((input$var_inter=="None" | input$var_moderator=="Select...")){
        hideTab(inputId="workPanel", target="interaction")
      }
    }
    
  })
  
  observeEvent(input$var_moderator,{
    req(globalVars$model)
    shinyjs::hide("interaction.error")
    updateCheckboxInput(session, "interaction.error", value =TRUE)
    
    if(input$var_moderator == "Select..."){
      hideTab(inputId="workPanel", target="interaction")   
    }else{
      showModal(modalDialog("Things are happening in the background!", footer=NULL))
      globalVars$emmeans <- prepare_interaction_emmeans()
      globalVars$mod.emmeanscontrast <- prepare_interaction_emmeanscontrasts()
      globalVars$mod.emtrends <- prepare_interaction_emtrends()
      if(!is.null(globalVars$mod.emtrends)){
        globalVars$mod.emtrendcontrast <- prepare_interaction_emtcontrast()
        globalVars$make_interaction_emtrendscontrast_table <- make_interaction_emtrendscontrast_table()
        globalVars$prepare_interaction_emtrendscontrast_interp <- prepare_interaction_emtrendscontrast_interp()
        globalVars$make_interaction_emtrends_tab <- make_interaction_emtrends_tab()
        globalVars$prepare_interaction_emtrends_interp <- prepare_interaction_emtrends_interp()
      }
      globalVars$make_interaction_emmeans_tab <- make_interaction_emmeans_tab()
      globalVars$prepare_interaction_emmeans_interp <- prepare_interaction_emmeans_interp()
      globalVars$make_interaction_contrasts_tab <- make_interaction_contrasts_tab()
      globalVars$prepare_interaction_emmeanscontrast_interp <- prepare_interaction_emmeanscontrast_interp()
      globalVars$ggemmeansplot <- ggemmeansplot()
      globalVars$jnplot <- jnplot()
      globalVars$prepare_jn_int <- prepare_jn_int()
      removeModal()
      showTab(inputId="workPanel", target="interaction")
    }
  })
  
  #############################################################################################
  # ADD VARIABLES TO FACTOR SELECTIZE INPUT
  #############################################################################################
  updateFactorsSelectize<- function(){
    req(globalVars$dataset)
    var.names = colnames(globalVars$dataset)
    fctvar.names = c(names(Filter(is.factor,globalVars$dataset)), names(Filter(is.character,globalVars$dataset)))
    globalVars$fcts <- fctvar.names
    updateSelectizeInput(session, "select_factors",
                         "Specify Categorical Variables in the Data:",
                         choices = var.names,
                         selected = fctvar.names)
  }
  
  
  #############################################################################################
  # Helper functions
  #############################################################################################
  # convert scientific notation to 10^
  changeSciNot <- function(n) {
    output <- format(n, scientific = TRUE) #Transforms the number into scientific notation even if small
    output <- sub("e", "*10^", output) #Replace e with 10^
    output <- sub("\\+0?", "", output) #Remove + symbol and leading zeros on expoent, if > 1
    output <- sub("-0?", "-", output) #Leaves - symbol but removes leading zeros on expoent, if < 1
    output
  }
  
  emptyEquation <- function(){
    updateTextInput(session, "equation", value = "")
  }
  
  uncheckAllAssumptions <- function() {
    shinyjs::hide("asmp_1note")
    shinyjs::hide("asmp_2note")
    shinyjs::hide("asmp_3note")
    shinyjs::hide("asmp_4note")
    shinyjs::hide("all")
    shinyjs::hide("asmp_note")
    updateCheckboxInput(session, "asmp_1", value = FALSE)
    updateCheckboxInput(session, "asmp_2", value = FALSE)
    updateCheckboxInput(session, "asmp_3", value = FALSE)
    updateCheckboxInput(session, "asmp_4", value = FALSE)
    
    shinyjs::hide("ihs_button")
    shinyjs::hide("log_button")
    shinyjs::hide("lp1_button")
    
    shinyjs::hide("check_note")
    updateCheckboxInput(session, "check_1", value = FALSE)
    updateCheckboxInput(session, "check_2", value = FALSE)
    updateCheckboxInput(session, "check_3", value = FALSE)
    updateCheckboxInput(session, "check_4", value = FALSE)
    
    updateCheckboxInput(session, "interaction.error", value = TRUE)
  }
  
  hideAllTabs <- function(){
    updateTabsetPanel(session, "workPanel", selected="data")
    
    hideTab(inputId="workPanel", target="summary")
    hideTab(inputId="workPanel", target="assumptions")
    hideTab(inputId="workPanel", target="checks")
    hideTab(inputId="workPanel", target="anova")
    hideTab(inputId="workPanel", target="interpretation")
    hideTab(inputId="workPanel", target="interaction")
  }
  
  showAllTabs <- function(){
    showTab(inputId="workPanel", target="summary")
    showTab(inputId="workPanel", target="assumptions")
    showTab(inputId="workPanel", target="checks")
    showTab(inputId="workPanel", target="anova")
    showTab(inputId="workPanel", target="interpretation")
  }
  
  hideInteractionInput <- function(){
    shinyjs::hide("interaction_analysis")
    shinyjs::hide("var_inter")
    shinyjs::hide("var_moderator")
    shinyjs::hide("interaction.error")
    
    updateSelectizeInput(session, "var_inter",
                         "Select Interaction",
                         choices = c("None"),
                         selected = "None")
    
    updateSelectizeInput(session, "var_moderator",
                         "Select Moderator",
                         choices = c("Select..."),
                         selected = "Select...")
    
    updateCheckboxInput(session, "interaction.error", value =TRUE)
  }
  
  showInteractionInput <- function(){
    shinyjs::show("interaction_analysis")
    shinyjs::show("var_inter")
  }
  
  #############################################################################################
  # When sample data is loaded
  #############################################################################################
  observeEvent(input$sample, {
    globalVars$changed.input <- TRUE
    updateTabsetPanel(session, "workPanel", selected = "data")
    globalVars$model <- NULL
    hideAllTabs()
    hideInteractionInput()
    uncheckAllAssumptions()
    emptyEquation()

    if(!globalVars$sample){
      globalVars$sample <- TRUE
      globalVars$dataset <- NULL
      shinyjs::hide("file_upload")
      shinyjs::show("choose_sample")
      shinyjs::show("select_factors")
      
      if(input$sample_data_choice=="Palmer Penguins"){
        library(palmerpenguins)
        dat<-data.frame(penguins)
      }else if(input$sample_data_choice=="Bracht et al. MFAP4" ){
        dat<-read.csv("www/mfap4.csv")%>%
          mutate(Age=as.numeric(Age))
      }else if(input$sample_data_choice=="U.S. News College Data"){
        library(ISLR)
        dat<-College
      }else if(input$sample_data_choice=="Cooley's Poor Beliefs Data"){
        dat<-read.csv("www/poorbeliefs.csv") %>% mutate(Democrat = factor(Democrat))
      }
      globalVars$dataset <- dat %>% mutate_if(is.character,as.factor)%>%
        mutate_if(is.integer,as.numeric)
      globalVars$dataset.original <- dat %>% mutate_if(is.character,as.factor)%>%
        mutate_if(is.integer,as.numeric)
      
      updateActionButton(session, "sample", label = "<- Back")
      updateFactorsSelectize()
      
    } else {
      globalVars$sample <- FALSE
      globalVars$dataset <- NULL
      shinyjs::show("file_upload")
      shinyjs::hide("choose_sample")
      
      if(!is.null(input$file_upload)){
        inFile <<- upload_data()
        shinyjs::show("select_factors")
        updateFactorsSelectize()
        
      }else{
        shinyjs::hide("select_factors")
        globalVars$dataset <- NULL
        globalVars$dataset.original <- NULL
        globalVars$fcts <- NULL
        updateSelectizeInput(session, "select_factors",
                             "Specify Categorical Variables in the Data:",
                             choices = c(""),
                             selected = NULL)
      }
      updateActionButton(session, "sample", label = "Sample Data")
    }
  })
  
  #############################################################################################
  # When a new sample selected
  #############################################################################################
  observeEvent(input$sample_data_choice,{
    globalVars$changed.input <- TRUE
    if(globalVars$sample){
      if(input$sample_data_choice=="Palmer Penguins"){
        library(palmerpenguins)
        dat<-data.frame(penguins)
      }else if(input$sample_data_choice=="Bracht et al. MFAP4" ){
        dat<-read.csv("www/mfap4.csv")%>%
          mutate(Age=as.numeric(Age))
      }else if(input$sample_data_choice=="U.S. News College Data"){
        library(ISLR)
        dat<-College
      }else if(input$sample_data_choice=="Cooley's Poor Beliefs Data"){
        dat<-read.csv("www/poorbeliefs.csv") %>% mutate(Democrat = factor(Democrat))
      } else if(input$sample_data_choice=="Lai et al. Tree Data"){
        dat<-read.csv("www/LaiTreeData.csv") %>% mutate(sp = factor(sp))
      } else if(input$sample_data_choice=="Lai et al. Schima Superba"){
        dat<-read.csv("www/LaiTreeData-SS.csv") %>% mutate(sp = factor(sp))
      } else if(input$sample_data_choice=="Loven et al. Road Weather Data"){
        dat<-read.csv("www/HalikkoAsphalt.csv") %>% mutate(RoadState = factor(RoadState))
      }
      shinyjs::show("select_factors")
      globalVars$dataset <- dat %>% mutate_if(is.character,as.factor)%>%
        mutate_if(is.integer,as.numeric)
      globalVars$dataset.original <- globalVars$dataset
      
      updateFactorsSelectize()
      hideInteractionInput()
      emptyEquation()
      uncheckAllAssumptions()
      hideAllTabs()
    }
  })
  
  ########################################
  # Check Equation Input
  ########################################
  checkEquationValidity <- function(){
    
    run <- TRUE
      dat <- globalVars$dataset
      
      if (run & grepl("~", input$equation)) {
        subbed <- str_replace_all(input$equation,fixed("+"),"~")
        subbed <- str_replace_all(subbed,fixed("*"),"~")
        subbed <- str_replace_all(subbed,fixed(":"),"~")
        variables <- trimws(str_split(string = subbed, pattern = "~", simplify = T))
        
        if(length(variables)>=2 && variables[2]!=""){
          globalVars$response <- variables[1]
          
          if(!(substr(globalVars$response, start=0, stop=16) %in% c("ihs.transformed.", "log.transformed.", "lp1.transformed."))){
            globalVars$transform.type <- "none"
            
            globalVars$dataset <- globalVars$dataset %>%
              dplyr::select(-starts_with("ihs.transformed.")) %>%
              dplyr::select(-starts_with("log.transformed.")) %>%
              dplyr::select(-starts_with("lp1.transformed."))
          }
          
          globalVars$equation <- input$equation

          #Check for misspellings 
          if(!all(variables %in% colnames(globalVars$dataset))){
            run <- FALSE
            shinyalert("Error!", text = "One of the variables in your equation does not exist in the data set. Please check your equation again for spelling or other errors.", type = "error")          
            # check for correct response
          }else if(!(is.numeric(globalVars$dataset[[variables[,1]]]))){
            run <- FALSE
            shinyalert("Error!", text = "Please make sure to choose a numeric response variable.", type = "error")
            # check for bad categorical variables
          }else{
            # Check for too many levels on explanatory variables
            for (i in 2:length(variables)){
              if(!is.numeric(globalVars$dataset[[variables[i]]])){
                if (length(unique(as.character(globalVars$dataset[[variables[i]]]))) > 12){
                  shinyalert("Error!", text = "One of your categorical predictor variables has more than the maximum supported number of unique levels (12). Please check that the predictor is indeed categorical or adjust your regression equation.", type = "error")
                  run <- FALSE
                  break
                }
              }
            } 
          }
        }else{
          run <- FALSE
          shinyalert("Error!", text="Your regression equation is not correctly specified, please rewrite it.", type = "error")
        }
      }else{
        run <- FALSE
        shinyalert("Error!", text="Your regression equation is not correctly specified, please rewrite it.", type = "error")
      }
    return(run)
    }
  
  #############################################################################################
  # When a new sample selected
  #############################################################################################
  observeEvent(input$DoCompute,{
    if(is.null(globalVars$dataset)){
      shinyalert("Error!", text = "Please choose a dataset.", type = "error")
    }
    req(globalVars$dataset)
    
    ########################################
    # Significance/confidence
    ########################################
    if(((!is.numeric(input$alpha))|is.null(input$alpha))){
      run <- FALSE
    }else{
      if((input$alpha<=0)|(input$alpha>=1)){
        run <- FALSE
        updateNumericInput(session, "alpha", value = 0.05)
        shinyalert("Error!", text = "The significance level must be a number between 0 and 1. It has been reset to the default of 0.05.", type = "error")
      }
    }
    
    ########################################
    # We changed the input, so try again
    ########################################
    if(globalVars$changed.input){
      run <- checkEquationValidity
      
      # Fit Model if we have an equation
      if(run){
        # Try to fit model
        model <- tryCatch({
          model<-lm((as.formula(globalVars$equation)), data=dat)
          # Change the call to have the full formula -- this is imporant for later. It is usually unnecessary but it is important
          # for the emmeans code -- this is how it checks whether a log transformation was used.
          model$call <- str2lang(paste("lm(formula=",globalVars$equation, ",data=globalVars$dataset)"))
          model 
        }, error = function(e){
          #
          if(grepl(pattern = "Error in eval(predvars, data, env): object", x = e, fixed = T)){
            # run <- FALSE
            shinyalert("Error!", text="You may have misspelled one of the variables. Please rewrite the regression equation.", type = "error")
          }else if(grepl(pattern = "Error in str2lang(x)", x = e, fixed=T)){  
            # run <- FALSE
            shinyalert("Error!", text="You may have misspelled one of the variables. Please rewrite the regression equation.", type = "error")
          }else if(grepl(pattern = "singular fits are not implemented", x = e, fixed=T)){  
            # run <- FALSE
            shinyalert("Error!", text="Your specified model has singular fit. This typically occurs when there is multicollinearity among predictor variables or too many predictors in the model. Please rewrite the regression equation, removing variables that cause the singularity.", type = "error")
          }else{
            # run <- FALSE
            shinyalert("Error!", text="There was an unanticipated error in fitting your regression model. Please report the issue and/or try another regression equation.", type = "error")
            safeError(e)
          }
        }, warning = function(w){
          #
          # catch when user has a response as a predictor
          if(as.character(w)=="simpleWarning in model.matrix.default(mt, mf, contrasts): the response appeared on the right-hand side and was dropped\n"){
            # run <- FALSE
            shinyalert("Error!", text="The response has been mistakenly referenced as a predictor. Please rewrite the regression equation.", type = "error")
          }else if(grepl(pattern = 'Warning in model.response(mf, "numeric"): using type = "numeric" with a factor response will be ignored', x = w, fixed = T)){
            # run <- FALSE
            shinyalert("Error!", text="You may have specified a non-numeric response variable. Please rewrite the regression equation.", type = "error")
          }else if(grepl(pattern = '<simpleError in model.frame.default(formula = (as.formula(globalVars$equation))', x = w, fixed = T)){
            # run <- FALSE
            shinyalert("Error!", text="You may have misspelled one of the variables. Please rewrite the regression equation.", type = "error")
          }else{
            # run <- FALSE
            shinyalert("Error!", text="There was an unanticipated error in fitting your regression model. Please report the issue and/or try another regression equation.", type = "error")          }
        })
        
        
        # Check the rank of the model matrix < number of predictors?
        model_matrix <- model.matrix(model)
        if(qr(model_matrix)$rank < (ncol(model_matrix))){
          shinyalert("Error!", text="Coefficients in your model are undefined due to signular fit. This can happen when there are missing combinations of observed categorical variables in a model with an interaction.", type = "error")
          return(NULL)
        }
        
        if(!("lm" %in% class(model))){
          run <- FALSE
        }
      }
      
      # Fit remaining analyses if the model was fit
      if(run){
        
        showModal(modalDialog("Things are happening in the background!", footer=NULL))
        
        # model pieces
        globalVars$model <- model
        globalVars$modelsummary <- prepare_model_summary()
        globalVars$prepare_model_interp <- prepare_model_interp()
        
        globalVars$anova <- prepare_anova()
        globalVars$prepare_anova_interp <- prepare_anova_interp()
        
        ### Show factor outputs, if necessary
        fct.vars <- names(which(attr(model$terms, "dataClasses")=="factor"))
        if(length(fct.vars)>=1){
          globalVars$anova_fctcomp <- prepare_anova_fctcomp()
          globalVars$make_anovafctcomp_plot <- make_anovafctcomp_plot()
          globalVars$make_anovafctcomp_num <- make_anovafctcomp_num()
          globalVars$prepare_anova_fctcompinterp <- prepare_anova_fctcompinterp()
          shinyjs::show("anova_fctcomp")
        }else{
          shinyjs::hide("anova_fctcomp")
        }
        
        #plots and summaries to make
        globalVars$make_ggpairs_plot <- make_ggpairs_plot()
        globalVars$make_ggpairs_summary <- make_ggpairs_summary()
        
        globalVars$make_assumptions_plot <- tryCatch({
          make_assumptions_plot()
        }, error = function(e) {
          shinyalert("Error!", text = "There was an issue calculating the residuals for your model. Perhaps one of your categorical variables is too sparse and must be removed from the data first.", type = "error")
          NULL
        })
        
        #check if there is a numeric variable to run VIF
        if(length(attr(model$terms, "term.labels"))>=2){
          globalVars$make_vif_num <- make_vif_num()  
        }
        
        globalVars$make_check_plot <- make_check_plot()
        globalVars$make_anova_num <- make_anova_num()
        globalVars$make_model_summary <- make_model_summary()
        
        # Show output tabs
        showAllTabs()
        updateTabsetPanel(session, "workPanel", selected = "summary")
        
        
        ### Show interaction outputs, if necessary
        if((grepl("[*]", globalVars$equation))){
          # for any model with an interaction
          shinyjs::show("marginaleffectsdiv")
          globalVars$modelmargins <- prepare_model_margins()
          globalVars$make_margins_summary <- make_margins_summary()
          globalVars$prepare_margins_interp <- prepare_margins_interp()
          
          # Extract Regression Terms
          terms <- attr(terms(model), "term.labels")
          # What are the two-way interactions?
          possible.ints <- terms[str_count(string=terms, pattern=":")==1]
          # What are the 3+-way interactions?
          bigger.ints <- terms[str_count(string=terms, pattern=":") > 1]
          
          if(length(possible.ints)==0){
            ints.to.select <- c("None")
          } else if(length(bigger.ints)==0){
            ints.to.select <- c(possible.ints)
          }else{
            ints.to.select <- c()
            # For each two-way
            for(int in possible.ints){
              # get the variables in the two way interaction
              curr.vars <- c(str_split(string=int, pattern=":", simplify = T))
              # now, check if it is part of a higher-order interaction
              include=TRUE
              for(bigger.int in bigger.ints){
                # get the variables in the bigger interaction
                curr.big.vars <- c(str_split(string=bigger.int, pattern=":", simplify = T))  
                # are all two-way interaction variables in the higher-order interaction?
                if(all(curr.vars %in% curr.big.vars)){
                  # if not, it stops at a two-way interaction and we can deal with it.
                  include=FALSE
                }
              }
              if(include){
                ints.to.select <- c(ints.to.select, int)
              }
            }  
          }
          if(length(ints.to.select)>0){
            globalVars$interactions = ints.to.select
            showInteractionInput()
            updateInteractionSelect()
          }
          else{
            globalVars$interactions = ints.to.select
            hideInteractionInput()
          }
          # # interaction untangling pieces (moderator specified)
          # if((input$var_inter!="None" & input$var_moderator!="Select...")){
          #   globalVars$emmeans <- prepare_interaction_emmeans()
          #   globalVars$mod.emmeanscontrast <- prepare_interaction_emmeanscontrasts()
          #   globalVars$mod.emtrends <- prepare_interaction_emtrends()
          #   if(!is.null(globalVars$mod.emtrends)){
          #     globalVars$mod.emtrendcontrast <- prepare_interaction_emtcontrast()
          #     globalVars$make_interaction_emtrendscontrast_table <- make_interaction_emtrendscontrast_table()
          #     globalVars$prepare_interaction_emtrendscontrast_interp <- prepare_interaction_emtrendscontrast_interp()
          #     globalVars$make_interaction_emtrends_tab <- make_interaction_emtrends_tab()
          #     globalVars$prepare_interaction_emtrends_interp <- prepare_interaction_emtrends_interp()
          #   }
          #   globalVars$make_interaction_emmeans_tab <- make_interaction_emmeans_tab()
          #   globalVars$prepare_interaction_emmeans_interp <- prepare_interaction_emmeans_interp()
          #   globalVars$make_interaction_contrasts_tab <- make_interaction_contrasts_tab()
          #   globalVars$prepare_interaction_emmeanscontrast_interp <- prepare_interaction_emmeanscontrast_interp()
          #   globalVars$ggemmeansplot <- ggemmeansplot()
          #   globalVars$jnplot <- jnplot()
          #   globalVars$prepare_jn_int <- prepare_jn_int()
          #   showTab(inputId="workPanel", target="interaction")
          # }else{
          #   hideTab(inputId="workPanel", target="interaction")
          # }
        }else{# no interaction
          shinyjs::hide("marginaleffectsdiv")
          hideTab(inputId="workPanel", target="interaction")
        }
        
        # toggle collinearity assumption
        if(length(attr(model$terms, "term.labels"))<2){
          shinyjs::hide('asmp_4')
          shinyjs::hide('asmp_4note') 
        }else{
          shinyjs::show('asmp_4')
        }
        removeModal()
        
        # Hide tabs and do nothing further if model is not fit
        globalVars$changed.input <-FALSE
      }else{
        globalVars$model <- NULL
        hideAllTabs()
      }
      
    }
    else{
      shinyalert("Warning!", 
                 text = "You have not changed the input. Your results are up to date.", 
                 type = "warning")
    }
    
  })
  

  
  #############################################################################################
  # Scatterplot Matrix
  #############################################################################################
  # Code to create plot
  make_ggpairs_plot <- metaReactive2({
    req(globalVars$model)
    model<-globalVars$model
    metaExpr({
      "####################################"
      "# Create Pairwise Plots"
      "####################################"
      # problem: switch cor and points below
      # problem: can we make discrete a better plot? (mosaic?)
      upper <- list(continuous = "points", discrete = wrap("colbar", size = 0), combo = "box_no_facet", na = "na")
      lower <- list(continuous = "cor", discrete = wrap("colbar", size = 0), combo = "box_no_facet", na = "na")
      ggpairs(model$model, progress = F, upper = upper, lower = lower) +
        theme_bw()+
        theme(axis.text.x = element_text(angle=60, vjust = 1, hjust=1)) + 
        scale_fill_grey()+
        scale_color_grey()
    })
  },inline=TRUE)
  
  # Render plot to UI
  output$ggpairs_plot<- renderPlot({
    globalVars$make_ggpairs_plot
  })
  
  # Download button for plots (call reactive function here to get plot object) ----
  output$downloadggpairsPlot <- downloadHandler(
    filename = function() { paste('pairwiseplots.',input$ggpairs_plot_format, sep='') },
    content = function(file) {
      ggsave(file, plot = globalVars$make_ggpairs_plot, device = input$ggpairs_plot_format, 
             width = as.numeric(input$ggpairs_plot_width), height = as.numeric(input$ggpairs_plot_height), 
             units = input$ggpairs_plot_units)
    }
  )
  
  
  refactor_data <- metaReactive2({
    if(length(input$select_factors) > 0){
      metaExpr({
        "####################################"
        "# Specify factor variables"
        "####################################"
        dat <- dat %>%
          mutate(across(..(input$select_factors), as.factor))
      })
    }
  },inline=TRUE)
  
  # R Code
  observeEvent(input$code_ggpairsplot, {
    code <- expandChain(
      "# Ensure to load your data as an object called dat",
      quote({
        library(GGally)
        library(tidyverse)
        #library(ggforce)
      }),
      "####################################",
      "# Load Data",
      "####################################",
      read_data(),
      refactor_data(),
      prepare_transformed_data(),
      prepare_scaled_data(),
      fitmodel(),
      make_ggpairs_plot()
    )
    
    displayCodeModal(
      code, 
      title = "Pairwise Plots",
      size = "l", 
      fontSize = 16,
      clip=NULL
    )
  })
  
  #############################################################################################
  # Correlation Matrix
  #############################################################################################
  # Code to create table
  make_ggpairs_summary <- metaReactive2({
    req(globalVars$model)
    model<-globalVars$model
    
    metaExpr({
      "####################################"
      "# Create Data for Correlation Matrix"
      "####################################"
      modmatrix <- model$model %>% 
        select_if(is.numeric)
      cormats<-Hmisc::rcorr(as.matrix(modmatrix), type = "pearson")
      cormat<-round(cormats$r,4)
      pmat<-as.matrix(data.frame(stars.pval(cormats$P)))
      "####################################"
      "# Create Correlation Matrix"
      "####################################"
      cormat<-matrix(paste(cormat, pmat, sep=""),nrow(cormats$r),ncol(cormats$r))
      rownames(cormat)<-colnames(cormats$r)
      colnames(cormat)<-rownames(cormats$r)
      data.frame(cormat)
    })
  },inline=TRUE)
  
  # Render table to UI
  output$ggpairs_summary <- DT::renderDataTable({
    globalVars$make_ggpairs_summary
  },
  extensions = 'Buttons',
  options = list(
    dom = 'Bfrtip',
    buttons = 
      list("copy", "print", list(
        extend = "collection",
        buttons = list(
          list(extend="csv", filename="model-correlations"),
          list(extend="excel", filename="model-correlations"),
          list(extend="pdf", filename="model-correlations")
        ),
        text = "Download",
        filename = "model-correlations"
      ))
  ), rownames = FALSE)
  
  # Download button for summary (LaTeX version)----
  output$downloadcormatLatex <- downloadHandler(
    filename = function() {
      paste("model-correlations.tex", sep="")
    },
    content = function(file) {
      base::print(xtable(globalVars$make_ggpairs_summary, digits = 4), file, type = "latex")
    }
  )
  
  #Rcode
  observeEvent(input$code_corrmat, {
    code <- expandChain(
      "# Ensure to load your data as an object called dat.",
      quote({
        library(gtools)
        library(Hmisc)
        library(tidyverse)
      }),
      "####################################",
      "# Load Data",
      "####################################",
      read_data(),
      refactor_data(),
      prepare_transformed_data(),
      prepare_scaled_data(),
      fitmodel(),
      make_ggpairs_summary()
    )
  
    displayCodeModal(
      code, 
      title = "Correlation Matrix",
      size = "l", 
      fontSize = 16,
      clip=NULL
    )
  })
  
  #############################################################################################
  # Regression Assumptions
  #############################################################################################
  #Code to create plot
  make_assumptions_plot <- metaReactive2({
    
    req(globalVars$model)
    
    model<-globalVars$model
    metaExpr({
      "####################################"
      "# Create Data for PDF of Residuals Plot"
      "####################################"
      ggdat<-data.frame(e=rstudent(model))
      ggdat.gaussian<-data.frame(x=seq(min(ggdat$e),max(ggdat$e),length.out = 1000),
                                 f=dnorm(seq(min(ggdat$e),max(ggdat$e),length.out = 1000),
                                         #ei should have mean zero
                                         mean=0,
                                         #ei should have common variance  
                                         #sd=summary(model)$sigma))
                                         sd=1))
      "####################################"
      "# Create PDF of Residuals Plot"
      "####################################"
      default.bins <- round(log2(nrow(ggdat)+1))
      p1<-ggplot(ggdat,aes(x=e))+
        geom_histogram(aes(y=after_stat(density)),bins=default.bins,
                       fill="lightgrey",color="black")+
        geom_density(aes(color="Empirical"), size=1,show.legend=FALSE)+
        stat_density(aes(x=e, color="Empirical"),
                     geom="line",position="identity")+
        geom_line(data=ggdat.gaussian,aes(x=x,y=f,color="Gaussian-Assumed"),linetype="dashed",size=1)+
        theme_bw()+
        xlab("Studentized Residual")+
        ylab("Density")+
        labs(color = "")+
        theme(legend.position="bottom")
      
      "####################################"
      "# Create Data for CDF of Residuals Plot"
      "####################################"
      e.cdf.func<-ecdf(rstudent(model))
      e.cdf<-e.cdf.func(sort(rstudent(model)))
      
      ggdat<-data.frame(e=sort(rstudent(model)),
                        e.cdf=e.cdf)
      ggdat.gaussian<-data.frame(x=seq(min(ggdat$e),max(ggdat$e),length.out = 1000),
                                 CDF=pnorm(seq(min(ggdat$e),max(ggdat$e),length.out = 1000),
                                           #ei should have mean zero
                                           mean=0,
                                           #ei should have common variance  
                                           #sd=summary(model)$sigma))
                                           sd=1))
      
      "####################################"
      "# Create CDF of Residuals Plot"
      "####################################"
      p2<-ggplot(data=ggdat,aes(x=e))+
        geom_step(aes(y=e.cdf,color="Empirical"))+
        geom_line(data=ggdat.gaussian,aes(x=x,y=CDF,color="Gaussian-Assumed"),linetype="dashed",size=1)+
        geom_hline(yintercept=0)+
        theme_bw()+
        xlab("Studentized Residual")+
        ylab("Cumulative Density")+
        labs(color = "")+
        theme(legend.position="bottom")
      
      "####################################"
      "# Create QQ Plot"
      "####################################"
      p3<-ggplot(data=ggdat,aes(sample=e))+ #standardize e
        geom_qq() +
        geom_qq_line() +
        theme_bw()+
        xlab("Gaussian Quantiles")+
        ylab("Sample Quantiles")
      
      "####################################"
      "# Create Data for Fitted vs Residual Plot"
      "####################################"
      ggdat<-data.frame(x=fitted(model),
                        e=rstudent(model))
      ggdat.out3<-ggdat %>% filter(abs(e)>3)
      
      
      "####################################"
      "# Create Fitted vs Residual Plot"
      "####################################"
      p4<-ggplot(data=ggdat,aes(x=x,y=e))+
        geom_point(shape=1)+
        geom_hline(yintercept = 0,color="red",linetype="dashed")+
        xlab(bquote("Fitted Values"~(hat(Y))))+
        ylab("Studentized Residual")+
        theme_bw()+
        geom_hline(yintercept = c(-3,3), color="red", linetype="dotted",size=0.75)+
        geom_point(data=ggdat.out3, aes(x=x,y=e), fill="red", shape=21)
      
      "####################################"
      "# Print Plots"
      "####################################"
      (p1+p2+p3+p4) + plot_layout(guides = "collect") & theme(legend.position = "bottom")
    })
  },inline=TRUE)
  
  # Render plot to UI
  output$asmp_plot <- renderPlot({
    globalVars$make_assumptions_plot
  })
  
  # Download button for plot
  output$downloadasmpPlot <- downloadHandler(
    filename = function() { paste('assumptionplots.',input$asmp_plot_format, sep='') },
    content = function(file) {
      ggsave(file, plot = globalVars$make_assumptions_plot, device = input$asmp_plot_format, 
             height = as.numeric(input$asmp_plot_height), width = as.numeric(input$asmp_plot_width), units = input$asmp_plot_units)
    }
  )
  
  # R Code
  observeEvent(input$code_asmp, {
    code <- expandChain(
      "# Ensure to load your data as an object called dat.",
      quote({
        library(tidyverse)
        library(patchwork)
      }),
      "####################################",
      "# Load Data",
      "####################################",
      read_data(),
      refactor_data(),
      prepare_transformed_data(),
      prepare_scaled_data(),
      fitmodel(),
      make_assumptions_plot()
    )
    displayCodeModal(
      code, 
      title = "Regression Assumptions Plot",
      size = "l", 
      fontSize = 16,
      clip=NULL
    )
  })
  
  # Render table to UI  
  output$vifTab <- DT::renderDataTable({
    globalVars$make_vif_num
  },
  extensions = 'Buttons',
  options = list(
    dom = 'Bfrtip',
    buttons = 
      list("copy", "print", list(
        extend = "collection",
        buttons = list(
          list(extend="csv", filename="model-vif"),
          list(extend="excel", filename="model-vif"),
          list(extend="pdf", filename="model-vif")
        ),
        text = "Download",
        filename = "model-vif"
      ))
  ), rownames = FALSE)
  
  make_vif_num <- metaReactive2({
    
    req(globalVars$model)
    model<-globalVars$model
    if(length(attr(model$terms, "term.labels"))>=2){
      shinyjs::show("vifdiv")
      
      model.vif <- vif(model)
      
      if(is.null(nrow(model.vif))){
        metaExpr({
          "####################################"
          "# Compute VIF"
          "####################################"
          model.vif <- vif(model)
          model.vif <- tibble(Terms = names(model.vif),
                              VIF   = model.vif) %>%
            mutate_if(is.numeric, round, 4)
        })
      }else{
        metaExpr({
          "####################################"
          "# Compute GVIF"
          "####################################"
          model.vif <- vif(model)
          model.vif <- tibble(Terms = rownames(model.vif),
                              `Adjusted GVIF`   = model.vif[,3])%>%
            mutate_if(is.numeric, round, 4)
        }) 
      }
    }else{
      shinyjs::hide("vifdiv")
    }
  }, inline=TRUE)
  
  # Download button for summary (LaTeX version)----
  output$downloadvifLatex <- downloadHandler(
    filename = function() {
      paste("model-viftable.tex", sep="")
    },
    content = function(file) {
      base::print(xtable(globalVars$make_vif_num, digits = 4), file, type = "latex")
    }
  )
  
  # R Code
  observeEvent(input$code_vif, {
    code <- expandChain(
      "# Ensure to load your data as an object called dat.",
      quote({
        library(tidyverse) 
        library(magrittr)
        library(car)
        library(effectsize)
      }),
      "####################################",
      "# Load Data",
      "####################################",
      read_data(),
      refactor_data(),
      prepare_transformed_data(),
      prepare_scaled_data(),
      fitmodel(),
      make_vif_num()
    )
    displayCodeModal(
      code, 
      title = "Regression VIF Table",
      size = "l", 
      fontSize = 16,
      clip=NULL
    )
  })
  
  #############################################################################################
  # Leverage and Outliers
  #############################################################################################
  # Code check plot
  make_check_plot <- metaReactive2({
    req(globalVars$model)
    
    model<-globalVars$model
    metaExpr({
      "####################################"
      "# Create Data for Leverage Plot"
      "####################################"
      d <- model$model 
      n <- nrow(d)
      p <- length(coef(model))
      d <- d %>% mutate(obs = 1:n)
      ggdat <- d %>% mutate(h.values = hatvalues(model))
      
      "####################################"
      "# Create Leverage Plot"
      "####################################"
      p1<-ggplot(data=ggdat,aes(x=obs)) +
        geom_linerange(aes(ymin=0, ymax=h.values)) +
        theme_bw()+
        xlab("Observation Number")+
        ylab("Leverage")+
        geom_hline(yintercept =2*p/n, linetype="dotted", color="orange",size=.75)+
        geom_hline(yintercept =3*p/n, linetype="dotted", color="red",size=.75)
      
      "####################################"
      "# Create Data for Cook's D Plot"
      "####################################"
      ggdat <- d %>% mutate(cook.d = cooks.distance(model))
      "####################################"
      "# Create Cook's D Plot"
      "####################################"
      p2<-ggplot(data=ggdat, aes(x=obs)) +
        geom_linerange(aes(ymin=0, ymax=cook.d)) +
        theme_bw()+
        theme(axis.text.x = element_text(angle=60, vjust=1, hjust=1))+
        xlab("Observation Number")+
        ylab("Cook's Distance")+
        geom_hline(yintercept = qf(p=0.10, df1=p, df2=n-p), linetype="dotted", color="orange",size=.75)+
        geom_hline(yintercept = qf(p=0.50, df1=p, df2=n-p), linetype="dotted", color="red",size=.75)
      
      "####################################"
      "# Create Data for DFFITS Plot"
      "####################################"
      ggdat <- d %>% mutate(dffits=dffits(model))
      "####################################"
      "# Create DFFITS Plot"
      "####################################"
      p3<-ggplot(data=ggdat, aes(x=obs)) +
        geom_linerange(aes(ymin=0, ymax=dffits)) +
        theme_bw()+
        xlab("Observation Number")+
        ylab("DFFITs")+
        geom_hline(yintercept = c(-2*sqrt(p/n), 2*sqrt(p/n)),
                   linetype="dotted",
                   size=.75,
                   color="orange")+
        geom_hline(yintercept = c(-2,2),
                   linetype="dotted",
                   size=.75,
                   color="red")
      
      "####################################"
      "# Create Data for Residual Plot"
      "####################################"
      ggdat<-data.frame(obs=d$obs,
                        y=rstudent(model))
      ggdat.out2<-ggdat %>% filter(abs(y)>2)
      ggdat.out3<-ggdat %>% filter(abs(y)>3)
      
      "####################################"
      "# Create Residual Plot"
      "####################################"
      p4<-ggplot(data=ggdat,aes(x=obs,y=y))+
        geom_point(shape=1)+
        geom_hline(yintercept = 0,color="red",linetype="dashed")+
        xlab("Observation Number")+
        ylab("Studentized Residual")+
        theme_bw()+
        geom_hline(yintercept = c(-3,3), color="red", linetype="dotted",size=0.75)+
        geom_hline(yintercept = c(-2,2), color="orange", linetype="dotted",size=0.75)+
        geom_point(data=ggdat.out2, aes(x=obs,y=y), fill="orange", shape=21)+
        geom_point(data=ggdat.out3, aes(x=obs,y=y), fill="red", shape=21)
      
      "####################################"
      "# Print Plots"
      "####################################"
      (p1|p2)/(p3|p4)
    })
  },inline=TRUE)
  
  # Render plot to UI
  output$check_plot <- renderPlot({
    globalVars$make_check_plot
  })
  
  # Download button for plot
  output$downloadcheckPlot <- downloadHandler(
    filename = function() { paste('checkplots.', input$check_plot_format, sep='') },
    content = function(file) {
      ggsave(file, plot = globalVars$make_check_plot, device = input$check_plot_format, 
             height = as.numeric(input$check_plot_height), width = as.numeric(input$check_plot_width), 
             units = input$check_plot_units)
    }
  )
  
  
  # Rcode
  observeEvent(input$code_check, {
    code <- expandChain(
      "# Ensure to load your data as an object called dat.",
      quote({
        library(tidyverse) 
        library(patchwork)
      }),
      "####################################",
      "# Load Data",
      "####################################",
      read_data(),
      refactor_data(),
      prepare_transformed_data(),
      prepare_scaled_data(),
      fitmodel(),
      make_check_plot()
    )
    
    displayCodeModal(
      code, 
      title = "Regression Leverage-Outlier-Influence Plot",
      size = "l", 
      fontSize = 16,
      clip=NULL
    )
  })
  
  prepare_anova <- metaReactive2({
    
    req(globalVars$model)
    dat <- globalVars$dataset
    model <- globalVars$model
    
    coef.table <- data.frame(summary(model)$coefficients)
    int.table <- coef.table[grep(x=rownames(coef.table), pattern=":", fixed = T),]
    
    if(nrow(int.table)>0 && any(int.table[4]<0.05)){ # significant interaction
      metaExpr({
        "####################################"
        "# ANOVA Table"
        "####################################" 
        anova.table <- data.frame(Anova(model, type="III"))
        "####################################"
        "# Clean Up Labels for Printing"
        "####################################"
        anova.table <- anova.table %>%
          mutate("Term" = rownames(.)) %>% # make terms part of table
          relocate(Term) %>%               # put Term in first column
          mutate(EffectSize= c(NA, epsilon_squared(model)[,2], NA)) %>%
          set_rownames(NULL) %>%
          set_colnames(c("Term", "SS (Type III)", "df", "F", "p-value", "Partial Epsilon-Squared"))
        
      })
    }else{
      if(deviance(model) < sqrt(.Machine$double.eps)){
        metaExpr({
          "####################################"
          "# ANOVA Table"
          "# Manual calc. due to perfect fit"
          "####################################" 
          aov.summary <- summary(aov(model))[[1]]
          inds <- trimws(rownames(aov.summary)[-nrow(aov.summary)])
          for(i in 1:length(inds)){
            aov.formula <- as.formula(paste(all.vars(model$call)[1], "~ ", paste(c(inds[-i], inds[i]), 
                                                                                 collapse = "+")))
            aov.curr <- lm(aov.formula, data=dat)
            aov.summary[i,] = summary(aov(aov.curr))[[1]][length(inds),]
          }
          "####################################"
          "# Clean Up Labels for Printing"
          "####################################"
          anova.table <- data.frame(aov.summary) %>%
            mutate("Term" = rownames(.)) %>% # make terms part of table
            mutate(EffectSize= c(epsilon_squared(model)[,2], NA)) %>%
            set_rownames(NULL) %>%
            dplyr::select(Term, Sum.Sq, Df, F.value, Pr..F., EffectSize) %>%               # put Term in first column
            set_colnames(c("Term", "SS (Type II)", "df", "F", "p-value", "Partial Epsilon-Squared")) %>%
            mutate_at(c("SS (Type II)", "df", "F", "p-value", "Partial Epsilon-Squared"), as.numeric)
        })
      }else{
        metaExpr({
          "####################################"
          "# ANOVA Table"
          "####################################" 
          anova.table <- data.frame(Anova(model, type="II"))
          
          "####################################"
          "# Clean Up Labels for Printing"
          "####################################"
          anova.table <- anova.table %>%
            mutate("Term" = rownames(.)) %>% # make terms part of table
            relocate(Term) %>%                   # put Term in first column
            mutate(EffectSize= c(epsilon_squared(model)[,2], NA)) %>%
            set_rownames(NULL) %>%
            set_colnames(c("Term", "SS (Type II)", "df", "F", "p-value", "Partial Epsilon-Squared")) 
        }) 
      }
    }
    
  }, inline=TRUE)
  
  #############################################################################################
  # Model ANOVA Table
  #############################################################################################
  # Code for model summary table
  make_anova_num <- metaReactive2({
    
    req(globalVars$anova)
    anova.table <- globalVars$anova
    metaExpr({
      "####################################"
      "# Print Table"
      "####################################"
      anova.table %>% 
        mutate_if(is.numeric, round, 4)%>%
        mutate(`p-value` = ifelse(`p-value` < 0.0001, "<0.0001", `p-value`))
      
    })
  }, inline=TRUE)
  
  # Render table to UI  
  output$anovaTab <- DT::renderDataTable({
    
    globalVars$make_anova_num
  },
  extensions = 'Buttons',
  options = list(
    dom = 'Bfrtip',
    buttons = 
      list("copy", "print", list(
        extend = "collection",
        buttons = list(
          list(extend="csv", filename="model-anova"),
          list(extend="excel", filename="model-anova"),
          list(extend="pdf", filename="model-anova")
        ),
        text = "Download",
        filename = "model-anova"
      ))
  ), rownames = FALSE)
  
  # Download button for summary (LaTeX version)----
  output$downloadanovaLatex <- downloadHandler(
    filename = function() {
      paste("model-anovatable.tex", sep="")
    },
    content = function(file) {
      base::print(xtable(globalVars$make_anova_num, digits = 4), file, type = "latex")
    }
  )
  
  # R Code
  observeEvent(input$code_anova, {
    code <- expandChain(
      "# Ensure to load your data as an object called dat.",
      quote({
        library(tidyverse) 
        library(magrittr)
        library(car)
        library(effectsize)
      }),
      "####################################",
      "# Load Data",
      "####################################",
      read_data(),
      refactor_data(),
      prepare_transformed_data(),
      prepare_scaled_data(),
      fitmodel(),
      prepare_anova(),
      make_anova_num()
    )
    
    displayCodeModal(
      code, 
      title = "Regression ANOVA Table",
      size = "l", 
      fontSize = 16,
      clip=NULL
    )
  })
  
  output$anovainterp <- renderUI({  
    globalVars$prepare_anova_interp
  })
  
  prepare_anova_interp <- function(){
    req(globalVars$anova)
    
    anova.table<-globalVars$anova %>%
      mutate(e2.text = case_when(`Partial Epsilon-Squared`<0.02     ~ "minuscule and perhaps negligible.",
                                 `Partial Epsilon-Squared`>=0.02 & `Partial Epsilon-Squared`<0.12  ~ "small.",
                                 `Partial Epsilon-Squared`>=0.13 & `Partial Epsilon-Squared`<0.26  ~ "moderate.",
                                 `Partial Epsilon-Squared`>=0.26           ~ "large."))
    alpha <- input$alpha
    
    anova.interp <- NULL
    for(i in 1:(nrow(anova.table))){
      if(anova.table$Term[i]=="Residuals"){
        #do nothing
      }else if(!grepl(x=anova.table$Term[i], pattern=":")){
        anova.interp <- (paste(anova.interp,"\U2022 The effect of ", sub("\\.scaled$", "", anova.table$Term[i]), " is ", ifelse(anova.table$`p-value`[i]<alpha, "significant ", "not significant "),
                               "(F = ",  round(anova.table$F[i],4),
                               ", p-value ",  ifelse(anova.table$`p-value`[i]<0.0001,"< 0.0001", paste("= ", round(anova.table$`p-value`[i],4), sep="")), ").",
                               ifelse(!is.na(anova.table[["Partial Epsilon-Squared"]][i]),
                                      paste(" The effect size is ", round(anova.table[["Partial Epsilon-Squared"]][i], 3), ", which indicates that the effect is ", anova.table$e2.text[i], sep=""), ""),
                               "<br/>", sep=""))
      }else{
        anova.interp <- (paste(anova.interp,"\U2022 The interactive effect of ", gsub("\\.scaled", "", anova.table$Term[i]), " is ", ifelse(anova.table$`p-value`[i]<alpha, "significant ", "not significant "),
                               "(F = ", round(anova.table$F[i],4),
                               ", p-value ",  ifelse(anova.table$`p-value`[i]<0.0001,"< 0.0001", paste("= ", round(anova.table$`p-value`[i],4), sep="")), ").",
                               ifelse(!is.na(anova.table[["Partial Epsilon-Squared"]][i]),
                                      paste(" The effect size is ", round(anova.table[["Partial Epsilon-Squared"]][i], 3), ", which indicates that the effect is ", anova.table$e2.text[i], sep=""), ""),
                               "<br/>", sep=""))
      }
    }
    
    HTML(anova.interp)
  }
  
  #############################################################################################
  # ANOVA Comparison
  #############################################################################################
  prepare_anova_fctcomp <- metaReactive2({
    
    req(globalVars$model)
    dat <- globalVars$dataset
    model <- globalVars$model
    
    metaExpr({
      # Determine the categorical predictors that need comparisons
      fct.vars <- names(which(attr(model$terms, "dataClasses")=="factor"))
      # Compare the different levels
      anova_fctcomp.table <- NULL
      
      for(fctvar in fct.vars){
        emm.obj <- emmeans(model, specs=fctvar)
        emm.test <-pairs(emm.obj)
        emm.ci <- confint(emm.test)
        
        emm.df <- data.frame(emm.test)
        emm.df$Variable <- rep(fctvar, nrow(emm.df))
        emm.df$lower.CI <- emm.ci$lower.CL
        emm.df$upper.CI <- emm.ci$upper.CL
        emm.df$CohensD <- abs(data.frame(eff_size(emm.obj, sigma(model), df.residual(model)))$effect.size)
        
        anova_fctcomp.table<- rbind(anova_fctcomp.table, emm.df)
      }
      
      # Return the table with clean labels
      anova_fctcomp.table %>%
        dplyr::select(Variable, everything()) %>% 
        set_rownames(NULL) %>%
        set_colnames(c("Variable", "Contrast", "Estimate", "SE", "df", "t ratio", "p-value", "Lower CI", "Upper CI", "Cohen's d"))
    })
  }, inline=TRUE)
  
  make_anovafctcomp_plot <- metaReactive2({
    req(globalVars$anova)
    anova_fctcomp.table <- globalVars$anova_fctcomp
    
    if(length(unique(anova_fctcomp.table$Variable))==1){
      metaExpr({
        "####################################"
        "# Create Plot"
        "####################################"
        ggplot(data=anova_fctcomp.table, aes(x=Estimate, y=Contrast))+
          geom_point()+
          geom_errorbar(aes(xmin=`Lower CI`, xmax=`Upper CI`), width=0.1) +
          xlab("Estimate")+
          ylab("Contrast")+
          theme_bw()+
          ggtitle("Factor Comparisons") + 
          xlim(min(anova_fctcomp.table$`Lower CI`), max(anova_fctcomp.table$`Upper CI`)) +
          geom_vline(xintercept = 0, linetype = "dashed")
      })
    }else{
      metaExpr({
        "####################################"
        "# Create Plot"
        "####################################"
        ggplot(data=anova_fctcomp.table, aes(x=Estimate, y=Contrast))+
          geom_point()+
          geom_errorbar(aes(xmin=`Lower CI`, xmax=`Upper CI`), width=0.1) +
          xlab("Estimate")+
          ylab("Contrast")+
          theme_bw()+
          ggtitle("Factor Comparisons")+
          facet_wrap(~Variable, scales = "free") +
          xlim(min(anova_fctcomp.table$`Lower CI`), max(anova_fctcomp.table$`Upper CI`)) +
          geom_vline(xintercept = 0, linetype = "dashed")
      }) 
    }
  }, inline = TRUE)
  
  # Code for model summary table
  make_anovafctcomp_num <- metaReactive2({
    
    req(globalVars$anova)
    anova_fctcomp.table <- globalVars$anova_fctcomp
    metaExpr({
      "####################################"
      "# Print Table"
      "####################################"
      anova_fctcomp.table <- anova_fctcomp.table %>% 
        mutate_if(is.numeric, round, 4) %>%
        mutate(`p-value` = ifelse(`p-value` < 0.0001, "<0.0001", `p-value`))
    })
  }, inline=TRUE)
  
  
  output$anova_fctcompinterp <- renderUI({  
    globalVars$prepare_anova_fctcompinterp
  })
  
  prepare_anova_fctcompinterp <- function(){
    
    anova_fctcomp.table <- globalVars$anova_fctcomp %>%
      mutate(d.text=  case_when(`Cohen's d`<0.20            ~ "minuscule and perhaps negligible.",
                                `Cohen's d`>=0.20 & `Cohen's d`<0.50  ~ "small.",
                                `Cohen's d`>=0.50 & `Cohen's d`<0.80  ~ "moderate.",
                                `Cohen's d`>=0.80           ~ "large."))
    alpha <- input$alpha
    
    anova.interp <- NULL
    for(i in 1:(nrow(anova_fctcomp.table))){
      anova.interp <- (paste(anova.interp,"\U2022 The contrast of (", anova_fctcomp.table$Contrast[i], ") is ", ifelse(anova_fctcomp.table$`p-value`[i]<alpha, "significant ", "not significant "),
                             "(t = ",  round(anova_fctcomp.table$`t ratio`[i],2),
                             ", p-value ",  ifelse(anova_fctcomp.table$`p-value`[i]<0.0001,"< 0.0001", paste("= ", round(anova_fctcomp.table$`p-value`[i],4), sep="")), 
                             ", 95% CI: ", round(anova_fctcomp.table$`Lower CI`[i],4), ",", round(anova_fctcomp.table$`Upper CI`[i], 4), ").",
                             " The effect size is ", round(anova_fctcomp.table[["Cohen's d"]][i], 3), ", which indicates that the effect is ", anova_fctcomp.table$d.text[i], "<br/>",
                             sep=""))
    }
    anova.interp<-paste(anova.interp, "<br/><strong>Note:</strong> This approach contrasts the estimated marginal means with a Tukey adjustment. These values are calculated at the 'average' of the other variables in the model.",
                        "You may want to set the other variables to specific values, which is supported in R but not currently supported in this application.")
    
    HTML(anova.interp)
  }
  
  output$anova_fctcomp_plot <- renderPlot({
    globalVars$make_anovafctcomp_plot
  })
  
  # Render table to UI  
  output$anova_fctcompTab <- DT::renderDataTable({
    globalVars$make_anovafctcomp_num
  },
  extensions = 'Buttons',
  options = list(
    dom = 'Bfrtip',
    buttons = 
      list("copy", "print", list(
        extend = "collection",
        buttons = list(
          list(extend="csv", filename="model-anova_fctcomp"),
          list(extend="excel", filename="model-anova_fctcomp"),
          list(extend="pdf", filename="model-anova_fctcomp")
        ),
        text = "Download",
        filename = "model-anova_fctcomp"
      ))
  ), rownames = FALSE)
  
  
  # Download button for summary (LaTeX version)----
  output$downloadanovafctcompLatex <- downloadHandler(
    filename = function() {
      paste("model-anovafctcomptable.tex", sep="")
    },
    content = function(file) {
      base::print(xtable(globalVars$make_anovafctcomp_num, digits = 4), file, type = "latex")
    }
  )
  
  # Download PDF button for plots (call reactive function here to get plot object) ----
  output$downloadanovafactorcomparePlot <- downloadHandler(
    filename = function() { paste('anovafactorcompare.', input$anova_fctcomp_plot_format, sep='') },
    content = function(file) {
      ggsave(file, plot = globalVars$make_anovafctcomp_plot, device = input$anova_fctcomp_plot_format, 
             height = as.numeric(input$anova_fctcomp_plot_height), width = as.numeric(input$anova_fctcomp_plot_width), 
             units = input$anova_fctcomp_plot_units)
    }
  )
  
  # R Code
  observeEvent(input$code_anova_fctcomp, {
    code <- expandChain(
      "# Ensure to load your data as an object called dat.",
      quote({
        library(tidyverse) 
        library(magrittr)
        library(car)
        library(effectsize)
        library(emmeans)
      }),
      "####################################",
      "# Load Data",
      "####################################",
      read_data(),
      refactor_data(),
      prepare_transformed_data(),
      prepare_scaled_data(),
      fitmodel(),
      prepare_anova_fctcomp(),
      make_anovafctcomp_num(),
      make_anovafctcomp_plot()
    )
    displayCodeModal(
      code, 
      title = "Regression ANOVA Factor Comparisons",
      size = "l", 
      fontSize = 16,
      clip=NULL
    )
  })
  
  
  
  #############################################################################################
  # Model Summary
  #############################################################################################
  prepare_model_summary <- metaReactive2({
    
    req(globalVars$model)
    
    dat <- globalVars$dataset
    model <- globalVars$model
    metaExpr({
      "####################################"
      "#Summarize Model"
      "####################################" 
      mod.sum <- summary(model)
      
      "####################################"
      "# Add Confidence Intervals"
      "####################################"  
      mod.table<-cbind(mod.sum$coefficients, na.omit(confint(model)))
      
      "####################################"
      "# Clean Up Labels for Printing"
      "####################################"
      mod.classes<-sapply(X = model$model, FUN = class)[-1]
      mod.classes<-mod.classes[which(mod.classes=="factor")]
      for(vname in names(mod.classes)){
        ind <- grepl(x=rownames(mod.table), pattern = vname)
        ind.int <- grepl(x=rownames(mod.table), pattern = ":")
        
        indexes.int<- ind & ind.int
        indexes <- xor(ind, indexes.int)
        
        varval  <- str_remove_all(rownames(mod.table)[indexes], vname)
        rownames(mod.table)[indexes]<-paste(vname, " = ", varval, sep="")
        
        if(any(indexes.int)){
          for(i in which(indexes.int)){
            intname <- rownames(mod.table)[i]
            vname.unique <- str_replace(string = intname, pattern = paste(rownames(mod.table)[(!indexes)&(!ind.int)],collapse ="|"), replacement = "")
            varval  <- str_remove_all(vname.unique, pattern = vname)
            varval  <- str_remove_all(varval, pattern = "[:]")
            
            rname<-str_remove_all(string = rownames(mod.table)[i], pattern = vname)
            rname<-str_remove_all(string = rname, pattern = varval)
            rname<-str_remove_all(string = rname, pattern = "[:]")
            
            rownames(mod.table)[i]<- paste(rname," : (",vname, " = ", varval,")", sep="")
          }
        }
      }
      mod.table <-  data.frame(mod.table) %>% 
        mutate("Term" = rownames(.)) %>%  # make terms part of table
        relocate(Term) %>%                # put Term in first column
        set_rownames(NULL) %>%
        set_colnames(c("Term", "Estimate", "SE", "t", "p-value", "Lower CI", "Upper CI"))
    })
  }, inline=TRUE)
  
  make_model_summary <- metaReactive2({
    
    req(globalVars$modelsummary)
    mod.table <- globalVars$modelsummary
    metaExpr({
      "####################################"
      "# Print Summary"
      "####################################" 
      mod.table %>% 
        mutate_if(is.numeric, round, 4) %>%
        mutate(`p-value` = ifelse(`p-value` < 0.0001, "<0.0001", `p-value`))
      
    })
  }, inline=TRUE)
  
  # Render table to UI  
  output$modsumTab <- DT::renderDataTable({
    globalVars$make_model_summary
  },
  extensions = 'Buttons',
  options = list(
    dom = 'Bfrtip',
    buttons = 
      list("copy", "print", list(
        extend = "collection",
        buttons = list(
          list(extend="csv", filename="model-summary"),
          list(extend="excel", filename="model-summary"),
          list(extend="pdf", filename="model-summary")
        ),
        text = "Download",
        filename = "model-summary"
      ))
  ), rownames = FALSE)
  
  # Download button for summary (LaTeX version)----
  output$downloadmodsumLatex <- downloadHandler(
    filename = function() {
      paste("model-summary.tex", sep="")
    },
    content = function(file) {
      base::print(xtable(globalVars$make_model_summary, digits = 4), file, type = "latex")
    }
  )
  
  
  # R Code
  observeEvent(input$code_modsum, {
    code <- expandChain(
      "# Ensure to load your data as an object called dat.",
      quote({
        library(tidyverse) 
        library(magrittr)
      }),
      "####################################",
      "# Load Data",
      "####################################",
      read_data(),
      refactor_data(),
      prepare_transformed_data(),
      prepare_scaled_data(),
      fitmodel(),
      prepare_model_summary(),
      make_model_summary()
    )
    
    displayCodeModal(
      code, 
      title = "Regression Summary Table",
      size = "l", 
      fontSize = 16,
      clip=NULL
    )
  })
  
  #############################################################################################
  # Summary of model and effects
  #############################################################################################
  # Code to interpret model
  output$modelinterp <- renderUI({
    globalVars$prepare_model_interp
  })
  
  prepare_model_interp <- function(){
    req(globalVars$model)
    req(globalVars$modelsummary)
    
    dat <- globalVars$dataset
    model <- globalVars$model
    mod.sum <- summary(model)
    mod.table <- globalVars$modelsummary
    
    alpha<-input$alpha
    
    F.pval<-pf(mod.sum$fstatistic[1],mod.sum$fstatistic[2],
               mod.sum$fstatistic[3], lower.tail = F)
    
    
    
    firstPart <- (paste("\U2022 The regression model ",
                        ifelse(F.pval<alpha, "significantly predicts ", "does not significantly predict "),
                        "the response (F = ", round(mod.sum$fstatistic[1],4),
                        ", df1 = ", round(mod.sum$fstatistic[2],4),
                        ", df2 = ", round(mod.sum$fstatistic[3],4),
                        ", p-value ", ifelse(F.pval<0.0001, " < 0.0001", paste(" =", round(F.pval, 4))),").<br/>",
                        "\U2022 The adjusted R-squared is ", round(mod.sum$adj.r.squared,4),
                        " which indicates that ", round(mod.sum$adj.r.squared*100,2), "% of the variability in the response is explained by the model.<br/>",
                        "\U2022 The root mean squared error is ", round(summary(model)$sigma,4),
                        ". This value is used to estimate the common standard deviation of the residuals.<br/>",
                        sep=""))
    
    ind <- which(grepl(pattern = "[:]", x = mod.table$Term))
    secondPart<-""
    
    if(length(ind)==0){ # NO INTERACTION
      numeric.vars <- setdiff(mod.table$Term[!grepl(pattern = " = ", x = mod.table$Term)], "(Intercept)")
      
      
        for(i in 1:nrow(mod.table)){
          curr.row <- mod.table$Term[i]
          if(curr.row=="(Intercept)"){next} #skip intercept
          effect.value <- mod.table$Estimate[i]
          t.value <- round(mod.table$t[i], 4)
          pval <- round(mod.table$`p-value`[i],4)
          pval <- ifelse(pval<0.0001, " < 0.0001", paste(" =", round(pval, 4)))
          sig.text <- ifelse(pval > input$alpha, 
                             paste(". This effect did not reach statistical significance", " (t = ", round(mod.table$t[i], 4), " p", pval,").", sep=""),
                             paste(" (t=", round(mod.table$t[i], 4), " p", pval,").", sep=""))
          
          if(curr.row %in% numeric.vars){ # numeric
            if(endsWith(curr.row, ".scaled")){
              unit <- "standard deviation"
              unit2 <- " standard deviation"
              curr.row <- sub("\\.scaled$", "", curr.row) #.scaled $ (only from end)
            } else{
              unit <- "unit"
              unit2 <- ""
            }
            if(globalVars$transform.type == "none"){
              adding.text <- paste("\U2022 For every ", unit," increase in ", curr.row, ", we expect a ", round(abs(effect.value),4) , unit2, ifelse(effect.value<0, " decrease ", " increase "),
                                   "in ", sub("\\.scaled$", "", names(model$model)[1]), ", on average.", sig.text, "\n", sep="")
            }else if(globalVars$transform.type == "log"){
              adding.text <- paste("\U2022 For every ", unit," increase in ", curr.row, ", we expect a 100(e^(", round(mod.table$Estimate[i],4),")-1) = ", round(abs(effect.value),2), ifelse(effect.value<0, " percent decrease ", " percent increase "),
                                   "in ", sub("\\.scaled$", "", substring(names(model$model)[1], first = 17)), ", on average.", sig.text, "\n", sep="")
            }else if(globalVars$transform.type == "lp1"){
              adding.text <- paste("\U2022 For every ", unit," increase in ", curr.row, ", we expect a 100(e^(", round(mod.table$Estimate[i],4),")-1) = ", round(abs(effect.value),2), ifelse(effect.value<0, " percent decrease ", " percent increase "),
                                   "in (", sub("\\.scaled$", "", substring(names(model$model)[1], first = 17)), "+1), on average.", sig.text, "\n", sep="")
            }else if(globalVars$transform.type == "ihs"){
              adding.text <- paste("\U2022 For every ", unit," increase in ", curr.row, ", we expect ", ifelse(effect.value<0, "a decrease ", "an increase "),
                                   "in ", sub("\\.scaled$", "", substring(names(model$model)[1], first = 17)), ", on average.", sig.text, "\n", sep="")
            }
            secondPart <- paste(secondPart, adding.text, sep="<br/>")
          }
          else{ #factor
            findequal <- str_locate(string=curr.row, pattern = " = ")
            varname <- substr(curr.row, start = 1, stop = findequal[1]-1)
            baselevel<-paste(varname, "=", levels(model$model[,varname])[1])
            
            if(globalVars$transform.type == "none"){
              adding.text <- paste("\U2022 We expect ", gsub("\\.scaled", "", names(model$model)[1]), " to be ", round(abs(effect.value),4), ifelse(effect.value<0, " lower ", " higher "), "when ",
                                   curr.row, " compared to ", baselevel, ", on average", sig.text, "\n", sep="")
            }else if(globalVars$transform.type == "log"){
              adding.text <-paste("\U2022 We expect ", gsub("\\.scaled", "", substring(names(model$model)[1], first=17)), " to be 100(e^(", round(abs(effect.value),4),")-1) = ", round(abs(effect.value),2), ifelse(effect.value<0, " percent lower ", " percent higher "), "when ",
                                  curr.row, " compared to ", baselevel, ", on average", sig.text, "\n", sep="")
            }else if(globalVars$transform.type == "lp1"){
              adding.text <-paste("\U2022 We expect (", gsub("\\.scaled", "", substring(names(model$model)[1], first=17)), "+1) to be 100(e^(", round(abs(effect.value),4),")-1) = ", round(abs(effect.value),2), ifelse(effect.value<0, " percent lower ", " percent higher "), "when ",
                                  curr.row, " compared to ", baselevel, ", on average", sig.text, "\n", sep="")
            }else if(globalVars$transform.type == "ihs"){
              adding.text <-paste("\U2022 We expect ", gsub("\\.scaled", "", substring(names(model$model)[1], first=17)), " to be ", ifelse(effect.value<0," lower ", " higher "),
                                  "when ", curr.row, " compared to ", baselevel, ", on average", sig.text, "\n", sep="")
            }
            
            secondPart <- paste(secondPart, adding.text, sep="<br/>")
          }
        }
    }else{
      for(i in ind){
        secondPart <- (paste(secondPart, "\U2022 The interactive effect of ", gsub("\\.scaled", "", mod.table$Term[i]), " is ", ifelse(mod.table$`p-value`[i]<alpha, "significant ", "not significant "),
                             "(", "\U03B2 = ", round(mod.table$Estimate[i],4),
                             ", t = ",  round(mod.table$t[i],4),
                             ", p-value ",  ifelse(mod.table$`p-value`[i]<0.0001,"< 0.0001", paste("= ", round(mod.table$`p-value`[i],4), sep="")), ").<br/>",
                             sep=""))
      }
    }
    HTML(paste(firstPart, secondPart))
    
  }
  
  #############################################################################################
  # Coefficient/Marginal Effect Interpretations
  #############################################################################################
  # Code to create margins table
  prepare_model_margins <- metaReactive2({
    req(globalVars$model)
    
    dat <- globalVars$dataset
    model <- globalVars$model
    metaExpr({
      "####################################"
      "# Summarize Model"
      "####################################"
      mod.sum <- summary(model)
      
      "####################################"
      "# Calculate Marginal Effects"
      "####################################"  
      ### Interpret Marginal Effects
      margins.table<-summary(margins(model))
      
      "####################################"
      "# Clean Up Labels for Printing"
      "####################################"
      marg.classes<-sapply(X = model$model, FUN = class)[-1]
      if(any(marg.classes=="factor")){
        marg.classes<-marg.classes[which(marg.classes=="factor")]
        for(vname in names(marg.classes)){
          varname <- vname
          indexes <- grepl(x=margins.table$factor, pattern = vname)
          varval  <- str_remove_all(margins.table$factor[indexes], varname)
          margins.table$factor[indexes]<-paste(varname, "=", varval)
        }
      }
      margins.table <- margins.table %>%
        set_rownames(NULL) %>%
        set_colnames(c("Term", "Average Marginal Effect", "SE", "z", "p-value", "Lower CI", "Upper CI"))
    })
  }, inline=TRUE)
  
  
  make_margins_summary <- metaReactive2({
    
    margins.table <- globalVars$modelmargins
    metaExpr({
      "####################################"
      "# Print Table"
      "####################################"
      margins.table %>% 
        mutate_if(is.numeric, round, 4) %>%
        mutate(`p-value` = ifelse(`p-value` < 0.0001, "<0.0001", `p-value`))
    })
  }, inline=TRUE)
  
  ### Render Model Summary to UI
  output$marginsTab <- DT::renderDataTable({
    globalVars$make_margins_summary
  },
  extensions = 'Buttons',
  options = list(
    dom = 'Bfrtip',
    buttons = 
      list("copy", "print", list(
        extend = "collection",
        buttons = list(
          list(extend="csv", filename="model-marginaleffects"),
          list(extend="excel", filename="model-marginaleffects"),
          list(extend="pdf", filename="model-marginaleffects")
        ),
        text = "Download",
        filename = "model-marginaleffects"
      ))
  ), rownames = FALSE)
  
  # Download button for summary (LaTeX version)----
  output$downloadmarginsLatex <- downloadHandler(
    filename = function() {
      paste("model-marginaleffects.tex", sep="")
    },
    content = function(file) {
      base::print(xtable(globalVars$make_margins_summary, digits = 4), file, type = "latex")
    }
  )
  
  # R Code
  # Display code for ci visualization plot ----
  observeEvent(input$code_margins, {
    code <- expandChain(
      "# Ensure to load your data as an object called dat.",
      quote({
        library(tidyverse) 
        library(magrittr)
        library(margins)
      }),
      "####################################",
      "# Load Data",
      "####################################",
      read_data(),
      refactor_data(),
      prepare_transformed_data(),
      prepare_scaled_data(),
      fitmodel(),
      prepare_model_margins(),
      make_margins_summary()
    )

    displayCodeModal(
      code, 
      title = "Regression Marginal Effects Table",
      size = "l", 
      fontSize = 16,
      clip=NULL
    )
  })
  
  # Code to interpret marginal effects
  output$marginsinterp <- renderUI({
    globalVars$prepare_margins_interp
  })
  
  prepare_margins_interp <- function(){
    model <- globalVars$model
    margins.table <- globalVars$modelmargins
    
    mod.classes<-sapply(X = model$model, FUN = class)[-1]
    mod.classes<-mod.classes[which(mod.classes=="numeric")]
    
    first.part <- ""
    for(i in 1:nrow(margins.table)){
      curr.row <- margins.table$Term[i]
      effect.value <- margins.table$`Average Marginal Effect`[i]
      z.value <- round(margins.table$z[i], 4)
      p.value <- ifelse(margins.table$`p-value`[i]<0.0001, "<0.0001", round(margins.table$`p-value`[i], 4))
      sig.text <- ifelse(p.value > input$alpha, " This effect did not reach statistical significance", "")
      
      if(curr.row %in% names(mod.classes)){ # numeric
        if(endsWith(curr.row, ".scaled")){
          unit <- "standard deviation"
          unit2 <- " standard deviation"
          curr.row <- sub("\\.scaled$", "", curr.row) #.scaled $ (only from end)
        } else{
          unit <- "unit"
          unit2 <- ""
        }
        if(globalVars$transform.type == "none"){
          adding.text <- paste("\U2022 For every ", unit," increase in ", curr.row, ", we expect a ", round(abs(effect.value),4) , unit2, ifelse(effect.value<0, " decrease ", " increase "),
                               "in ", sub("\\.scaled$", "", names(model$model)[1]), ", on average.", sig.text, " (z=", round(z.value, 4), " p=",  p.value,  ").", "\n", sep="")
        }else if(globalVars$transform.type == "log"){
          adding.text <- paste("\U2022 For every ", unit," increase in ",curr.row, ", we expect a 100(e^(",round(margins.table$`Average Marginal Effect`[i],4),")-1) = ", round(abs(effect.value),2) , ifelse(effect.value<0, " percent decrease ", " percent increase "),
                               "in ", sub("\\.scaled$", "", substring(names(model$model)[1], first = 17)), ", on average.", sig.text, " (z=", round(z.value, 3), " p=",  p.value,  ").", "\n", sep="")
        }else if(globalVars$transform.type == "lp1"){
          adding.text <- paste("\U2022 For every ", unit," increase in ",curr.row, ", we expect a 100(e^(",round(margins.table$`Average Marginal Effect`[i],4),")-1) = ", round(abs(effect.value),2) , ifelse(effect.value<0, " percent decrease ", " percent increase "),
                               "in (", sub("\\.scaled$", "", substring(names(model$model)[1], first = 17)), "+1), on average.", sig.text, " (z=", round(z.value, 3), " p=",  p.value,  ").", "\n", sep="")
        }else if(globalVars$transform.type == "ihs"){
          adding.text <- paste("\U2022 For every ", unit," increase in ",curr.row, ", we expect ", ifelse(effect.value<0, "a decrease ", "an increase "),
                               "in ", sub("\\.scaled$", "", substring(names(model$model)[1], first = 17)), ", on average.", sig.text, " (z=", round(z.value, 3), " p=",  p.value,  ").", "\n", sep="")
        }
        
        
        first.part <- paste(first.part, adding.text, sep="<br/>")
      }
      else{ #factor
        findequal <- str_locate(string=curr.row, pattern = " = ")
        varname <- substr(curr.row, start = 1, stop = findequal[1]-1)
        baselevel<-paste(varname, "=", levels(model$model[,varname])[1])
        
        if(globalVars$transform.type == "none"){
          adding.text <-paste("\U2022 We expect ", names(model$model)[1], " to be ", round(abs(effect.value),4), ifelse(effect.value<0, " lower ", " higher "), "when ",
                              curr.row, " compared to ", baselevel, ", on average.", sig.text, " (z=", round(z.value, 4), " p=",  p.value,  ").", "\n", sep="")
        }else if(globalVars$transform.type == "log"){
          adding.text <-paste("\U2022 We expect ", substring(names(model$model)[1], first = 17), " to be 100(e^(",round(margins.table$`Average Marginal Effect`[i],4),")-1) = ", round(abs(effect.value),2), ifelse(effect.value<0, " percent lower ", " percent higher "), "when ",
                              curr.row, " compared to ", baselevel, ", on average.", sig.text, " (z=", round(z.value, 4), " p=",  p.value,  ").", "\n", sep="")
        }else if(globalVars$transform.type == "lp1"){
          adding.text <-paste("\U2022 We expect (", substring(names(model$model)[1], first = 17), "+1) to be 100(e^(",round(margins.table$`Average Marginal Effect`[i],4),")-1) = ", round(abs(effect.value),2), ifelse(effect.value<0, " percent lower ", " percent higher "), "when ",
                              curr.row, " compared to ", baselevel, ", on average.", sig.text, " (z=", round(z.value, 4), " p=",  p.value,  ").", "\n", sep="")
        }else if(globalVars$transform.type == "ihs"){
          adding.text <-paste("\U2022 We expect ", substring(names(model$model)[1], first = 17), " to be ", ifelse(effect.value<0, "lower ", "higher "), "when ",
                              curr.row, " compared to ", baselevel, ", on average.", sig.text, " (z=", round(z.value, 4), " p=",  p.value,  ").", "\n", sep="")
        }
        
        
        first.part <- paste(first.part, adding.text, sep="<br/>")
      }
    }
    
    
    if(any(grepl(x = model$call$formula, pattern = "*"))|any(grepl(x = model$call$formula, pattern = ":"))){
      second.part <- paste("<br/><strong>Note:</strong> This model has interactions so these interpretations are based on marginal effects, which are partial 
          derivatives of the regression equation with respect to each variable. These marginal effects are calculated as 
          the average change across observations. Calculating marginal effects at representative prespecified values is 
          supported in R, but not currently supported in this application.")
    }
    HTML(paste(first.part, second.part, sep="<br/>"))
  }
  
  
  #############################################################################################
  # ggemmeans plot
  #############################################################################################
  # Code to create ggemmeanss plot
  ggemmeansplot <- metaReactive2({

    req(globalVars$model)
    model <- globalVars$model
    
    dat <- globalVars$dataset
    
    se.on = ifelse(input$interaction.error==TRUE, 0.40, 0)
    
    interacts<-input$var_inter
    
    interact.vars <- strsplit(x = interacts, split = "[:]")[[1]]
    int.var <-interact.vars[which(interact.vars != input$var_moderator)]
    moderator<-input$var_moderator
    
    int.vars.classes<-sapply(X = dat[,interact.vars], FUN = class)
    
    if(globalVars$transform.type == "none"){
      y.label <- paste("Predicted ", names(model$model)[1],sep="")
    }else if(globalVars$transform.type == "log"){
      y.label <- paste("Predicted log(", substring(names(model$model)[1], first=17), ")", sep="")  
    }else if(globalVars$transform.type == "lp1"){
      y.label <- paste("Predicted log(", substring(names(model$model)[1], first=17), " + 1)", sep="")
    }else if(globalVars$transform.type == "ihs"){
      y.label <- paste("Predicted IHS(", substring(names(model$model)[1], first=17), ")", sep="")
    }
    
    if(all(int.vars.classes=="numeric")){ #Both Continuous
      shinyjs::show("emtrendsdiv")
      shinyjs::show("emtrendscontrastdiv")
      shinyjs::show("jndiv")
      shinyjs::show("interaction.error")
      metaExpr({
        "####################################"
        "# Effects of Interest"
        "####################################"
        m.mod<-mean(unlist(model$model[..(moderator)]), na.rm=T)
        s.mod<-sd(unlist(model$model[..(moderator)]), na.rm=T)
        meffectsfor <- c(..(int.var), paste(..(moderator), "[",round(m.mod-s.mod,2), ",", round(m.mod+s.mod,2),"]", sep=""))
        mod.emmeans<-ggemmeans(model = model, terms = meffectsfor , interval = "confidence") # checkbox
        mod.emmeans <- ggemmeans(model = model, terms = meffectsfor, interval = "confidence")
        ggdat <- data.frame(mod.emmeans) %>%
          mutate(group = case_when(group == round(m.mod - s.mod, 2) ~ paste("Low (Mean - 1SD = ", round(m.mod - s.mod, 2), ")", sep = ""),
                                   TRUE                             ~ paste("High (Mean + 1SD = ", round(m.mod + s.mod, 2), ")", sep = "")))
        "####################################"
        "# Plot"
        "####################################"
        ggplot(data = ggdat, aes(x = x, y = predicted, color = group, fill = group)) +
          geom_line() +
          geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
                      linetype="dotted",
                      alpha = ifelse(..(se.on), 0.4, 0)) +
          xlab(..(int.var))+
          ylab(..(y.label))+
          scale_color_brewer(..(moderator), palette = "Pastel1")+
          scale_fill_brewer(..(moderator), palette = "Pastel1")+
          theme_bw()
      })
    }else if(all(int.vars.classes=="factor")){ #Both Factors
      shinyjs::hide("emtrendsdiv")
      shinyjs::hide("emtrendscontrastdiv")
      shinyjs::hide("jndiv")
      shinyjs::hide("interaction.error")
      metaExpr({
        "####################################"
        "# Effects of Interest"
        "####################################"
        #mod.emmeans<-ggemmeans(model = model, terms = c(..(int.var), ..(moderator)) , interval = "prediction") # checkbox
        mod.emmeans<-ggemmeans(model = model, terms = c(..(int.var), ..(moderator)) , interval = "confidence") # checkbox
        
        "####################################"
        "# Plot"
        "####################################"
        ggdat <- data.frame(mod.emmeans)
        ggplot(data=ggdat, aes(x=x, y=predicted, color=group))+
          geom_point(position = position_dodge(.25))+
          geom_errorbar(aes(ymin=conf.low, ymax=conf.high), position = position_dodge(.25), width=0.1)+
          xlab(..(int.var))+
          ylab(..(y.label))+
          scale_color_brewer(..(moderator), palette = "Pastel1")+
          scale_fill_brewer(..(moderator), palette = "Pastel1")+
          theme_bw()
      })
    }else{ #One of Each
      if(int.vars.classes[moderator]=="factor"){
        shinyjs::show("emtrendsdiv")
        shinyjs::show("emtrendscontrastdiv")
        shinyjs::hide("jndiv")
        shinyjs::show("interaction.error")
        metaExpr({
          "####################################"
          "# Effects of Interest"
          "####################################"
          #mod.emmeans<-ggemmeans(model = model, terms = meffectsfor , interval = "prediction") # to select
          mod.emmeans<-ggemmeans(model = model, terms = c(..(int.var),..(moderator)) , interval = "confidence") # checkbox
          
          "####################################"
          "# Plot"
          "####################################"
          ggdat <- data.frame(mod.emmeans)
          ggplot(data = ggdat, aes(x = x, y = predicted, color = group, fill = group)) +
            geom_line() +
            geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
                        linetype="dotted",
                        alpha = ifelse(..(se.on), 0.4, 0)) +
            xlab(..(int.var))+
            ylab(..(y.label))+
            scale_color_brewer(..(moderator), palette = "Pastel1")+
            scale_fill_brewer(..(moderator), palette = "Pastel1")+
            theme_bw()
        })
      }else{
        shinyjs::hide("emtrendsdiv")
        shinyjs::hide("emtrendscontrastdiv")
        shinyjs::hide("jndiv")
        shinyjs::hide("interaction.error")
        metaExpr({
          "####################################"
          "# Effects of Interest"
          "####################################"
          m.mod<-mean(unlist(model$model[..(moderator)]), na.rm=T)
          s.mod<-sd(unlist(model$model[..(moderator)]), na.rm=T)
          
          meffectsfor <- c(..(int.var), paste(..(moderator), "[",round(m.mod-s.mod,2), ",", round(m.mod+s.mod,2),"]", sep=""))
          #mod.emmeans<-ggemmeans(model = model, terms = meffectsfor , interval = "prediction") # to select
          mod.emmeans<-ggemmeans(model = model, terms = meffectsfor , interval = "confidence") # checkbox
          
          mod.emmeans <- mod.emmeans %>% 
            mutate(group=ifelse(group == round(m.mod-s.mod,2), paste("Low (Mean - 1SD = ", round(m.mod-s.mod,2), ")", sep=""),
                                paste("High (Mean + 1SD = ", round(m.mod+s.mod,2), ")", sep="")))
          "####################################"
          "# Plot"
          "####################################"
          ggdat <- data.frame(mod.emmeans)
          ggplot(data=ggdat, aes(x=x, y=predicted, color=group))+
            geom_point(position = position_dodge(.25))+
            geom_errorbar(aes(ymin=conf.low, ymax=conf.high), position = position_dodge(.25), width=0.1)+
            xlab(..(int.var))+
            ylab(..(y.label))+
            scale_color_brewer(..(moderator), palette = "Pastel1")+
            scale_fill_brewer(..(moderator), palette = "Pastel1")+
            theme_bw()
        })
      }
    }
  },inline=TRUE)
  
  # Render plot to UI
  output$ggemmeans_plot <- renderPlot({
    globalVars$ggemmeansplot
  })
  
  # Download button for plots (call reactive function here to get plot object) ----
  output$downloadggemmeansPlot <- downloadHandler(
    filename = function() { paste('ggemmeansplot.', input$ggemmeans_plot_format, sep='') },
    content = function(file) {
      ggsave(file, plot = globalVars$ggemmeansplot, device = input$ggemmeans_plot_format, 
             height = as.numeric(input$ggemmeans_plot_height), width = as.numeric(input$ggemmeans_plot_width), 
             units = input$ggemmeans_plot_units)
    }
  )
  
  
  # Rcode 
  # Display code for ci visualization plot ----
  observeEvent(input$code_ggemmeans, {
    code <- expandChain(
      "# Ensure to load your data as an object called dat.",
      quote({
        library(tidyverse) 
        library(ggeffects)
      }),
      "####################################",
      "# Load Data",
      "####################################",
      read_data(),
      refactor_data(),
      prepare_transformed_data(),
      prepare_scaled_data(),
      fitmodel(),
      ggemmeansplot()
    )
    
    displayCodeModal(
      code, 
      title = "Interaction Marginal Effects Plot",
      size = "l", 
      fontSize = 16,
      clip=NULL
    )
  })
  
  #############################################################################################
  # emmeans table
  #############################################################################################
  prepare_interaction_emmeans <- metaReactive2({
    req(input$var_inter)
    req(input$var_moderator)
    
    model<-globalVars$model
    
    dat <- globalVars$dataset
    interacts<-input$var_inter
    
    interact.vars <- strsplit(x = interacts, split = "[:]")[[1]]
    int.var <-interact.vars[which(interact.vars != input$var_moderator)]
    moderator<-input$var_moderator
    
    int.vars.classes<-sapply(X = dat[,interact.vars], FUN = class)
    if(all(int.vars.classes=="numeric")){ #Both Continuous
      metaExpr({
        "####################################"
        "# Calculate Marginal Effects"
        "####################################"
        m.mod<-mean(unlist(model$model[..(moderator)]), na.rm=T)
        s.mod<-sd(unlist(model$model[..(moderator)]), na.rm=T)
        m.var<-mean(unlist(model$model[..(int.var)]), na.rm=T)
        s.var<-sd(unlist(model$model[..(int.var)]), na.rm=T)
        modvarat<-list(c(round(m.mod-s.mod,2), round(m.mod+s.mod,2)),
                       c(round(m.var-s.var,2), round(m.var+s.var,2)))
        names(modvarat)<-c((..(moderator)), ..(int.var))
        
        mod.emmeans<-data.frame(emmeans(object = model, spec=c(..(int.var),..(moderator)), var=..(int.var), at=modvarat, type="response")) 
        
        "####################################"
        "# Clean Up Labels for Printing"
        "####################################"
        mod.emmeans[,..(int.var)]<- ifelse(mod.emmeans[,..(int.var)]==round(m.var-s.var,2),     paste("Low (Mean - 1SD = ", round(m.var-s.var,2), ")", sep=""),paste("High (Mean + 1SD = ", round(m.var+s.var,2), ")", sep=""))
        mod.emmeans[,..(moderator)]<- ifelse(mod.emmeans[,..(moderator)]==round(m.mod-s.mod,2), paste("Low (Mean - 1SD = ", round(m.mod-s.mod,2), ")", sep=""),paste("High (Mean + 1SD = ", round(m.mod+s.mod,2), ")", sep=""))
        
        mod.emmeans <- mod.emmeans %>%
          set_rownames(NULL) %>%
          set_colnames(c(..(moderator),..(int.var), "Estimated Marginal Mean", "SE", "df", "Lower CI", "Upper CI"))
      })
    }else if(all(int.vars.classes=="factor")){ #Both Factors
      metaExpr({
        "####################################"
        "# Calculate Marginal Effects"
        "####################################"
        mod.emmeans<-data.frame(emmeans(object = model, spec=c(..(int.var),..(moderator)), var=..(int.var), type="response")) 
        
        "####################################"
        "# Clean Up Labels for Printing"
        "####################################"
        mod.emmeans <- mod.emmeans %>%
          set_rownames(NULL) %>%
          set_colnames(c(..(moderator),..(int.var),"Estimated Marginal Mean", "SE", "df", "Lower CI", "Upper CI"))
      })
    }else{ 
      if(int.vars.classes[moderator]=="factor"){
        metaExpr({
          "####################################"
          "# Calculate Marginal Effects"
          "####################################"  
          m.var<-mean(unlist(model$model[..(int.var)]), na.rm=T)
          s.var<-sd(unlist(model$model[..(int.var)]), na.rm=T)
          modvarat<-list(c(round(m.var-s.var,2), round(m.var+s.var,2)))
          names(modvarat)<-..(int.var)
          
          mod.emmeans<-data.frame(emmeans(model, specs = c(moderator, int.var), var = int.var, at=modvarat, type="response")) 
          
          
          "####################################"
          "# Clean Up Labels for Printing"
          "####################################"
          mod.emmeans[,..(int.var)]<- ifelse(mod.emmeans[,..(int.var)]==round(m.var-s.var,2),paste("Low (Mean - 1SD = ", round(m.var-s.var,2), ")", sep=""),paste("High (Mean + 1SD = ", round(m.var+s.var,2), ")", sep=""))
          
          mod.emmeans <- mod.emmeans %>%
            set_rownames(NULL) %>%
            set_colnames(c(..(moderator),..(int.var),"Estimated Marginal Mean", "SE", "df", "Lower CI", "Upper CI"))
        })
      }else{
        model<-globalVars$model
        metaExpr({
          "####################################"
          "# Calculate Marginal Effects"
          "####################################"
          m.mod<-mean(unlist(model$model[..(moderator)]), na.rm=T)
          s.mod<-sd(unlist(model$model[..(moderator)]), na.rm=T)
          
          modvarat<-list(c(round(m.mod-s.mod,2), round(m.mod+s.mod,2)))
          names(modvarat)<-c(..(moderator))
          
          mod.emmeans<-data.frame(emmeans(model, specs = c(..(moderator),..(int.var)), var=..(moderator), at=modvarat, type="response")) 
          
          
          "####################################"
          "# Clean Up Labels for Printing"
          "####################################"
          mod.emmeans[,..(moderator)]<- ifelse(mod.emmeans[,..(moderator)]==round(m.mod-s.mod,2),paste("Low (Mean - 1SD = ", round(m.mod-s.mod,2), ")", sep=""),paste("High (Mean + 1SD = ", round(m.mod+s.mod,2), ")", sep=""))
          
          mod.emmeans <- mod.emmeans %>%
            set_rownames(NULL) %>%
            set_colnames(c(..(moderator),..(int.var),"Estimated Marginal Mean", "SE", "df", "Lower CI", "Upper CI"))
        })
      }
    }
  }, inline=TRUE)
  
  make_interaction_emmeans_tab <- metaReactive2({
    mod.emmeans <- globalVars$emmeans
    metaExpr({
      mod.emmeans %>%
        mutate_if(is.numeric, round, 4) 
    })
    
  }, inline=TRUE)
  
  ### Model Summary
  output$interaction_emmeans_tab <- DT::renderDataTable({
    globalVars$make_interaction_emmeans_tab
  },
  extensions = 'Buttons',
  options = list(
    dom = 'Bfrtip',
    buttons = 
      list("copy", "print", list(
        extend = "collection",
        buttons = list(
          list(extend="csv", filename="model-interaction-marginalmeans"),
          list(extend="excel", filename="model-interaction-marginalmeans"),
          list(extend="pdf", filename="model-interaction-marginalmeans")
        ),
        text = "Download",
        filename = "model-interaction-marginalmeans"
      ))
  ), rownames = FALSE)
  
  # Download button for summary (LaTeX version)----
  output$download_interaction_emmeansLatex <- downloadHandler(
    filename = function() {
      "model-interaction-marginalmeans.tex"
    },
    content = function(file) {
      base::print(xtable(globalVars$make_interaction_emmeans_tab, digits = 4), file, type = "latex")
    }
  )
  
  # R Code
  observeEvent(input$code_interaction_emmeanstab, {
    code <- expandChain(
      "# Ensure to load your data as an object called dat.",
      quote({
        library(tidyverse)
        library(magrittr)
        library(emmeans)
      }),
      "####################################",
      "# Load Data",
      "####################################",
      read_data(),
      refactor_data(),
      prepare_transformed_data(),
      prepare_scaled_data(),
      fitmodel(),
      prepare_interaction_emmeans(),
      make_interaction_emmeans_tab()
    )

    displayCodeModal(
      code,
      title = "Interaction Estimated Marginal Means Table",
      size = "l",
      fontSize = 16,
      clip=NULL
    )
  })
  
  #Interpretation
  output$interaction_emmeans_interp <- renderUI({
    globalVars$prepare_interaction_emmeans_interp
  })
  
  prepare_interaction_emmeans_interp <- function(){
    mod.emmeans <- globalVars$emmeans %>%
      mutate_if(is.numeric, round, 4) 
    model<-globalVars$model
    dat<-globalVars$dataset
    
    interacts<-input$var_inter
    
    interact.vars <- strsplit(x = interacts, split = "[:]")[[1]]
    int.var <-interact.vars[which(interact.vars != input$var_moderator)]
    moderator<-input$var_moderator
    
    int.vars.classes<-sapply(X = dat[,interact.vars], FUN = class)
    
    
    if(globalVars$transform.type == "none"){
      response <- names(model$model)[1]
    }else if(globalVars$transform.type == "log"){
      response <- paste("log(", substring(names(model$model)[1], first=17), ")", sep="")  
    }else if(globalVars$transform.type == "lp1"){
      response <- paste("log(", substring(names(model$model)[1], first=17), " + 1)", sep="")
    }else if(globalVars$transform.type == "ihs"){
      response <- paste("inverse hyperbolic sine transformed ", substring(names(model$model)[1], first=17), sep="")
    }
    
    
    emmeans.text<-""
    if(all(int.vars.classes=="numeric")){ #Both Continuous
      for(i in 1:nrow(mod.emmeans)){
        emmeans.text <- paste(emmeans.text,"\U2022 The estimated marginal mean of ", sub("\\.scaled$", "", names(model$model)[1]), " for ", sub("\\.scaled$", "", int.var), " = ", mod.emmeans[i,int.var] ," and ", sub("\\.scaled$", "", colnames(mod.emmeans[moderator])), " = " , as.character(mod.emmeans[i,moderator]), " is ", round(mod.emmeans[i,"Estimated Marginal Mean"],2),
                              " (95% CI: ", round(mod.emmeans[i,"Lower CI"],2), ", ", round(mod.emmeans[i,"Upper CI"],2), "). <br/>",
                              sep="")
      }
    }else if(all(int.vars.classes=="factor")){ #Both Factors
      for(i in 1:nrow(mod.emmeans)){
        emmeans.text <- paste(emmeans.text,"\U2022 The estimated marginal mean of ", sub("\\.scaled$", "", response), " for ", sub("\\.scaled$", "", int.var), " = ", as.character(mod.emmeans[i,int.var]) ," and ", sub("\\.scaled$", "", colnames(mod.emmeans[moderator])), " = " , as.character(mod.emmeans[i,moderator]), " is ", round(mod.emmeans[i,"Estimated Marginal Mean"],2),
                              " (95% CI: ", round(mod.emmeans[i,"Lower CI"],2), ", ", round(mod.emmeans[i,"Upper CI"],2), "). <br/>",
                              sep="")
      }
    }else{
      if(int.vars.classes[moderator]=="factor"){
        for(i in 1:nrow(mod.emmeans)){
          emmeans.text <- paste(emmeans.text, "\U2022 The estimated marginal mean of ", sub("\\.scaled$", "", response), " for ", sub("\\.scaled$", "", int.var), " = ", mod.emmeans[i,int.var] ," and ", sub("\\.scaled$", "", colnames(mod.emmeans[moderator])), " = " , as.character(mod.emmeans[i,moderator]), " is ", round(mod.emmeans[i,"Estimated Marginal Mean"],2),
                                " (95% CI: ", round(mod.emmeans[i,"Lower CI"],2), ", ", round(mod.emmeans[i,"Upper CI"],2), "). <br/>",
                                sep="")
        }
      }else{
        for(i in 1:nrow(mod.emmeans)){
          emmeans.text <- paste(emmeans.text,"\U2022 The estimated marginal mean of ", sub("\\.scaled$", "", response), " for ", sub("\\.scaled$", "", int.var), " = ", as.character(mod.emmeans[i,int.var]) ," and ", sub("\\.scaled$", "", colnames(mod.emmeans[moderator])), " = " , as.character(mod.emmeans[i,moderator]), " is ", round(mod.emmeans[i,"Estimated Marginal Mean"],2),
                                " (95% CI: ", round(mod.emmeans[i,"Lower CI"],2), ", ", round(mod.emmeans[i,"Upper CI"],2), "). <br/>",
                                sep="")
        }
      }
    }
    emmeans.text<-paste(emmeans.text, "<br/><strong>Note:</strong> This approach contrasts the estimated marginal means with a Tukey adjustment for multiple comparisons. These values are calculated at the 'average' of the other variables in the model.",
                        "You may want to set the other variables to specific values, which is supported in R but not currently supported in this application.")
    HTML(emmeans.text)
  }
  
  #############################################################################################
  # contrast table
  #############################################################################################
  prepare_interaction_emmeanscontrasts <- metaReactive2({
    
    dat <- globalVars$dataset
    model <- globalVars$model
    
    interacts<-input$var_inter
    
    interact.vars <- strsplit(x = interacts, split = "[:]")[[1]]
    int.var <-interact.vars[which(interact.vars != input$var_moderator)]
    moderator<-input$var_moderator
    
    int.vars.classes<-sapply(X = dat[,interact.vars], FUN = class)
    
    if(all(int.vars.classes=="numeric")){ #Both Continuous
      metaExpr({
        m.mod<-mean(unlist(model$model[..(moderator)]), na.rm=T)
        s.mod<-sd(unlist(model$model[..(moderator)]), na.rm=T)
        m.var<-mean(unlist(model$model[..(int.var)]), na.rm=T)
        s.var<-sd(unlist(model$model[..(int.var)]), na.rm=T)
        modvarat<-list(c(round(m.mod-s.mod,2), round(m.mod+s.mod,2)),
                       c(round(m.var-s.var,2), round(m.var+s.var,2)))
        names(modvarat)<-c((..(moderator)), ..(int.var))
        
        mod.emmeans<-emmeans(object = model, spec=c(..(int.var),..(moderator)), var=..(int.var), at=modvarat, type="response")
        mod.emmeans <- add_grouping(mod.emmeans, "int.var", ..(int.var) , c(paste("(Low ", ..(int.var),")", sep=""), paste("(High ", ..(int.var),")", sep="")))
        mod.emmeans <- add_grouping(mod.emmeans, "moderator", ..(moderator) , c(paste("(Low ", ..(moderator),")", sep=""), paste("(High ", ..(moderator),")", sep="")))
        
        mod.emmeanscontrast <-data.frame(pairs(emmeans(mod.emmeans, c("int.var","moderator"), type="response")))
        mod.emmeanscontrastci<-data.frame(confint(pairs(mod.emmeans)))
        mod.emmeanscontrast$lower.CL<-mod.emmeanscontrastci$lower.CL
        mod.emmeanscontrast$upper.CL<-mod.emmeanscontrastci$upper.CL
        
        mod.emmeanscontrast$contrast <- gsub("\\.scaled", "", mod.emmeanscontrast$contrast)
        
        "####################################"
        "# Clean Up Labels for Printing"
        "####################################"
        mod.emmeanscontrast <- mod.emmeanscontrast %>%
          dplyr::select(-any_of("null"))%>%
          set_rownames(NULL) %>%
          set_colnames(c("Contrast", "Estimate", "SE", "df", "t ratio", "p-value", "Lower CI", "Upper CI"))
      }) 
    }else if(all(int.vars.classes=="factor")){ #Both Factors
      metaExpr({
        "####################################"
        "# Calculate Marginal Effects"
        "####################################"
        mod.emmeans<-emmeans(model, specs = c(..(int.var),..(moderator)), var=moderator, type="response")
        mod.emmeanscontrast <-data.frame(pairs(mod.emmeans))
        mod.emmeanscontrastci<-data.frame(confint(pairs(mod.emmeans)))
        mod.emmeanscontrast$lower.CL<-mod.emmeanscontrastci$lower.CL
        mod.emmeanscontrast$upper.CL<-mod.emmeanscontrastci$upper.CL
        
        mod.emmeanscontrast$contrast <- gsub("\\.scaled", "", mod.emmeanscontrast$contrast)
        
        "####################################"
        "# Clean Up Labels for Printing"
        "####################################"
        mod.emmeanscontrast <- mod.emmeanscontrast %>%
          dplyr::select(-any_of("null"))%>%
          set_rownames(NULL) %>%
          set_colnames(c("Contrast", "Estimate", "SE", "df", "t ratio", "p-value", "Lower CI", "Upper CI"))
      })    
    }else{ 
      if(int.vars.classes[moderator]=="factor"){
        metaExpr({
          "####################################"
          "# Calculate Marginal Effects"
          "####################################"
          m<-mean(unlist(model$model[..(int.var)]), na.rm=T)
          s<-sd(unlist(model$model[..(int.var)]), na.rm=T)
          modvarat<-list(c(round(m-s,2), round(m+s,2)))
          names(modvarat)<-c(..(int.var))
          
          mod.emmeans <- emmeans(model, specs = c(..(moderator),..(int.var)), var=..(moderator), at=modvarat, type="response")
          mod.emmeans <- add_grouping(mod.emmeans, "int.var", ..(int.var) ,  c(paste("(Low ",..(int.var),")", sep=""), paste("(High ",..(int.var),")", sep="")))
          mod.emmeanscontrast <-data.frame(pairs(mod.emmeans))
          mod.emmeanscontrastci<-data.frame(confint(pairs(mod.emmeans)))
          mod.emmeanscontrast$lower.CL<-mod.emmeanscontrastci$lower.CL
          mod.emmeanscontrast$upper.CL<-mod.emmeanscontrastci$upper.CL
          
          mod.emmeanscontrast$contrast <- gsub("\\.scaled", "", mod.emmeanscontrast$contrast)
          
          "####################################"
          "# Clean Up Labels for Printing"
          "####################################"
          mod.emmeanscontrast <- mod.emmeanscontrast %>%
            dplyr::select(-any_of("null"))%>%
            set_rownames(NULL) %>%
            set_colnames(c("Contrast", "Estimate", "SE", "df", "t ratio", "p-value", "Lower CI", "Upper CI"))
        })
      }else{
        metaExpr({
          "####################################"
          "# Calculate Marginal Effects"
          "####################################"
          m<-mean(unlist(model$model[..(moderator)]), na.rm=T)
          s<-sd(unlist(model$model[..(moderator)]), na.rm=T)
          
          modvarat<-list(c(round(m-s,2), round(m+s,2)))
          names(modvarat)<-c(..(moderator))
          
          mod.emmeans <- emmeans(model, specs = c(..(moderator), ..(int.var)), var=..(moderator), at=modvarat, type="response")
          mod.emmeans <- add_grouping(mod.emmeans, "moderator", ..(moderator) ,  c(paste("(Low ",..(moderator),")", sep=""), paste("(High ",..(moderator),")")))
          mod.emmeanscontrast <-data.frame(pairs(mod.emmeans))
          
          mod.emmeanscontrastci<-data.frame(confint(pairs(mod.emmeans)))
          mod.emmeanscontrast$lower.CL<-mod.emmeanscontrastci$lower.CL
          mod.emmeanscontrast$upper.CL<-mod.emmeanscontrastci$upper.CL
          
          mod.emmeanscontrast$contrast <- gsub("\\.scaled", "", mod.emmeanscontrast$contrast)
          
          "####################################"
          "# Clean Up Labels for Printing"
          "####################################"
          mod.emmeanscontrast <- mod.emmeanscontrast %>%
            dplyr::select(-any_of("null"))%>%
            set_rownames(NULL) %>%
            set_colnames(c("Contrast", "Estimate", "SE", "df", "t ratio", "p-value", "Lower CI", "Upper CI"))
        })
      }
    }
  }, inline=TRUE)
  
  # Code to create contrast table
  make_interaction_contrasts_tab<- metaReactive2({
    
    mod.emmeanscontrast <- globalVars$mod.emmeanscontrast
    metaExpr({
      "####################################"
      "# Print Table"
      "####################################"
      mod.emmeanscontrast %>%
        mutate_if(is.numeric, round, 4) %>%
        mutate(`p-value` = ifelse(`p-value` < 0.0001, "<0.0001", `p-value`))
    })
  }, inline=TRUE)
  
  # Render Table to UI
  output$interaction_emmeanscontrast_tab <- DT::renderDataTable({
    globalVars$make_interaction_contrasts_tab
  },
  extensions = 'Buttons',
  options = list(
    dom = 'Bfrtip',
    buttons = 
      list("copy", "print", list(
        extend = "collection",
        buttons = list(
          list(extend="csv", filename="model-interaction-contrasts"),
          list(extend="excel", filename="model-interaction-contrasts"),
          list(extend="pdf", filename="model-interaction-contrasts")
        ),
        text = "Download",
        filename = "model-interaction-contrasts"
      ))
  ), rownames = FALSE)
  
  # Download button for summary (LaTeX version)----
  output$download_interaction_emmeanscontrastLatex <- downloadHandler(
    filename = function() {
      "model-interaction-contrasts.tex"
    },
    content = function(file) {
      base::print(xtable(globalVars$make_interaction_contrasts_tab, digits = 4), file, type = "latex")
    }
  )
  
  # R Code
  observeEvent(input$code_interaction_emmeans_contrast, {
    code <- expandChain(
      "# Ensure to load your data as an object called dat.",
      quote({
        library(tidyverse)
        library(magrittr)
        library(emmeans)
      }),
      "####################################",
      "# Load Data",
      "####################################",
      read_data(),
      refactor_data(),
      prepare_transformed_data(),
      prepare_scaled_data(),
      fitmodel(),
      prepare_interaction_emmeanscontrasts(),
      make_interaction_contrasts_tab()
    )
    
    displayCodeModal(
      code,
      title = "Interaction Estimated Marginal Means Contrasts Table",
      size = "l",
      fontSize = 16,
      clip=NULL
    )
  })
  
  # Interpretation
  output$interaction_emmeanscontrast_interp <- renderPrint({
    globalVars$prepare_interaction_emmeanscontrast_interp
  })
  
  prepare_interaction_emmeanscontrast_interp <- function(){
    mod.emmeanscontrast <- globalVars$mod.emmeanscontrast
    model <- globalVars$model
    dat <- globalVars$dataset
    alpha<-input$alpha
    
    
    interacts<-input$var_inter
    
    interact.vars <- strsplit(x = interacts, split = "[:]")[[1]]
    int.var <-interact.vars[which(interact.vars != input$var_moderator)]
    moderator<-input$var_moderator
    
    int.vars.classes<-sapply(X = dat[,interact.vars], FUN = class)
    
    if(globalVars$transform.type == "none"){
      response <- names(model$model)[1]
    }else if(globalVars$transform.type == "log"){
      response <- paste("log(", substring(names(model$model)[1], first=17), ")", sep="")  
    }else if(globalVars$transform.type == "lp1"){
      response <- paste("log(", substring(names(model$model)[1], first=17), " + 1)", sep="")
    }else if(globalVars$transform.type == "ihs"){
      response <- paste("inverse hyperbolic sine transformed ", substring(names(model$model)[1], first=17), sep="")
    }
    
    if(all(int.vars.classes=="numeric")){ #Both Continuous
      text <- ""
      for(i in 1:nrow(mod.emmeanscontrast)){
        adding.text <- paste("\U2022 The contrast of estimated marginal means of ", sub("\\.scaled$", "", response), " for", " (", gsub("\\.scaled", "", mod.emmeanscontrast$Contrast[i]),") is",
                             ifelse(mod.emmeanscontrast[i,"p-value"]<alpha, " significant ", " not significant "), "(t = ", round(mod.emmeanscontrast[i,"t ratio"],2),
                             ", df = ", round(mod.emmeanscontrast[i,"df"]), ", p-value ", ifelse(mod.emmeanscontrast[i,"p-value"]<0.0001,"< 0.0001", paste("= ",round(mod.emmeanscontrast[i,"p-value"],4), sep="")),
                             "; 95% CI: ", round(mod.emmeanscontrast[i,"Lower CI"],2), ", ", round(mod.emmeanscontrast[i,"Upper CI"],2), ").",
                             sep="")
        text <- paste(text, adding.text, sep="<br>")
      }
      HTML(text)
    }else if(all(int.vars.classes=="factor")){ #Both Factors
      text <- ""
      for(i in 1:nrow(mod.emmeanscontrast)){
        adding.text <- paste("\U2022 The contrast of estimated marginal means of ", sub("\\.scaled$", "", response), " for", " (", gsub("\\.scaled", "", mod.emmeanscontrast$Contrast[i]),") is",
                             ifelse(mod.emmeanscontrast[i,"p-value"]<alpha, " significant ", " not significant "), "(t = ", round(mod.emmeanscontrast[i,"t ratio"],2),
                             ", df = ", round(mod.emmeanscontrast[i,"df"]), ", p-value ", ifelse(mod.emmeanscontrast[i,"p-value"]<0.0001,"< 0.0001", paste("= ",round(mod.emmeanscontrast[i,"p-value"],4), sep="")),
                             "; 95% CI: ", round(mod.emmeanscontrast[i,"Lower CI"],2), ", ", round(mod.emmeanscontrast[i,"Upper CI"],2), ").",
                             sep="")
        text <- paste(text, adding.text, sep="<br>")
      }
      HTML(text)
    }else{
      if(int.vars.classes[moderator]=="factor"){
        text <- ""
        for(i in 1:nrow(mod.emmeanscontrast)){
          adding.text <- paste("\U2022 The contrast of estimated marginal means of ", sub("\\.scaled$", "", response), " for", " (", gsub("\\.scaled", "", as.character(mod.emmeanscontrast$Contrast[i])),") when (", int.var, " = ", mod.emmeanscontrast[i,int.var], ") is",
                               ifelse(mod.emmeanscontrast[i,"p-value"]<alpha, " significant ", " not significant "), "(t = ", round(mod.emmeanscontrast[i,"t ratio"],2),
                               ", df = ", round(mod.emmeanscontrast[i,"df"]), ", p-value ", ifelse(mod.emmeanscontrast[i,"p-value"]<0.0001,"< 0.0001", paste("= ",round(mod.emmeanscontrast[i,"p-value"],4), sep="")),
                               "; 95% CI: ", round(mod.emmeanscontrast[i,"Lower CI"],2), ", ", round(mod.emmeanscontrast[i,"Upper CI"],2), ").",
                               sep="")
          text <- paste(text, adding.text, sep="<br>")
        }
        HTML(text)
      }else{
        text <- ""
        for(i in 1:nrow(mod.emmeanscontrast)){
          adding.text <- paste("\U2022 The contrast of estimated marginal means of ", sub("\\.scaled$", "", response), " for", " (", gsub("\\.scaled", "", mod.emmeanscontrast$Contrast[i]),") is",
                               ifelse(mod.emmeanscontrast[i,"p-value"]<alpha, " significant ", " not significant "), "(t = ", round(mod.emmeanscontrast[i,"t ratio"],2),
                               ", df = ", round(mod.emmeanscontrast[i,"df"]), ", p-value ", ifelse(mod.emmeanscontrast[i,"p-value"]<0.0001,"< 0.0001", paste("= ",round(mod.emmeanscontrast[i,"p-value"],4), sep="")),
                               "; 95% CI: ", round(mod.emmeanscontrast[i,"Lower CI"],2), ", ", round(mod.emmeanscontrast[i,"Upper CI"],2), ").",
                               sep="")
          text <- paste(text, adding.text, sep="<br>")
        }
        HTML(text)
      }
    }
  }
  
  #############################################################################################
  # marginal effects table 
  #############################################################################################
  prepare_interaction_emtrends<- metaReactive2({
    
    model <-globalVars$model
    dat <- globalVars$dataset
    
    interacts<-input$var_inter
    
    interact.vars <- strsplit(x = interacts, split = "[:]")[[1]]
    int.var <-interact.vars[which(interact.vars != input$var_moderator)]
    moderator<-input$var_moderator
    
    int.vars.classes<-sapply(X = dat[,interact.vars], FUN = class)
    
    if(all(int.vars.classes=="numeric")){ #Both Continuous
      metaExpr({
        "####################################"
        "# Calculate Marginal Effects"
        "####################################"
        m<-mean(unlist(model$model[..(moderator)]), na.rm=T)
        s<-sd(unlist(model$model[..(moderator)]), na.rm=T)
        modvarat<-list(c(round(m-s,2), round(m+s,2)))
        names(modvarat)<-c(..(moderator))
        
        mod.emtrends<-data.frame(emtrends(object = model, spec=..(moderator), var=..(int.var), at=modvarat))
        mod.emtrends[,..(moderator)]<- ifelse(mod.emtrends[,..(moderator)]==round(m-s,2),"Low (Mean - 1SD)", "High (Mean + 1SD)")
        
        "####################################"
        "# Clean Up Labels for Printing"
        "####################################"
        colnames(mod.emtrends)[-(1)]<-c(paste("Slope of", ..(int.var)), "SE", "df", "Lower CI", "Upper CI")
        emtrend.test <-test(emtrends(object = model, spec=..(moderator), var=..(int.var), at=modvarat))
        mod.emtrends<-mod.emtrends %>% mutate("p-value"= emtrend.test$p.value,
                                              "t ratio"= emtrend.test$t.ratio) %>%
          relocate(c(`t ratio`, `p-value`), .after=df)
        
      })
    }else if(all(int.vars.classes=="factor")){ #Both Factors
      # HIDE emtrends_tab, emtrends_int
    }else{
      if(int.vars.classes[moderator]=="factor"){
        metaExpr({
          "####################################"
          "# Calculate Marginal Effects"
          "####################################"
          mod.emtrends<-data.frame(emtrends(model, specs = ..(moderator), var = ..(int.var)))
          
          "####################################"
          "# Clean Up Labels for Printing"
          "####################################"
          colnames(mod.emtrends)[-(1)]<-c(paste("Slope of", ..(int.var)), "SE", "df", "Lower CI", "Upper CI")
          emtrend.test <-test(emtrends(model, specs = ..(moderator), var = ..(int.var)))
          mod.emtrends<-mod.emtrends %>% mutate("p-value"= emtrend.test$p.value,
                                                "t ratio"= emtrend.test$t.ratio)%>%
            relocate(c(`t ratio`, `p-value`), .after=df)
        })
      }else{
        # HIDE emtrends_tab, emtrends_int
      }
    }
  }, inline=TRUE)
  
  make_interaction_emtrends_tab <- metaReactive2({
    
    mod.emtrends<-globalVars$mod.emtrends
    
    metaExpr({
      "####################################"
      "# Print Table"
      "####################################"
      mod.emtrends %>%
        mutate_if(is.numeric, round, 4) %>%
        mutate(`p-value` = ifelse(`p-value` < 0.0001, "<0.0001", `p-value`))
    })
  }, inline=TRUE)
  
  ### Render Model Summary to UI
  output$interaction_emtrends_tab <- DT::renderDataTable({
    globalVars$make_interaction_emtrends_tab
  },
  extensions = 'Buttons',
  options = list(
    dom = 'Bfrtip',
    buttons = 
      list("copy", "print", list(
        extend = "collection",
        buttons = list(
          list(extend="csv", filename="model-interaction-marginaleffects"),
          list(extend="excel", filename="model-interaction-marginaleffects"),
          list(extend="pdf", filename="model-interaction-marginaleffects")
        ),
        text = "Download",
        filename = "model-interaction-marginaleffects"
      ))
  ), rownames = FALSE)
  
  # Download button for summary (LaTeX version)----
  output$download_interaction_emtrendsLatex <- downloadHandler(
    filename = function() {
      paste("model-interaction-marginaleffects.tex", sep="")
    },
    content = function(file) {
      base::print(xtable(globalVars$make_interaction_emtrends_tab, digits = 4), file, type = "latex")
    }
  )
  
  # R Code
  # Display code for ci visualization plot ----
  observeEvent(input$code_interaction_emtrendstab, {
    code <- expandChain(
      "# Ensure to load your data as an object called dat.",
      quote({
        library(tidyverse) 
        library(magrittr)
        library(emmeans)
      }),
      "####################################",
      "# Load Data",
      "####################################",
      read_data(),
      refactor_data(),
      prepare_transformed_data(),
      prepare_scaled_data(),
      fitmodel(),
      prepare_interaction_emtrends(),
      make_interaction_emtrends_tab()
    )
    
    displayCodeModal(
      code, 
      title = "Interaction Marginal Effects Table",
      size = "l", 
      fontSize = 16,
      clip=NULL
    )
  })
  
  
  output$interaction_emtrends_interp <- renderUI({  
    globalVars$prepare_interaction_emtrends_interp
  })
  
  prepare_interaction_emtrends_interp <- function(){
    mod.emtrends<-globalVars$mod.emtrends
    model<-globalVars$model
    dat = globalVars$dataset
    alpha <- input$alpha
    
    interacts<-input$var_inter
    
    interact.vars <- strsplit(x = interacts, split = "[:]")[[1]]
    int.var <-interact.vars[which(interact.vars != input$var_moderator)]
    moderator<-input$var_moderator
    
    int.vars.classes<-sapply(X = dat[,interact.vars], FUN = class)
    
    emtrends.interp<-""
    for(i in 1:nrow(mod.emtrends)){
      if(class(mod.emtrends[,1])=="factor" | class(mod.emtrends[,1])=="character"){
        emtrends.interp <- paste(emtrends.interp,"\U2022 The effect of ", gsub("\\.scaled", "", (int.var)),  " is ", ifelse(mod.emtrends[i,"p-value"]<alpha, "significant ", "not significant "),
                                 "when ", gsub("\\.scaled", "", (moderator)), " is ", mod.emtrends[i,1],
                                 " (Slope = ", round(mod.emtrends[i,2],4),
                                 ", t ratio = ", round(mod.emtrends[i,"t ratio"],4),
                                 ", p-value ",  ifelse(mod.emtrends[i,"p-value"]<0.0001,"< 0.0001", paste("= ", round(mod.emtrends[i,"p-value"],4), sep="")), ").<br/>",
                                 sep="")
      }else{
        emtrends.interp <- paste(emtrends.interp,"\U2022 The effect of ", gsub("\\.scaled", "", (int.var)),  " is ", ifelse(mod.emtrends[i,"p-value"]<alpha, "significant ", "not significant "),
                                 "when ", gsub("\\.scaled", "", (moderator)), " is ", round(mod.emtrends[i,1],4),
                                 " (Slope = ", round(mod.emtrends[i,2],4),
                                 ", t ratio = ", round(mod.emtrends[i,"t ratio"],4),
                                 ", p-value ",  ifelse(mod.emtrends[i,"p-value"]<0.0001,"< 0.0001", paste("= ", round(mod.emtrends[i,"p-value"],4), sep="")), ").<br/>",
                                 sep="")
      }
    }
    
    if(globalVars$transform.type == "none"){
      emtrends.interp<-paste(emtrends.interp, "<br/><strong>Note:</strong> These marginal effects are calculated at the 'average' of the other variables in the model.",
                             "You may want to set the other variables to specific values, which is supported in R but not currently supported in this application.")      
    }else if(globalVars$transform.type == "log"){
        emtrends.interp<-paste(emtrends.interp, "<br/><strong>Note:</strong> These effects are on the original log-transformed response. Further, note that these marginal effects are calculated at the 'average' of the other variables in the model.",
                               "You may want to set the other variables to specific values, which is supported in R but not currently supported in this application.")
    }else if(globalVars$transform.type == "lp1"){
      emtrends.interp<-paste(emtrends.interp, "<br/><strong>Note:</strong> These effects are on the original log-plus-1-transformed response. Further, note that these marginal effects are calculated at the 'average' of the other variables in the model.",
                             "You may want to set the other variables to specific values, which is supported in R but not currently supported in this application.")
    }else if(globalVars$transform.type == "ihs"){
      emtrends.interp<-paste(emtrends.interp, "<br/><strong>Note:</strong> These effects are on the original inverse hyperbolic sine transformed response. Further, note that these marginal effects are calculated at the 'average' of the other variables in the model.",
                             "You may want to set the other variables to specific values, which is supported in R but not currently supported in this application.")
    }
    
    HTML(emtrends.interp)
  }
  
  #############################################################################################
  # marginal effects contrast table 
  #############################################################################################
  prepare_interaction_emtcontrast<- metaReactive2({
    
    model <-globalVars$model
    dat <- globalVars$dataset
    
    interacts<-input$var_inter
    
    interact.vars <- strsplit(x = interacts, split = "[:]")[[1]]
    int.var <-interact.vars[which(interact.vars != input$var_moderator)]
    moderator<-input$var_moderator
    
    int.vars.classes<-sapply(X = dat[,interact.vars], FUN = class)
    if(all(int.vars.classes=="numeric")){ #Both Continuous
      metaExpr({
        "####################################"
        "# Calculate Marginal Effects Contrasts"
        "####################################"
        m.mod<-mean(unlist(model$model[..(moderator)]), na.rm=T)
        s.mod<-sd(unlist(model$model[..(moderator)]), na.rm=T)
        m.var<-mean(unlist(model$model[..(int.var)]), na.rm=T)
        s.var<-sd(unlist(model$model[..(int.var)]), na.rm=T)
        modvarat<-list(c(round(m.mod-s.mod,2), round(m.mod+s.mod,2)),
                       c(round(m.var-s.var,2), round(m.var+s.var,2)))
        names(modvarat)<-c((..(moderator)), ..(int.var))
        mod.emtrends <- emtrends(object = model, spec=..(moderator), var=..(int.var), at=modvarat)
        mod.emtrends <- add_grouping(mod.emtrends, "moderator", ..(moderator) , c(paste("(Low ", ..(moderator),")", sep=""), paste("(High ", ..(moderator),")", sep="")))
        mod.emtrends <- emmeans(mod.emtrends, spec="moderator", var=(int.var), at=modvarat)
        
        mod.emtrendcontrast <-data.frame(pairs(mod.emtrends))
        mod.emtrendcontrastci<-data.frame(confint(pairs(mod.emtrends)))
        mod.emtrendcontrast$lower.CL<-mod.emtrendcontrastci$lower.CL
        mod.emtrendcontrast$upper.CL<-mod.emtrendcontrastci$upper.CL
        
        mod.emtrendcontrast$contrast <- gsub("\\.scaled", "", mod.emtrendcontrast$contrast)
        
        "####################################"
        "# Clean Up Labels for Printing"
        "####################################"
        mod.emtrendcontrast <- mod.emtrendcontrast %>%
          set_rownames(NULL) %>%
          set_colnames(c("Contrast", "Estimate", "SE", "df", "t ratio", "p-value", "Lower CI", "Upper CI"))
      })
    }else if(all(int.vars.classes=="factor")){ #Both Factors
      # HIDE emtrends_tab, emtrends_int
    }else{
      if(int.vars.classes[moderator]=="factor"){
        metaExpr({
          "####################################"
          "# Calculate Marginal Effects Contrasts"
          "####################################"
          mod.emtrends <- emtrends(object = model, spec=..(moderator), var=..(int.var))
          mod.emtrendcontrast <-data.frame(pairs(mod.emtrends))
          
          mod.emtrendcontrastci<-data.frame(confint(pairs(mod.emtrends)))
          mod.emtrendcontrast$lower.CL<-mod.emtrendcontrastci$lower.CL
          mod.emtrendcontrast$upper.CL<-mod.emtrendcontrastci$upper.CL
          
          mod.emtrendcontrast$contrast <- gsub("\\.scaled", "", mod.emtrendcontrast$contrast)
          
          "####################################"
          "# Clean Up Labels for Printing"
          "####################################"
          mod.emtrendcontrast <- mod.emtrendcontrast %>%
            set_rownames(NULL) %>%
            set_colnames(c("Contrast", "Estimate", "SE", "df", "t ratio", "p-value", "Lower CI", "Upper CI"))
          
        })
      }else{
        # HIDE emtrends_tab, emtrends_int
      }
    }
  }, inline=TRUE)
  
  make_interaction_emtrendscontrast_table <- metaReactive2({
    
    mod.emtrendcontrast<-globalVars$mod.emtrendcontrast
    
    metaExpr({
      "####################################"
      "# Print Table"
      "####################################"
      mod.emtrendcontrast %>%
        mutate_if(is.numeric, round, 4) %>%
        mutate(`p-value` = ifelse(`p-value` < 0.0001, "<0.0001", `p-value`))
    })
  }, inline=TRUE)
  
  ### Render Model Summary to UI
  output$interaction_emtrendscontrast_tab <- DT::renderDataTable({
    globalVars$make_interaction_emtrendscontrast_table
  },
  extensions = 'Buttons',
  options = list(
    dom = 'Bfrtip',
    buttons = 
      list("copy", "print", list(
        extend = "collection",
        buttons = list(
          list(extend="csv", filename="model-interaction-marginaleffectscontrast"),
          list(extend="excel", filename="model-interaction-marginaleffectscontrast"),
          list(extend="pdf", filename="model-interaction-marginaleffectscontrast")
        ),
        text = "Download",
        filename = "model-interaction-marginaleffectscontrast"
      ))
  ), rownames = FALSE)
  
  # Download button for summary (LaTeX version)----
  output$download_interaction_emtrendscontrastLatex <- downloadHandler(
    filename = function() {
      paste("model-interaction-marginaleffectscontrast.tex", sep="")
    },
    content = function(file) {
      base::print(xtable(globalVars$make_interaction_emtrendscontrast_table, digits = 4), file, type = "latex")
    }
  )
  
  # R Code
  # Display code for ci visualization plot ----
  observeEvent(input$code_interaction_emtrendscontrasttab, {
    code <- expandChain(
      "# Ensure to load your data as an object called dat.",
      quote({
        library(tidyverse) 
        library(magrittr)
        library(emmeans)
      }),
      "####################################",
      "# Load Data",
      "####################################",
      read_data(),
      refactor_data(),
      prepare_transformed_data(),
      prepare_scaled_data(),
      fitmodel(),
      prepare_interaction_emtcontrast(),
      make_interaction_emtrendscontrast_table()
    )
    
    displayCodeModal(
      code, 
      title = "Interaction Marginal Effects Contrasts Table",
      size = "l", 
      fontSize = 16,
      clip=NULL
    )
  })
  
  output$interaction_emtrendscontrast_interp <- renderUI({  
    globalVars$prepare_interaction_emtrendscontrast_interp
  })
  
  prepare_interaction_emtrendscontrast_interp <- function(){
    mod.emtrendcontrast<-globalVars$mod.emtrendcontrast
    model<-globalVars$model
    
    alpha <- input$alpha
    dat = globalVars$dataset
    interacts<-input$var_inter
    
    interact.vars <- strsplit(x = interacts, split = "[:]")[[1]]
    int.var <-interact.vars[which(interact.vars != input$var_moderator)]
    moderator<-input$var_moderator
    
    int.vars.classes<-sapply(X = dat[,interact.vars], FUN = class)
    
    emtrends.interp<-""
    for(i in 1:nrow(mod.emtrendcontrast)){
      emtrends.interp <- paste(emtrends.interp,"\U2022 The effect of ", gsub("\\.scaled", "", (int.var)),  " is ", ifelse(mod.emtrendcontrast[i,"p-value"]<alpha, "significantly ", "not significantly "),
                               "different by ", gsub("\\.scaled", "", (moderator)), 
                               " (Contrast = ", mod.emtrendcontrast[i,"Contrast"],
                               ", Estimate = ", round(mod.emtrendcontrast[i,"Estimate"],4),
                               ", t ratio = ", round(mod.emtrendcontrast[i,"t ratio"],4),
                               ", p-value ",  ifelse(mod.emtrendcontrast[i,"p-value"]<0.0001,"< 0.0001", paste("= ", round(mod.emtrendcontrast[i,"p-value"],4), sep="")), ").<br/>",
                               sep="")
    }
    
    HTML(emtrends.interp)
  }
  
  #############################################################################################
  # Johnson Neyman Plot
  #############################################################################################
  # Code for Johnson Neyman Plot
  jnplot <- metaReactive2({
    req(globalVars$model)
    
    dat <- globalVars$dataset
    
    interacts<-input$var_inter
    
    interact.vars <- strsplit(x = interacts, split = "[:]")[[1]]
    int.var <-interact.vars[which(interact.vars != input$var_moderator)]
    moderator<-input$var_moderator
    
    int.vars.classes<-sapply(X = dat[,interact.vars], FUN = class)
    if(all(int.vars.classes=="numeric")){ #Both Continuous
      model<-globalVars$model
      
      metaExpr({
        "####################################"
        "# Johnson Neyman"
        "####################################"
        jn<-interactions::johnson_neyman(model = model, pred =  !!sym(..(int.var)), 
                                         modx = !!sym(..(moderator)),
                                         control.fdr = TRUE, plot = FALSE)
        
        "####################################"
        "# Plot"
        "####################################"
        p<-jn$plot + 
          xlab(..(moderator))+
          ylab(paste("Slope of ", ..(int.var),sep=""))+
          ggtitle("Johnson-Neyman Plot")+
          scale_color_brewer(paste("Slope of ", ..(int.var),sep=""), palette = "Pastel1")+
          scale_fill_brewer(paste("Slope of ", ..(int.var),sep=""), palette = "Pastel1")+
          theme_bw()
        # If greyscale, this bit is needed
        # p$layers[[9]]  <- geom_vline(xintercept = jnplot$bounds[1], color="black", linetype="dashed")
        # p$layers[[10]] <- geom_vline(xintercept = jnplot$bounds[2], color="black", linetype="dashed")
        p
      })
    }else if(all(int.vars.classes=="factor")){ #Both Factors
      #not appropriate
      cat("Johnson Neyman plots do not support categorical predictors.")
    }else{ #One of Each
      #not appropriate
      cat("Johnson Neyman plots do not support categorical predictors.")
    }
  },inline=TRUE)
  
  # Render plot to UI
  output$jn_plot <- renderPlot({
    globalVars$jnplot
  })
  
  # R Code
  observeEvent(input$code_jn, {
    code <- expandChain(
      "# Ensure to load your data as an object called dat.",
      quote({
        library(tidyverse) 
        library(interactions)
      }),
      "####################################",
      "# Load Data",
      "####################################",
      read_data(),
      refactor_data(),
      prepare_transformed_data(),
      prepare_scaled_data(),
      fitmodel(),
      jnplot()
    )
    
    displayCodeModal(
      code, 
      title = "Johnson Neyman Plot",
      size = "l", 
      fontSize = 16,
      clip=NULL
    )
  })
  
  # Interpret Johnson Neyman Plot
  output$jn_int <- renderPrint({
    globalVars$prepare_jn_int
  })
  
  prepare_jn_int <- function(){
    
    dat <- globalVars$dataset
    
    interacts<-input$var_inter
    
    interact.vars <- strsplit(x = interacts, split = "[:]")[[1]]
    int.var <-interact.vars[which(interact.vars != input$var_moderator)]
    moderator<-input$var_moderator
    
    int.vars.classes<-sapply(X = dat[,interact.vars], FUN = class)
    
    model<-globalVars$model
    
    if(all(int.vars.classes=="numeric")){ #Both Continuous
      jn<-interactions::johnson_neyman(model = model, pred = !!sym(moderator), modx = !!sym(int.var),
                                       control.fdr = TRUE, plot = FALSE)
      jn<-paste(capture.output(jn), collapse=" ")
      jn<-crayon::strip_style(jn)
      jn<-str_remove(string = jn, pattern = "JOHNSON-NEYMAN INTERVAL   ")
      jn<-str_replace(string = jn, pattern = "slope", replacement = "effect")
      jn<-str_replace(string = jn, pattern = "Interval calculated using", replacement = "and the interval was calculated using")
      jn<-strsplit(jn, "adjusted")[[1]][1]
      jn<-paste(jn, "adjustment.", sep="")
      jn<-str_replace(string = jn, pattern = "Note:", replacement = "<br><strong>Note:</strong>")
      jn <- paste("\U2022 ", jn, sep="")
      jn <- gsub("\\.scaled", "", jn)
      
      HTML(jn)
      
    }else if(all(int.vars.classes=="factor")){ #Both Factors
      #not appropriate
      HTML("Johnson Neyman plots do not support categorical predictors.")
    }else{ #One of Each
      #not appropriate
      HTML("Johnson Neyman plots do not support categorical predictors.")
    }
  }
  
  # Download button for plots (call reactive function here to get plot object) ----
  output$downloadjnPlot <- downloadHandler(
    filename = function() { paste('johnsonneymanplot.', input$jn_plot_format, sep='') },
    content = function(file) {
      ggsave(file, plot = globalVars$jnplot, device = input$jn_plot_format, 
             height = as.numeric(input$jn_plot_height), width = as.numeric(input$jn_plot_width), 
             units = input$jn_plot_units)
    }
  )
  
  
  
  # ############################################################################
  # # HANDLE CHECKING FOR ASSUMPTIONS
  # ############################################################################
  observeEvent(input$check_asmp, {
    req(globalVars$model)
    model<-globalVars$model
    dat <- globalVars$dataset
    # REPRESENTATIVE AND INDEPENDENT DATA CHECK
    if(input$asmp_1 == FALSE){
      shinyjs::show("asmp_1note")
      output$asmp_1 <- renderText("Please discuss with the researchers to make sure that the selected sample is generalizable to larger population of interest.")
    }else{shinyjs::hide("asmp_1note")}
    
    
    # NORMALITY CHECK
    df <- data.frame(e=residuals(model))
    norm.testing<-NULL
    if(nrow(df) <= 5000 & nrow(df) >= 3){
      norm.test <- shapiro.test(df$e)
      norm.testing$test = "SW"
      norm.testing$statistic = norm.test$statistic
      norm.testing$p.value = norm.test$p.value
    } else if (nrow(df) > 5000) {
      norm.test <- ks.test(df$e, "pnorm")
      norm.testing$test = "KS"
      norm.testing$statistic = norm.test$statistic
      norm.testing$p.value = norm.test$p.value
    } else {
      # error sample size too small to test
    }
    
    normtext<-""
    if(norm.testing$p.value < 0.05){
      shinyjs::show('asmp_2note')
      normtext<-paste(normtext, "We used normality tests to assess this assumption. ", sep="")
      normtext<-paste(normtext,
                      "The residuals have a distribution that exhibits significant deviations from normality (", norm.testing$test,"=", round(norm.testing$statistic,4),
                      ", p-value", ifelse(norm.testing$p.value<0.0001, "<0.0001", paste("=",round(norm.testing$p.value, 4), sep='')),"). ", sep="")
      normtext<-paste(normtext, "Note that normality tests may detect minuscule deviations from normality, but mild deviations are okay.")
      normtext<-paste(normtext, "Still, you might consider using a transformation or quantile regression to address the normality of residuals.")
      output$asmp_2 <- renderText(normtext)
    }else{
      shinyjs::show('asmp_2note')
      normtext<-paste(normtext, "We used normality tests to assess this assumption. ", sep="")
      normtext<-paste(normtext,
                      "The residuals have a distribution that does not exhibit significant deviations from normality (", norm.testing$test,"=", round(norm.testing$statistic,4),
                      ", p-value", ifelse(norm.testing$p.value<0.0001, "<0.0001", paste("=",round(norm.testing$p.value, 4), sep='')),"). ", sep="")
      output$asmp_2 <- renderText(normtext)
    }
    
    #SHOW TRANSFORMATION BUTTONS IF NECESSARY (hihihi)
    if(input$asmp_2==FALSE | norm.testing$p.value < 0.05){
      if(!(substr(globalVars$response, start=0, stop=16) %in% c("ihs.transformed.", "log.transformed.", "lp1.transformed."))){
        shinyjs::show("ihs_button")
        if(min(dat[[globalVars$response]], na.rm = T) > 0){
          shinyjs::show("log_button") 
        }else if (min(dat[[globalVars$response]], na.rm = T) >= 0){
          shinyjs::show("lp1_button") 
        }
      }
    }else{
      shinyjs::hide("log_button")
      shinyjs::hide("lp1_button")
      shinyjs::hide("ihs_button")
    }
    
    # Constant Variance CHECK
    bp.test <- bptest(model)
    
    vartext<-""
    if(bp.test$p.value < 0.05){
      shinyjs::show('asmp_3note')
      vartext<-paste(vartext, "We used the Breusch-Pagan test to assess this assumption. ", sep="")
      vartext<-paste(vartext,
                     "The residuals exhibit significant evidence of non-constant variance (BP","=", round(bp.test$statistic,4),
                     ", p-value", ifelse(bp.test$p.value<0.0001, "<0.0001", paste("=",round(bp.test$p.value, 4), sep='')),"). ", sep="")
      vartext<-paste(vartext, "Note that the Breusch-Pagan test may detect minuscule deviations from constant variance, but mild deviations are okay.")
      vartext<-paste(vartext, "Still, you might consider using a transformation or quantile regression to address the constant variance of the residuals.")
      output$asmp_3 <- renderText(vartext)
    }else{
      shinyjs::show('asmp_3note')
      vartext<-paste(vartext, "We used the Breusch-Pagan test to assess this assumption. ", sep="")
      vartext<-paste(vartext,
                     "The residuals do not exhibit significant evidence of non-constant variance (BP","=", round(bp.test$statistic,4),
                     ", p-value", ifelse(bp.test$p.value<0.0001, "<0.0001", paste("=",round(bp.test$p.value, 4), sep='')),"). ", sep="")
      output$asmp_3 <- renderText(vartext)
    }
    
    # VIF Check
    if(length(attr(model$terms, "term.labels"))>=2){
      shinyjs::show('asmp_4note')
      VIF = vif(model)
      vartext2 = ""
      if(any(VIF>5)){
        vartext2 <-"We calculated the Variance Inflation Factor (VIF) to assess collinearity of the model -- "
        viftext <- paste("VIF(", names(VIF), ")=", round(VIF,2), sep="")
        if(sum(VIF>5)==1){
          vartext2<-paste(vartext2, viftext, " has high VIF, indicating that collinearity may be an issue. You might consider standardizing your data or using a regularization method like ridge regression or LASSO.")
        }else{
          viftext <- paste(paste(viftext[-length(viftext)], collapse = ", "), " and ", viftext[length(viftext)], sep="")  
          vartext2<-paste(vartext2, viftext, " have high VIF, indicating that collinearity may be an issue. You might consider standardizing your data or using a regularization method like ridge regression or LASSO.")
        }
      }else{
        vartext2<-paste(vartext2, "We calculated the Variance Inflation Factor (VIF) to assess collinearity of the model. None of the calculated VIFs were larger than five, indicating that collinearity may not be an issue.", sep="")
      }
      
      output$asmp_4 <- renderText(vartext2)
    }else{
      updateCheckboxInput(session, "asmp_4", value =FALSE)
    }
    
    
    if(!(length(attr(model$terms, "term.labels"))>=2)){ #only three assumptions
      if(input$asmp_1 & input$asmp_2 & input$asmp_3){
        if(!any(norm.testing$p.value < 0.05) & !(bp.test$p.value < 0.05)){ #agreed
          shinyjs::show("asmp_note")
          output$asmp_note <- renderText("All conditions for use are met. Please proceed to the next step.")
        } else{
          shinyjs::show("asmp_note")
          output$asmp_note <- renderText("Double check the conditions for use are met, and then proceed to the next step.")
        } 
      } else{
        shinyjs::show("asmp_note")
        output$asmp_note <- renderText("Some condition(s) for use are not met. Consider another method or proceed with caution.")
      }
    }
    else{  #Four assumptions
      if(input$asmp_1 & input$asmp_2 & input$asmp_3 & input$asmp_4){
        if(!(any(VIF>5)) & !(any(norm.testing$p.value < 0.05)) & !(bp.test$p.value < 0.05)){ #agreed
          shinyjs::show("asmp_note")
          output$asmp_note <- renderText("All conditions for use are met. Please proceed to the next step.")
        }
        else{
          shinyjs::show("asmp_note")
          output$asmp_note <- renderText("Double check the conditions for use are met, and then proceed to the next step.")
        }
      }
      else{
        shinyjs::show("asmp_note")
        output$asmp_note <- renderText("Some condition(s) for use are not met. Consider another method or proceed with caution.")
      }
      
    }
    
    
    
  })
  
  ############################################################################
  # HANDLE CHECKING FOR OBSERVATIONS
  ############################################################################
  observeEvent(input$check_obs, {
    shinyjs::show("check_note")
    
    if(input$check_1 == TRUE && input$check_2 == TRUE && input$check_3 == TRUE && input$check_4 == TRUE){
      discuss_text <- "The model likely isn't affected by outliers or high-leverage points. Please proceed to the next step."
      output$check_note <- renderText(discuss_text)
    } else{
      discuss_text <- "There are observations that might have an outsized effect on the model fit. You may want to consider using a weighted regression or quantile regression model."
      output$check_note <- renderText(discuss_text)
    }
    
  })
})
