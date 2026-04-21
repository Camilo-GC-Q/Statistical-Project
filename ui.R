# #################################################################################################
# #################################################################################################
# #################################################################################################
# #################################################################################################
#' Linear Regression Application
#' @author The Data Science Collaboratory At Colgate University (datascience[at]colgate[dot]edu)
#' @description UI code for the Linear Regression app
# #################################################################################################
# #################################################################################################
# #################################################################################################
# #################################################################################################
library(Hmisc)      #correlations and tests (still here, here due to overriding summarize)

##########################################
# Shiny
##########################################
library(shiny)
library(shinyalert)
library(shinyjs)
library(shinymeta)
library(shinythemes)
library(shinycssloaders)
library(shinyAce)
library(shinyBS)
library(DT)
##########################################
# Data
##########################################
library(palmerpenguins)
library(ISLR)
##########################################
# General
##########################################
library(tidyverse) #ggplot2, tibble, tidyr, reader, 
#purrr, dplyr, stringr, forcats
library(magrittr)
library(patchwork) #combining plots
library(xtable) #LaTex Plots

##########################################
# Regression
##########################################
library(GGally)      #pairwise plots
library(gtools)      #pvales for cormat
#library(Hmisc)      #correlations and tests (still here, not loaded due to overriding summarize)
library(lmtest)      #constant error variance
library(margins)     #for marginal effects
library(multcomp)    #glht
library(interactions)#for johnson neyman
library(ggeffects)   #for marginal effects plots
library(emmeans)     #for estimated marginal means
library(car)         #for anova
#library(crayon)      #for removing style from Johnson Neyman output
library(effectsize)
#library(ggforce)
#--------------------------------------------
#---------------     UI    ------------------
#--------------------------------------------
ui<-tagList(tags$head(tags$link(rel = "icon",  type = "image/x-icon", 
                                href = "www/favicon.png"), 
                      tags$style(HTML(".paragraph {margin:auto;max-width: 50%;font-size: 15px; text-align:justify;}
                                        h1 {text-align:center;}")),
                      tags$style(HTML("div.MathJax_Display{text-align: left !important;}"))),
            
            # action buttons smaller
            tags$style(HTML(".btn {padding:5px; font-size:12px;}")),
            # checkbox label text smaller
            tags$style(HTML(".checkbox {font-size:12px; margin:5px;}")),
            # input label text smaller
            tags$style(HTML(".control-label {font-size:14px;}")),
            # text input smaller
            tags$style(HTML(".form-control {height:auto; padding:5px;}")),
            tags$style(HTML(".shiny-input-text {font-size:12px;}")),
            tags$style(HTML(".shiny-input-number {font-size:12px;}")),
            # assumption text smaller
            #tags$style(HTML(".shiny-bound-output {font-size:13px;}")), # this changes all UIOutput()
            tags$style(HTML("#asmp_1note {font-size:12px;}")),
            tags$style(HTML("#asmp_2note {font-size:12px;}")),
            tags$style(HTML("#asmp_3note {font-size:12px;}")),
            tags$style(HTML("#asmp_4note {font-size:12px;}")),
            tags$style(HTML("#check_note {font-size:12px;}")),
            tags$style(HTML("#asmp_note {font-size:12px;}")),
            # dropdowns smaller
            tags$style(HTML(".item {height:auto;}")),
            tags$style(HTML(".selectize-input, .selectize-dropdown {height:auto; padding:5px; font-size: 12px;}")),
            # reduce space between inputs
            tags$style(HTML(".form-group {margin-bottom: 5px; }")),
            
            useShinyjs(), #useShinyalert(), withMathJax(),  # load functions from packages
  navbarPage(title="Linear Regression | Data Science Collaboratory", id = "tabs",theme = shinytheme("flatly"),
             #Introduction panel
             tabPanel(title = "About",
                      h1("Linear Regression", align = "center"), br(),
                      h3("What is linear regression?", align = "Center"), 
                      tags$div(class = "paragraph", align = "center", tags$hr(),
                               p("Linear Regression is a statistical tool used to quantify the linear relationship between a quantitative variable of interest and one or more explanatory variable(s). The resulting model can be used to evaluate evidence for hypotheses about the relationship and to make predictions under the following conditions."),
                               tags$p("1.	The observations are representative of the population of interest and independent."),
                               tags$p("2.	The residuals (errors) are normally distributed with common variance.") 
                      ),
                      br(),
                      h3("How to use this app?", align="center"), 
                      tags$div(class = "paragraph", tags$hr(),
                               p("Step 1: To use this app, go to the 'Dataset & Model' tab and upload your .csv type dataset, or select a sample dataset."), 
                               p("Step 2: Fit your model by inputting your desired regression equation in the form:"),
                               wellPanel(strong("response_name ~ explanatory_1_name + explanatory_2_name + ... + explanatory_k_name")),
                               p("Designate interaction terms using the * or : symbol between the two variable names. Using the asterisk will include both variables and their interaction (recommended), whereas the colon will only include the interaction. For example, an interaction between explanatory variable 1 and 2 can be specified as follows."),
                               wellPanel(strong("response_name ~ explanatory_1_name * explanatory_2_name + ... + explanatory_k_name")),
                               wellPanel(strong("response_name ~ explanatory_1_name : explanatory_2_name + ... + explanatory_k_name")),
                               p("Step 3: You can see the data and a correlation matrix of the numeric variables in the 'Dataset Summary' tab"), 
                               p("Step 4: You can check the assumptions provided in the 'Assumptions' tab. We recommend assessing assumptions visually using the provided graphical summary and confirming using the numerical summaries. The app will provide results for a Breusch-Pagan test assessing common variance of the residuals and Shapiro-Wilkes (n \U2264 5000) or Kolmogorov-Smirnov (n > 5000) tests for normality. While these tests might be helpful, they can be rather sensitive for small sample sizes leading us to detect minuscule transgressions."), 
                               p("Step 5: You can check the effect of outlying, influential, or leverage points in the 'Outliers' tab. Many models exhibit some influential points and researchers should ensure that the results of their model hold when using a robust regression model."), 
                               p("Step 6: The ANOVA table for the regression model is reported in the 'ANOVA' tab."),
                               p("Step 7: The resulting model and interpretation of key values can be found in the 'Interpretation' tab"),
                               p("Step 8 (Optional): If your model has an interaction, the appropriate analyses will be reported in the 'Interaction' tab.")), 
                      h3("Contact us", align="center"), 
                      tags$div(class = "paragraph", tags$hr(), align="center", 
                               tags$p("Please contact us if you have any questions at", align="center", 
                                      tags$a(href="mailto:datascience@colgate.edu", "datascience@colgate.edu."))), 
                      br(), br(), br()
             ),
             #Tutorial panel w/ pictures 
             tabPanel("Tutorial",
                      tabsetPanel(id="Examples",
                                  tabPanel("Example 1",
                                           h1("Example 1", align="center"),
                                           tags$div(class = "paragraph", tags$hr(),
                                                    p("Within the regression app, we provide data collected on a representative sample of n=558 White Americans by Cooley et al. (2022). The researchers aimed to assess whether beliefs that White people are poor are associated with the humanization of welfare recipients among White Americans who feel intergroup status threat—namely, those high in racial zero-sum beliefs."),
                                                    p("If this were the case, it would suggest that the link between White-poor beliefs, the humanization of welfare recipients, and welfare policy support may be motivated by a desire to preserve the racial status quo.")),
                                           
                                           tags$div(class = "paragraph", 
                                                    p("The researchers used perceived agency of welfare recipients as a measure of humanization, and they wanted to evaluate whether White-poor and racial zero-sum beliefs affect this perception by controlling for education, income, political affiliation (Democrat or not) and their beliefs that Black people are poor."),
                                                    p("Specifically, they hypothesized that the association between White-poor beliefs and the humanization of welfare recipients would be stronger among white Americans who also had higher racial zero-sum beliefs, indicating that an interaction term is necessary."),
                                                    tags$hr(),
                                                    wellPanel(strong("Zagency ~ ZWpoor*Zzerosum + Zedu + Zincome + Democrat + ZBpoor"))),
                                           
                                           tags$div(class = "paragraph",  tags$hr(),
                                                    p("The data are quite noisy, and we can see that some variables are discrete. We see that the perceived agency of welfare recipients (humanization) is positively correlated with beliefs that White people are poor, beliefs that Black people are poor, and negatively correlated with zero-sum beliefs. Further, we can see that the perceived agency of welfare recipients (humanization) appears to be more prominent among Democrats than non-democrats."),
                                                    p("While these findings provide some insight toward our research question, they are zero-order, meaning we look at the pairs of correlations independently without considering how all the explanatory variables work together."),
                                                    tags$hr()),
                                           
                                           HTML('<center><img src="ex1-datasummary.png"></center>'),
                                           
                                           tags$div(class = "paragraph", tags$hr(),
                                                    p("The first step of linear regression analysis is to evaluate the assumptions. When we click 'Assumptions', the residuals are plotted."),
                                                    tags$hr()),
                                           
                                           HTML('<center><img src="ex1-assumptions.png"></center>'),
                                           
                                           tags$div(class = "paragraph", tags$hr(),
                                                    p("This plot shows that the residuals are roughly normally distributed as the density is symmetric and bell-shaped. The variance is roughly constant from left to right in the fitted values versus residuals plot. We see the boundary of the scale for perceived agency create a band of points from right to left."), 
                                                    p("Evaluating whether the observations represent the population of interest and are independent is more challenging. The researchers recruited a representative sample of White Americans living in the United States based on the region of the country, age, gender, and education through the Lucid Panels service. We assume the company ensures independent observations; e.g., responses from the same IP address should be filtered."),
                                                    tags$hr()),
                                           
                                           HTML('<center><img src="ex1-outliers.png"></center>'),
                                           
                                           tags$div(class = "paragraph", tags$hr(),
                                                    p("In the outliers, influential, and leverage points plot, we see several observations impact the fit of the regression. This may not be a big issue; for example, the extreme observations may balance each other. Still, we might consider fitting a robust model to confirm that just a handful of points do not drive the results we see in the later tabs."),
                                                    tags$hr()
                                           ),
                                           
                                           HTML('<center><img src="ex1-anova.png"></center>'),
                                           
                                           tags$div(class = "paragraph", tags$hr(),
                                                    p("The ANOVA table shows us that the effect of racial zero-sum beliefs (F=19.48, p<0.0001), political affiliation (F=10.42, p=0.0013), and the interaction between White-poor and racial zero-sum beliefs (F=8.63, p=0.0034) are significant explanatory variables. Because we have a significant interaction, we report Type III sums of squares. Therefore, these tests indicate whether explanatory variables have statistically significant predictive ability when accounting for all other explanatory variables, including interactions."),
                                                    tags$hr()
                                           ),
                                           
                                           
                                           HTML('<center><img src="ex1-modelsummary.png"></center>'),
                                           
                                           tags$div(class = "paragraph", tags$hr(),
                                                    p("The regression model results mirror what we see in the ANOVA table. We noted that the data are noisy, which is reflected in the low adjusted R-squared that indicates that the model explains 7.83% of the variation in perceived agency of welfare recipients (humanization). While this might be discouraging, the social behavior of humans is wildly complex. Explaining even little slices of human beliefs and their consequences can be very important."),
                                                    p("The marginal effects give us information about how the explanatory variables are related to the response individually. The model tells us that, on average, Democrats perceive the agency of welfare recipients (humanization) higher than non-Democrats (z=3.23, p=0.0012) and that those with higher racial zero-sum beliefs have lower perceptions of agency in welfare recipients (z=-4.414, p<0.0001). The marginal effect of White-poor beliefs does not reach traditional levels of statistical significance (z=1.85, p=0.06). Note that the response and explanatory variables have been z-score transformed so the units are in 'number of standard deviations.' That is, a unit increase is an increase equal to the standard deviation of the observed data."),
                                                    p("To interpret the interactions, we test the effect of White-poor beliefs on the perceived agency of welfare recipients separately for people with high (+ 1 SD) and low (-1 SD) racial-zero-sum beliefs."),
                                                    tags$hr()
                                           ),
                                           
                                           HTML('<center><img src="ex1-interaction.png"></center>'),
                                           
                                           tags$div(class = "paragraph", tags$hr(),
                                                    p("As the researchers predicted, among those high in racial-zero-sum beliefs, greater White-poor beliefs were associated with greater perceived agency of welfare recipients (slope=0.1996, t-ratio=3.5725, p=0.0004; 95% CI: 0.0899, 0.3094). In contrast, White-poor beliefs were not associated with the perceived agency of welfare recipients among those low in racial-zero-sum beliefs (slope=-0.0007, t-ratio=-0.0096, p=0.9923; 95% CI: -0.139, 0.1376)."),
                                                    p("In terms of significance testing, the Johnson Neyman analyses show that the effect of White-poor beliefs is significant when racial zero-sum beliefs are greater than 0.12. Noting that the predictors and response are z-score transformed, we note that this corresponds to racial zero-sum beliefs that are 0.12 standard deviations above the mean.")),
                                           
                                           tags$hr(),
                                           tags$div(class = "paragraph", 
                                                    p("Cooley, E., Brown-Iannuzzi, J. L., Lei, R. F., Philbrook, L & Cipolli III, W. (2022). Beliefs that White People are Poor, Above and Beyond Beliefs that Black People are Poor, Predict White. White Paper, Colgate University."))
                                  ))),
             #Dataset and Model Summary Panel
             tabPanel("Dataset & Model",
                      sidebarPanel(
                        fileInput("file_upload", "Upload a File", accept=c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                        actionButton("sample", "Sample Data"),
                        hidden(div(id='choose_sample',
                                   selectInput("sample_data_choice","Sample Data:",
                                               choices = c("Cooley's Poor Beliefs Data", "Palmer Penguins", "Bracht et al. MFAP4" ,"U.S. News College Data", "Lai et al. Tree Data", "Lai et al. Schima Superba", "Loven et al. Road Weather Data"),
                                               selected = "U.S. News College Data"))),
                        tags$hr(),
                        tags$style(type="text/css", ".selectize-input{overflow: auto;}"), # this fixes overflow in selectize
                        hidden(selectizeInput("select_factors",
                                       "Specify Categorical Variables in the Data:",
                                       choices = NULL,
                                       selected = NULL,
                                       multiple = TRUE)),
                        
                        textInput("equation", "Enter your desired regression equation:"),
                        bsTooltip("equation", "Example: response ~ explanatory_1 + explanatory_2 + ... + explanatory_k",
                                  "right", trigger = "hover", options = list(container = "body")),
                        
                        checkboxInput("scalevars", "Scale all variables (standardize)", FALSE),
                        
                        
                        
                        numericInput("alpha", "Significance level (\u03B1): ", value = 0.05, step = 0.001, min = 0, max = 1),     # alpha level
                        div(class = "text-center", actionButton("DoCompute", "Compute Model Output")),
                        div(h3("Interaction Analysis:"), id="interaction_analysis"),
                        selectizeInput("var_inter",
                                       "Select Interaction",
                                       choices = c("None"),
                                       selected = "None",
                                       multiple = FALSE),
                        
                        selectizeInput("var_moderator",
                                       "Select Moderator",
                                       choices = c("Select..."),
                                       selected = "Select...",
                                       multiple = FALSE),
                        hidden(checkboxInput("interaction.error", "Error Ribbon for Interaction", TRUE)),
                      ),
                      #visualize dataset and display dataset
                      mainPanel(
                        tabsetPanel(id="workPanel",
                                    #display dataset
                                    tabPanel("Data Preview", br(), value="data",
                                             shinycssloaders::withSpinner(DT::dataTableOutput("preview.data"))),
                                    tabPanel("Data Summary", value="summary",
                                             fluidPage(
                                               h1("Pairwise Plots", align = "center"), br(),
                                               fluidRow(column(12, actionButton("code_ggpairsplot", "R code", icon("code"))),
                                               fluidRow(column(12, shinycssloaders::withSpinner(plotOutput("ggpairs_plot")))),
                                               fluidRow(
                                                 column(width=2, textInput("ggpairs_plot_height", "Enter Height", value=7)),
                                                 column(width=2, textInput("ggpairs_plot_width", "Enter Width", value=7)),
                                                 column(width=2, selectInput("ggpairs_plot_units", "Units", choices = c("in", "cm"))),
                                                 column(width=2, selectInput("ggpairs_plot_format", "Format", choices = c("png", "pdf", "tiff", "bmp"))),
                                                 column(width=2, downloadButton('downloadggpairsPlot'),style = "margin-top: 25px;"), #
                                                 tags$head(tags$style(HTML(".selectize-input {height: 42px;}")))
                                               ),
                                               tags$hr(),
                                               br(),
                                               
                                               h1("Correlation Matrix", align = "center"), br(),
                                               fluidRow(column(12, actionButton("code_corrmat", "R code", icon("code")), downloadButton('downloadcormatLatex',label="LaTeX"))),br(),
                                               fluidRow(column(12, shinycssloaders::withSpinner(DT::dataTableOutput("ggpairs_summary")))), "*<0.05; **<0.01; ***<0.001", br()
                                               #verbatimTextOutput("summary")),
                                             ),
                                             tags$hr())
                                    ),
                                    tabPanel("Assumptions", value="assumptions",
                                             fluidPage(h1("Assumptions for Regression"),
                                                       fluidRow(
                                                         column(4, style = "background-color:#ecf0f1;", tags$hr(), br(),
                                                                h4("Make sure that you satisfy all linear regression assumptions:"), br(),
                                                                # Assumption 1
                                                                checkboxInput("asmp_1", HTML("The sample(s) is representative, and observations are independent."), FALSE),
                                                                hidden(div(id='asmp_1note', htmlOutput('asmp_1'))),
                                                                # Assumption 2
                                                                checkboxInput("asmp_2", "The underlying distribution of the residuals is Gaussian", FALSE),
                                                                hidden(div(id='asmp_2note', htmlOutput('asmp_2'), style="margin-bottom:10px;margin-top:10px")),
                                                                hidden(div(id='log_button', actionButton("logtransform", "log transform"), style="margin-bottom:10px;margin-top:10px")),
                                                                hidden(div(id='lp1_button', actionButton("logplus1transform", "log(y+1) transform"), style="margin-bottom:10px;margin-top:10px")),
                                                                hidden(div(id='ihs_button', actionButton("ihstransform", "inverse hyperbolic sine transform"), style="margin-bottom:10px;margin-top:10px")),
                                                                # Assumption 3
                                                                checkboxInput("asmp_3", "The residuals have constant variance."),
                                                                hidden(div(id='asmp_3note', htmlOutput('asmp_3'))),
                                                                # Assumption 4
                                                                checkboxInput("asmp_4", "The predictors are not collinear."),
                                                                hidden(div(id='asmp_4note', htmlOutput('asmp_4'))),
                                                                br(),
                                                                actionButton("check_asmp", strong("Check Assumptions")), br(), br(),    # Button to check all assumptions
                                                                # Hidden divs are displayed only for two-sample independent test
                                                                hidden(div(id='asmp_note', htmlOutput('asmp_note')))
                                                         ),
                                                         column(8, 
                                                                fluidRow(actionButton("code_asmp", "R code", icon("code")), style = "margin-left: 20px;"), 
                                                                br(),   # Show code using shinymeta pkg
                                                                fluidRow(shinycssloaders::withSpinner(plotOutput("asmp_plot")), style = "margin-left: 20px;"),
                                                                fluidRow(
                                                                  column(width=2, textInput("asmp_plot_height", "Enter Height", value=7)),
                                                                  column(width=2, textInput("asmp_plot_width", "Enter Width", value=7)),
                                                                  column(width=2, selectInput("asmp_plot_units", "Units", choices = c("in", "cm"))),
                                                                  column(width=2, selectInput("asmp_plot_format", "Format", choices = c("png", "pdf", "tiff", "bmp"))),
                                                                  column(width=2, downloadButton('downloadasmpPlot'),style = "margin-top: 25px;"), #
                                                                  tags$head(tags$style(HTML(".selectize-input {height: 42px;}")))
                                                                ),
                                                                br(),
                                                                hidden(div(id='vifdiv',
                                                                     fluidRow(column(12, actionButton("code_vif", "R code", icon("code")), downloadButton('downloadvifLatex',label="LaTeX"))),br(),
                                                                     fluidRow(column(12, shinycssloaders::withSpinner(DT::dataTableOutput("vifTab")))),br()
                                                                )),
                                                         )
                                                       )
                                                       
                                             )
                                    ),
                                    tabPanel("Outliers", value="checks",
                                             fluidPage(h1("Outliers, Influential, and Leverage Points"),
                                                       fluidRow(
                                                         column(4, style = "background-color:#ecf0f1;", tags$hr(), br(),
                                                                h4("Make sure to evaluate whether there are outliers or influential points:"), br(),
                                                                # Assumption 1
                                                                checkboxInput("check_1", HTML("Few/no observations with large leverage values."), FALSE),
                                                                # Assumption 2
                                                                checkboxInput("check_2", "Few/no observations with large Cook's distance values.", FALSE),
                                                                # Assumption 3
                                                                checkboxInput("check_3", "Few/no observations with DFFITS with large magnitude.", FALSE),
                                                                # Assumption 4
                                                                checkboxInput("check_4", "Few/no observations with outlying residuals."),
                                                                br(),
                                                                actionButton("check_obs", strong("Check Observations")), br(), br(),    # Button to check all assumptions
                                                                hidden(div(id='check_note', htmlOutput('check_note')))
                                                         ),
                                                         column(8,
                                                                fluidRow(actionButton("code_check", "R code", icon("code")), style = "margin-left: 20px;"),
                                                                br(),   # Show code using shinymeta pkg
                                                                fluidRow(shinycssloaders::withSpinner(plotOutput("check_plot")), style = "margin-left: 20px;"), 
                                                                fluidRow(
                                                                  column(width=2, textInput("check_plot_height", "Enter Height", value=7)),
                                                                  column(width=2, textInput("check_plot_width", "Enter Width", value=7)),
                                                                  column(width=2, selectInput("check_plot_units", "Units", choices = c("in", "cm"))),
                                                                  column(width=2, selectInput("check_plot_format", "Format", choices = c("png", "pdf", "tiff", "bmp"))),
                                                                  column(width=2, downloadButton('downloadcheckPlot'),style = "margin-top: 25px;"), #
                                                                  tags$head(tags$style(HTML(".selectize-input {height: 42px;}")))
                                                                )
                                                         )
                                                       )
                                             )
                                    ),
                                    tabPanel("ANOVA", value="anova",
                                             fluidPage(h1("ANOVA Table"), 
                                                       fluidRow(column(12, actionButton("code_anova", "R code", icon("code")), downloadButton('downloadanovaLatex',label="LaTeX"))),br(),
                                                       fluidRow(column(12, shinycssloaders::withSpinner(DT::dataTableOutput("anovaTab")))),br(),
                                                       h1("Interpretation"),
                                                       htmlOutput("anovainterp"),
                                                       tags$hr(),
                                                       hidden(div(id='anova_fctcomp',
                                                                  h1("Factor Comparisions"),
                                                                  fluidRow(column(12, actionButton("code_anova_fctcomp", "R code", icon("code")))),
                                                                  br(),
                                                                  fluidRow(shinycssloaders::withSpinner(plotOutput("anova_fctcomp_plot")), style = "margin-left: 20px;"), 
                                                                  br(),
                                                                  fluidRow(
                                                                    column(width=2, textInput("anova_fctcomp_plot_height", "Enter Height", value=7)),
                                                                    column(width=2, textInput("anova_fctcomp_plot_width", "Enter Width", value=7)),
                                                                    column(width=2, selectInput("anova_fctcomp_plot_units", "Units", choices = c("in", "cm"))),
                                                                    column(width=2, selectInput("anova_fctcomp_plot_format", "Format", choices = c("png", "pdf", "tiff", "bmp"))),
                                                                    column(width=2, downloadButton('downloadanovafactorcomparePlot'),style = "margin-top: 25px;"), #
                                                                    tags$head(tags$style(HTML(".selectize-input {height: 42px;}")))
                                                                  ),
                                                                  fluidRow(column(12, downloadButton('downloadanovafctcompLatex',label="LaTeX"))),br(),
                                                                  fluidRow(column(12, shinycssloaders::withSpinner(DT::dataTableOutput("anova_fctcompTab")))),br(),
                                                                  h1("Interpretation"),
                                                                  htmlOutput("anova_fctcompinterp"),
                                                                  tags$hr()
                                                                  ))
                                                                  
                                             )
                                    ),
                                    tabPanel("Interpretation", value="interpretation",
                                             fluidPage(h1("Regression Model"), 
                                                       fluidRow(column(12, actionButton("code_modsum", "R code", icon("code")), downloadButton('downloadmodsumLatex',label="LaTeX"))),br(),
                                                       fluidRow(column(12, shinycssloaders::withSpinner(DT::dataTableOutput("modsumTab")))),br(),
                                                       h1("Interpretation"),
                                                       htmlOutput("modelinterp"),
                                                       tags$head(tags$style("#clickGene{color:red; font-size:12px; font-style:italic;}")),
                                                       br(),
                                                       tags$hr(),
                                                       hidden(div(id='marginaleffectsdiv',
                                                         h1("Marginal Effects"), 
                                                         fluidRow(column(12, actionButton("code_margins", "R code", icon("code")), downloadButton('downloadmarginsLatex',label="LaTeX"))),br(),
                                                         fluidRow(column(12, shinycssloaders::withSpinner(DT::dataTableOutput("marginsTab")))),br(),
                                                         h1("Interpretation"),
                                                         htmlOutput("marginsinterp"),br(),
                                                         tags$hr()
                                                       ))
                                             )
                                    ),
                                    tabPanel("Interaction", value="interaction",
                                             fluidPage(tags$hr(),
                                                       h1("Interaction Analysis"),
                                                       h3("Visualization"),
                                                       fluidRow(column(12, actionButton("code_ggemmeans", "R code", icon("code")))),
                                                       fluidRow(column(12, shinycssloaders::withSpinner(plotOutput("ggemmeans_plot")))),
                                                       fluidRow(
                                                         column(width=2, textInput("ggemmeans_plot_height", "Enter Height", value=7)),
                                                         column(width=2, textInput("ggemmeans_plot_width", "Enter Width", value=7)),
                                                         column(width=2, selectInput("ggemmeans_plot_units", "Units", choices = c("in", "cm"))),
                                                         column(width=2, selectInput("ggemmeans_plot_format", "Format", choices = c("png", "pdf", "tiff", "bmp"))),
                                                         column(width=2, downloadButton('downloadggemmeansPlot'),style = "margin-top: 25px;"), #
                                                         tags$head(tags$style(HTML(".selectize-input {height: 42px;}")))
                                                       ),
                                                       tags$hr(),
                                                       
                                                       h3("Estimated Marginal Means"),
                                                       fluidRow(column(12, actionButton("code_interaction_emmeanstab", "R code", icon("code")), downloadButton('download_interaction_emmeansLatex',label="LaTeX"))),br(),
                                                       fluidRow(column(12, shinycssloaders::withSpinner(DT::dataTableOutput("interaction_emmeans_tab")))),
                                                       h3("Interpretation"),
                                                       htmlOutput("interaction_emmeans_interp"),br(),
                                                       
                                                       h3("Contrasts of Marginal Means"),
                                                       fluidRow(column(12, actionButton("code_interaction_emmeans_contrast", "R code", icon("code")), downloadButton('download_interaction_emmeanscontrastLatex',label="LaTeX"))),br(),
                                                       fluidRow(column(12, shinycssloaders::withSpinner(DT::dataTableOutput("interaction_emmeanscontrast_tab")))), br(),
                                                       h3("Interpretation"),
                                                       htmlOutput("interaction_emmeanscontrast_interp"), 
                                                       
                                                       ##############################################################################
                                                       # Should only be available for everything but factor/factor and numeric/factor with factor as the moderator
                                                       ##############################################################################
                                                       hidden(div(id='emtrendsdiv',
                                                                  tags$hr(),
                                                                  h3("Estimated Marginal Effects"),
                                                                  fluidRow(column(12, actionButton("code_interaction_emtrendstab", "R code", icon("code")), downloadButton('download_interaction_emtrendsLatex',label="LaTeX"))),br(),
                                                                  fluidRow(column(12, shinycssloaders::withSpinner(DT::dataTableOutput("interaction_emtrends_tab")))),
                                                                  h3("Interpretation"),
                                                                  htmlOutput("interaction_emtrends_interp"),br(),
                                                       )),
                                                       
                                                       ##############################################################################
                                                       # Should only be available for everything but factor/factor and numeric/factor with factor as the moderator
                                                       ##############################################################################
                                                       hidden(div(id='emtrendscontrastdiv',
                                                                  tags$hr(),
                                                                  h3("Contrast of Marginal Effects"),
                                                                  fluidRow(column(12, actionButton("code_interaction_emtrendscontrasttab", "R code", icon("code")), downloadButton('download_interaction_emtrendscontrastLatex',label="LaTeX"))),br(),
                                                                  fluidRow(column(12, shinycssloaders::withSpinner(DT::dataTableOutput("interaction_emtrendscontrast_tab")))),
                                                                  h3("Interpretation"),
                                                                  htmlOutput("interaction_emtrendscontrast_interp"),br(),
                                                       )),
                                                       
                                                       ##############################################################################
                                                       # Should only be available for numeric*numeric interactions
                                                       ##############################################################################
                                                       hidden(div(id='jndiv',
                                                                  tags$hr(),
                                                                  h3("Johnson Neyman"),
                                                                  fluidRow(column(12, actionButton("code_jn", "R code", icon("code")))),
                                                                  fluidRow(column(12, shinycssloaders::withSpinner(plotOutput("jn_plot")))),
                                                                  fluidRow(
                                                                    column(width=2, textInput("jn_plot_height", "Enter Height", value=7)),
                                                                    column(width=2, textInput("jn_plot_width", "Enter Width", value=7)),
                                                                    column(width=2, selectInput("jn_plot_units", "Units", choices = c("in", "cm"))),
                                                                    column(width=2, selectInput("jn_plot_format", "Format", choices = c("png", "pdf", "tiff", "bmp"))),
                                                                    column(width=2, downloadButton('downloadjnPlot'),style = "margin-top: 25px;"), #
                                                                    tags$head(tags$style(HTML(".selectize-input {height: 42px;}")))
                                                                  ),
                                                                  h3("Interpretation"),
                                                                  htmlOutput("jn_int"),br()
                                                       )),
                                                       ##############################################################################
                                                       tags$hr(),
                                                       
                                             )
                                    )
                        ))
             ),
             tabPanel("References",
                      fluidPage(
                        includeHTML("www/livebib.html")
                      )
             )
  )
)