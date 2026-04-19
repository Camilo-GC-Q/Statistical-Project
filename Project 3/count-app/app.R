library(shiny)
library(tidyverse)
list.files("R", full.names = TRUE) |> purrr::walk(source)

ui = fluidPage(
    titlePanel("Count Regression Toolkit"),
    sidebarLayout(
        sidebarPanel(
            fileInput("file", "Upload CSV File", accept = ".csv"),
            uiOutput("response_ui"),
            uiOutput("predictor_ui"),
            actionButton("fit_poisson", "Fit Poisson Model"),
            br(), br(),
            actionButton("fit_nb", "Fit Negative Binomial Model"),
            br(), br(),
            actionButton("fit_quasi_poisson", "Fit Quasi Poisson Model")
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("Data Preview",
                        tableOutput("preview")
                ),
                tabPanel("Count Summary",
                        verbatimTextOutput("summary_stats"),
                        plotOutput("count_plot")
                ),
                tabPanel("Poisson Model",
                        verbatimTextOutput("model_formula"),
                        h4("Incidence Rate Ratios"),
                        tableOutput("rate_ratio_table")
                ),
                tabPanel("Negative Binomial Model",
                        verbatimTextOutput("nb_model_formula"),
                        h4("Incidence Rate Ratios"),
                        tableOutput("irr_table")
                ),
                tabPanel("Quasi-Poisson Model",
                        verbatimTextOutput("quasi_poisson_formula"),
                        h4("Incidence Rate Ratios"),
                        tableOutput("quasi_irr_table")
                )
            )
        )
    )
)


server = function(input, output, session){
    # Load Data
    data = reactive({
        req(input$file)
        read_csv(input$file$datapath)
    })

    poisson_model = eventReactive(input$fit_poisson, {
        req(data(), input$response, input$predictors)
        fit_poisson_model(data(), input$response, input$predictors)
    })

    nb_model = eventReactive(input$fit_nb, {
        req(data(), input$response, input$predictors)
        fit_negative_binomial_model(data(), input$response, input$predictors)
    })

    quasi_poisson_model = eventReactive(input$fit_quasi_poisson, {
        req(data(), input$response, input$predictors)
        fit_quasi_poisson_model(data(), input$response, input$predictors)
    })

    output$model_formula = renderPrint({
        req(input$response, input$predictors)

        validate(
            need(length(input$predictors) > 0, "Please select at least one predictor.")
        )

        cat(paste(input$response, "~", paste(input$predictors, collapse = " + ")))
    })


    output$rate_ratio_table = renderTable({
        req(poisson_model())
        get_rate_ratio_table(poisson_model())
    })

    # Negative Binomial outputs
    output$nb_model_formula = renderPrint({
        req(nb_model())
        formula(nb_model())
    })


    output$irr_table = renderTable({
        req(nb_model())
        get_incidence_rate_ratio_table(nb_model())
    })

    # Quasi Poisson outputs
    output$quasi_poisson_formula = renderPrint({
        req(quasi_poisson_model())
        formula(quasi_poisson_model())
    })

    output$quasi_irr_table = renderTable({
        req(quasi_poisson_model())
        get_quasi_rate_ratio_table(quasi_poisson_model())
    })

    output$response_ui = renderUI({
    req(data())
    
    selectInput(
      "response",
      "Select Response Variable (Count)",
      choices = names(data())[sapply(data(), is.numeric)]
    )
    })
    output$predictor_ui = renderUI({
        req(data(), input$response)

        selectInput(
            "predictors",
            "Select Predictor Variable(s)",
            choices = setdiff(names(data()), input$response),
            multiple = TRUE
        )
    })

    # Data Preview
    output$preview = renderTable({
        req(data())
        head(data())
    })

     # Summary stats
    output$summary_stats <- renderPrint({
        req(data(), input$response)
        
        s <- get_count_summary(data(), input$response)
        
        cat("Mean:", s$mean, "\n")
        cat("Variance:", s$variance, "\n")
        cat("Min:", s$min, "\n")
        cat("Max:", s$max, "\n")
        cat("Proportion of zeros:", s$zero_prop, "\n")
    })

    # Plot
    output$count_plot = renderPlot({
    req(data(), input$response)
    
    df <- data()
    y <- df[[input$response]]
    
    ggplot(df, aes(x = y)) +
      geom_bar(fill = "steelblue") +
      labs(
        title = "Count Distribution",
        x = "Count Value",
        y = "Frequency"
      ) +
      theme_minimal()
  })
}

shinyApp(ui, server)

