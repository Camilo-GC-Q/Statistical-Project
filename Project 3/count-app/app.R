library(shiny)
library(tidyverse)
library(shinyWidgets)
list.files("R", full.names = TRUE) |> purrr::walk(source)

ui = fluidPage(
    titlePanel("Count Regression Toolkit"),
    sidebarLayout(
        sidebarPanel(
            fileInput("file", "Upload CSV File", accept = ".csv"),
            uiOutput("response_ui"),
            uiOutput("predictor_ui"),
            selectInput("plot_model_type", "Select Model Type for Plotting",
                choices = c("Poisson", "Quasipoisson", "Negative Binomial", "ZIP", "ZINB")),
            hr(),
            actionButton("fit_poisson", "Fit Poisson Model"),
            br(), br(),
            actionButton("fit_quasi_poisson", "Fit Quasi Poisson Model"),
            br(), br(),
            actionButton("fit_nb", "Fit Negative Binomial Model"),
            br(), br(),
            actionButton("fit_zip", "Fit Zero-Inflated Poisson Model"),
            br(), br(),
            actionButton("fit_zinb", "Fit Zero-Inflated Negative Binomial Model")
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("Data Preview",
                    tableOutput("preview")
                ),
                tabPanel("Data Summary",
    verbatimTextOutput("summary_stats"),
    plotOutput("count_plot"),
    hr(),
    h4("Pairwise Plots"),
    selectInput("plot_model_type", "Select Model Type for Plotting",
        choices = c("Poisson", "Quasipoisson", "Negative Binomial", "ZIP", "ZINB")),
    plotOutput("pair_plot", height = "800px"),
    hr(),
    h4("Correlation Matrix of Model Terms"),
    selectInput("cor_model_type", "Select Model",
        choices = c("Poisson", "Quasipoisson", 
                    "Negative Binomial", "ZIP", "ZINB")
    ),
    tableOutput("coeff_cor_table")
),
                tabPanel("Assumptions",
                    sidebarLayout(
                        sidebarPanel(
                            tags$p(tags$strong("Step 1:"),
                                " Confirm assumptions verified by study design."),
                            checkboxInput("assume_independent",
                                "Observations are random and independent",
                                value = FALSE),
                            hr(),
                            tags$p(tags$strong("Step 2:"),
                                " Fit a Poisson model, then click to run checks."),
                            actionButton("check_poisson_assumptions", "Check Assumptions",
                                class = "btn btn-primary"),
                            br(), br(),
                            uiOutput("assumption_checks_ui")
                        ),
                        mainPanel(
                            br(),
                            h4("VIF Table"),
                            tableOutput("vif_table_output"),
                            hr(),
                            plotOutput("assumption_residual_plot", height = "700px")
                        )
                    )
                ),
                tabPanel("Outliers"),
                tabPanel("Interpretation",
    br(),
    selectInput("interp_model_type", "Select Model to Interpret",
        choices = c("Poisson", "Quasipoisson", 
                    "Negative Binomial", "ZIP", "ZINB")
    ),
    uiOutput("interpretation_ui")
),
                tabPanel("Interaction"),
                tabPanel("Poisson Model",
                    verbatimTextOutput("model_formula"),
                    h4("Incidence Rate Ratios"),
                    tableOutput("rate_ratio_table")
                ),
                tabPanel("Quasi-Poisson Model",
                    verbatimTextOutput("quasi_poisson_formula"),
                    h4("Incidence Rate Ratios"),
                    tableOutput("quasi_irr_table")
                ),
                tabPanel("Negative Binomial Model",
                    verbatimTextOutput("nb_model_formula"),
                    h4("Incidence Rate Ratios"),
                    tableOutput("irr_table")
                ),
                tabPanel("Zero-Inflated Poisson Model",
                    verbatimTextOutput("zip_model_formula"),
                    h4("Incidence Rate Ratios"),
                    tableOutput("zip_irr_table")
                ),
                tabPanel("Zero-Inflated Negative Binomial Model",
                    verbatimTextOutput("zinb_model_formula"),
                    h4("Incidence Rate Ratios"),
                    tableOutput("zinb_irr_table")
                )
            )
        )
    )
)

server = function(input, output, session){

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

    zinb_model = eventReactive(input$fit_zinb, {
        req(data(), input$response, input$predictors)
        fit_zinb_model(data(), input$response, input$predictors)
    })

    zip_model = eventReactive(input$fit_zip, {
        req(data(), input$response, input$predictors)
        fit_zip_model(data(), input$response, input$predictors)
    })

    poisson_assumptions = eventReactive(input$check_poisson_assumptions, {
        req(poisson_model(), data(), input$response, input$predictors)
        check_poisson_assumptions(poisson_model(), data(), input$response, input$predictors)
    })

    # Helper to resolve the active model from a selector input
    resolve_model = function(type) {
        switch(type,
            "Poisson"           = poisson_model(),
            "Quasipoisson"      = quasi_poisson_model(),
            "Negative Binomial" = nb_model(),
            "ZIP"               = zip_model(),
            "ZINB"              = zinb_model()
        )
    }

    # ── Formulas ────────────────────────────────────────────────────────────
    output$model_formula = renderPrint({
        req(input$response, input$predictors)
        validate(need(length(input$predictors) > 0, "Please select at least one predictor."))
        cat(paste(input$response, "~", paste(input$predictors, collapse = " + ")))
    })

    output$nb_model_formula = renderPrint({
        req(input$response, input$predictors)
        cat(paste(input$response, "~", paste(input$predictors, collapse = " + ")))
    })

    output$quasi_poisson_formula = renderPrint({
        req(input$response, input$predictors)
        cat(paste(input$response, "~", paste(input$predictors, collapse = " + ")))
    })

    output$zinb_model_formula = renderPrint({
        req(input$response, input$predictors)
        cat(paste(input$response, "~", paste(input$predictors, collapse = " + ")))
    })

    output$zip_model_formula = renderPrint({
        req(input$response, input$predictors)
        cat(paste(input$response, "~", paste(input$predictors, collapse = " + ")))
    })

    # ── Model tables ─────────────────────────────────────────────────────────
    output$rate_ratio_table = renderTable({
        req(poisson_model())
        get_rate_ratio_table(poisson_model())
    })

    output$irr_table = renderTable({
        req(nb_model())
        get_incidence_rate_ratio_table(nb_model())
    })

    output$quasi_irr_table = renderTable({
        req(quasi_poisson_model())
        get_quasi_rate_ratio_table(quasi_poisson_model())
    })

    output$zinb_irr_table = renderTable({
        req(zinb_model())
        get_zinb_irr_table(zinb_model())
    })

    output$zip_irr_table = renderTable({
        req(zip_model())
        get_zip_irr_table(zip_model())
    })

    # ── Pairwise plots ───────────────────────────────────────────────────────
    output$pair_plot = renderPlot({
        req(data(), input$response, input$predictors, input$plot_model_type)
        plot_count_pairs(
            df         = data(),
            response   = input$response,
            predictors = input$predictors,
            model_type = input$plot_model_type
        )
    })

    # ── Coefficient correlation matrix ───────────────────────────────────────
    output$coeff_cor_table = renderTable({
    req(input$cor_model_type)
    model_to_display <- resolve_model(input$cor_model_type)
    req(model_to_display)
    get_coeff_correlation_matrix(model_to_display)
}, rownames = TRUE, striped = TRUE, hover = TRUE, bordered = TRUE)

    # ── Residual diagnostics (standalone tab removed, lives in Assumptions) ──
    output$assumption_residual_plot = renderPlot({
        req(poisson_model())
        plotResiduals(poisson_model())
    }, height = 700)

    output$interpretation_ui = renderUI({
    req(input$interp_model_type)
    
    model <- tryCatch(
        resolve_model(input$interp_model_type),
        error = function(e) NULL
    )
    
    if (is.null(model)) {
        return(tags$p("Please fit this model first.", style = "color:grey;"))
    }
    
    interp <- interpret_count_model(model, input$response, input$predictors)
    
    # Count component
    count_items <- purrr::map(interp$count_sentences, function(s) {
        tags$li(s, style = "margin-bottom: 8px;")
    })
    
    out <- tagList(
        tags$h3(paste(interp$model_label, "— Interpretation")),
        tags$h4("Count Component"),
        tags$ul(count_items)
    )
    
    # Zero component (ZIP/ZINB only)
    if (interp$is_zeroinfl && !is.null(interp$zero_sentences)) {
        zero_items <- purrr::map(interp$zero_sentences, function(s) {
            tags$li(s, style = "margin-bottom: 8px;")
        })
        out <- tagList(
            out,
            tags$hr(),
            tags$h4("Zero-Inflation Component"),
            tags$p(tags$em(
                "These coefficients are on the log-odds scale. ",
                "Exponentiated values are odds ratios for being a structural zero."
            )),
            tags$ul(zero_items)
        )
    }
    
    # Quasi-Poisson note
    if (input$interp_model_type == "Quasipoisson") {
        out <- tagList(out, tags$hr(), tags$p(
            tags$strong("Note: "),
            "Quasi-Poisson adjusts standard errors for overdispersion but ",
            "rate ratio point estimates are identical to Poisson. ",
            "Inference (p-values, CIs) is more reliable when overdispersion is present."
        ))
    }
    
    out
})

    # ── Assumption checks ────────────────────────────────────────────────────
    output$vif_table_output = renderTable({
        req(poisson_assumptions())
        poisson_assumptions()$multicollinearity$vif_table
    }, digits = 3)

    output$poisson_assumption_plots = renderPlot({
        req(poisson_model(), data(), input$response, input$predictors)
        plot_poisson_assumptions(poisson_model(), data(), input$response, input$predictors)
    }, height = 700)

    output$assumption_checks_ui = renderUI({
        req(poisson_assumptions())
        a <- poisson_assumptions()

        make_item <- function(label, result, extra_ui = NULL) {
            icon_col <- if (result$flagged) "red" else "darkgreen"
            icon     <- if (result$flagged) "\u274c" else "\u2705"
            tagList(
                tags$div(
                    style = "margin-bottom:10px;",
                    tags$span(icon, style = paste0("color:", icon_col, "; font-size:1.1em;")),
                    tags$strong(paste0(" ", label)),
                    tags$br(),
                    tags$span(result$message, style = paste0("color:", icon_col, ";")),
                    if (!is.null(extra_ui)) extra_ui
                ),
                tags$hr(style = "margin:6px 0;")
            )
        }

        epp_detail <- tags$small(glue::glue(
            " | total events = {a$events_per_pred$total_events}, ",
            "p = {a$events_per_pred$n_predictors}, ",
            "EPP = {a$events_per_pred$epp}"
        ))

        tagList(
            tags$h4("Poisson Assumption Checks"),
            make_item("1. Count Response Variable",                      a$count_response),
            make_item("2. Mean\u2013Variance Equality (Equidispersion)", a$mean_variance),
            make_item("3. Overdispersion (Deviance / df)",               a$overdispersion),
            make_item("4. Excess Zeros",                                 a$zero_inflation),
            make_item("5. Linearity of log(\u03bb) in Predictors",       a$linearity),
            make_item("6. Multicollinearity (VIF)",                      a$multicollinearity),
            make_item("7. Events Per Predictor",                         a$events_per_pred, epp_detail),
            make_item("8. Goodness of Fit (Pearson \u03c7\u00b2)",       a$goodness_of_fit)
        )
    })

    # ── Data preview & summary ───────────────────────────────────────────────
    output$preview = renderTable({
        req(data())
        head(data())
    })

    output$summary_stats = renderPrint({
        req(data(), input$response)
        s <- get_count_summary(data(), input$response)
        cat("Mean:", s$mean, "\n")
        cat("Variance:", s$variance, "\n")
        cat("Min:", s$min, "\n")
        cat("Max:", s$max, "\n")
        cat("Proportion of zeros:", s$zero_prop, "\n")
    })

    output$count_plot = renderPlot({
        req(data(), input$response)
        df <- data()
        y  <- df[[input$response]]
        ggplot(df, aes(x = y)) +
            geom_bar(fill = "steelblue") +
            labs(title = "Count Distribution", x = "Count Value", y = "Frequency") +
            theme_minimal()
    })

    output$response_ui = renderUI({
        req(data())
        selectInput("response", "Select Response Variable (Count)",
            choices = names(data())[sapply(data(), is.numeric)])
    })

    output$predictor_ui = renderUI({
        req(data(), input$response)
        all_vars     <- setdiff(names(data()), input$response)
        numerics     <- all_vars[sapply(data()[, all_vars, drop = FALSE], is.numeric)]
        categoricals <- all_vars[sapply(data()[, all_vars, drop = FALSE], function(x) !is.numeric(x))]
        pickerInput("predictors", "Select Predictor Variable(s)",
            choices  = list("Numeric" = numerics, "Categorical" = categoricals),
            options  = list(`actions-box` = TRUE),
            multiple = TRUE)
    })
}

shinyApp(ui, server)