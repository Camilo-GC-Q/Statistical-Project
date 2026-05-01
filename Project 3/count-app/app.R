library(shiny)
library(tidyverse)
library(shinyWidgets)
library(ggeffects)
list.files("R", full.names = TRUE) |> purrr::walk(source)

ui = fluidPage(
    titlePanel("Count Regression Toolkit"),
    sidebarLayout(
        sidebarPanel(
            fileInput("file", "Upload CSV File", accept = ".csv"),
            uiOutput("response_ui"),
            uiOutput("predictor_ui"),
            uiOutput("offset_ui"),
            uiOutput("scale_ui"),
            hr(),
            selectInput("model_type", "Select Model to Fit",
                choices = c("Poisson", "Quasipoisson", "Negative Binomial",
                "Zero-Inflated Poisson", "Zero-Inflated Negative Binomial", "Tweedie")),
            actionButton("fit_model", "Fit Model", class = "btn btn-primary")
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("Data Preview",
                    tableOutput("preview")
                ),
                tabPanel("Data Summary",
                    verbatimTextOutput("summary_stats"),
                    downloadButton("dl_count_plot", "Download", icon = icon("Download")),
                    plotOutput("count_plot"),
                    hr(),
                    h4("Pairwise Plots"),
                    downloadButton("dl_pair_plot", "Download", icon = icon("Download")),
                    plotOutput("pair_plot", height = "800px"),
                    hr(),
                    h4("Correlation Matrix of Model Terms"),
                    tableOutput("coeff_cor_table")
                ),
                tabPanel("Diagnostics",
                    sidebarLayout(
                        sidebarPanel(
                            uiOutput("rqr_checks_ui"),
                            hr(),
                            tags$p(tags$strong("Assumption Checks")),
                            tags$p("Fit a Poisson model, then click to run checks."),
                            actionButton("check_poisson_assumptions", "Check Assumptions",
                                class = "btn btn-primary"),
                            br(),
                            br(),
                            uiOutput("assumption_checks_ui"),
                            hr(),
                            tags$p(tags$strong("Influence Checks")),
                            tags$p("Make sure to evaluate whether there are outliers or influential points:"),
                            br(),
                            checkboxInput("assume_leverage",
                                "Few/no observations with large leverage values.", value = FALSE),
                            checkboxInput("assume_cooks",
                                "Few/no observations with large Cook's distance values.", value = FALSE),
                            checkboxInput("assume_dffits",
                                "Few/no observations with DFFITS with large magnitude.", value = FALSE),
                            br(),
                            actionButton("check_influence", "Check Observations",
                                class = "btn btn-secondary"),
                            br(), br(),
                            uiOutput("influence_verdict_ui")
                        ),
                        mainPanel(
                            h4("VIF Table"),
                            tableOutput("vif_table_output"),
                            hr(),
                            downloadButton("dl_rqr_plot", "Download", icon = icon("Download")),
                            plotOutput("rqr_plot", height = "700px"),
                            hr(),
                            h4("Influence Diagnostics"),
                            downloadButton("dl_influence_plot", "Download", icon = icon("Download")),
                            plotOutput("assumption_influence_plot", height = "700px"),
                            uiOutput("zero_inflation_test_ui")
                        )
                    )
                ),
                tabPanel("Interpretation",
                    br(),
                    h4(textOutput("interp_model_label")),
                    hr(),
                    h4("Incidence Rate Ratios"),
                    tableOutput("dynamic_irr_table"),
                    hr(),
                    uiOutput("interpretation_ui")
                ),
                tabPanel("Interaction",
                    br(),    
                    sidebarLayout(
                        sidebarPanel(
                            uiOutput("jn_interaction_ui"),
                            uiOutput("jn_moderator_ui")
                        ),
                        mainPanel(
                            h4("Simple Slopes Visualization"),
                            downloadButton("dl_simple_slopes", "Download", icon = icon("Download")),
                            plotOutput("simple_slopes_plot", height = "400px"),
                            hr(),
                            h4("Estimated Marginal Means"),
                            tableOutput("emmeans_table"),
                            uiOutput("emmeans_interpretation_ui"),
                            hr(),
                            h4("Contrasts of Marginal Means"),
                            tableOutput("emmeans_contrasts_table"),
                            uiOutput("emmeans_contrasts_interpretation_ui"),
                            hr(),
                            h4("Marginal Effects (emtrends)"),
                            tableOutput("emtrends_table"),
                            uiOutput("emtrends_interpretation_ui"),
                            hr(),
                            h4("Contrasts of Marginal Effects"),
                            tableOutput("emtrends_contrasts_table"),
                            uiOutput("emtrends_contrasts_interpretation_ui"),
                            hr(),
                            h4("Johnson-Neyman Plot"),
                            downloadButton("dl_jn_plot", "Download", icon = icon("Download")),
                            plotOutput("jn_plot", height = "500px")
                        )
                    )  
                )
            )
        )
    )
)

server = function(input, output, session){

    #model selection
    selected_model_type = reactive(input$model_type)

    # Data input
    data = reactive({
        req(input$file)
        read_csv(input$file$datapath)
    })

    # Scaled data
    scaled_data = reactive({
        req(data())
        df = data()
        vars = input$scale_vars
        if (!is.null(vars) && length(vars) > 0) {
            df[vars] = lapply(df[vars], scale)
        }
        df
    })

    # Interaction coefficient 
    output$interp_model_label = renderText({
        paste("Model Formula —", selected_model_type())
    })

    output$dynamic_irr_table = renderTable({
        req(selected_model_type())
        model <- resolve_model(selected_model_type())
        req(model)

        switch(selected_model_type(),
            "Poisson"                        = get_rate_ratio_table(model),
            "Quasipoisson"                   = get_quasi_rate_ratio_table(model),
            "Negative Binomial"              = get_incidence_rate_ratio_table(model),
            "Zero-Inflated Poisson"          = get_zip_irr_table(model),
            "Zero-Inflated Negative Binomial" = get_zinb_irr_table(model),
            "Tweedie" = get_tweedie_irr_table(model)
        )
    })

    # RQR plots
    output$rqr_plot = renderPlot({
        req(selected_model_type())
        model = resolve_model(selected_model_type())
        validate(need(!is.null(model),
            paste("Please fit the", selected_model_type(), "model first.")))
        plotRQR(model)
    }, height = 800)

    rqr_results = reactive({
        req(selected_model_type())
        model = resolve_model(selected_model_type())
        validate(need(!is.null(model),
            paste("Please fit the", selected_model_type(), "model first.")))
        plotRQR(model)
    })

    output$rqr_plot = renderPlot({
        rqr_results()$plot
    }, height = 700)

    output$rqr_checks_ui = renderUI({
        req(rqr_results())
        checks = rqr_results()$checks

        make_item = function(label, result) {
            color = if (result$flagged) "red" else "darkgreen"
            tagList(
                tags$div(
                    style = "margin-bottom:10px;",
                    tags$strong(label),
                    tags$br(),
                    tags$span(result$message, style = paste0("color:", color, ";"))
                ),
                tags$hr(style = "margin:6px 0;")
            )
        }

        rec_color = if (grepl("appears adequately|most flexible", checks$recommendation))
            "darkgreen" else "darkorange"

        tagList(
            make_item("Normality of RQRs",      checks$findings$normality),
            make_item("Dispersion",             checks$findings$dispersion),
            make_item("Excess Zeros",           checks$findings$zeros),
            make_item("Mean-Variance Relation", checks$findings$mean_variance),
            tags$hr(),
            tags$h4("Model Recommendation"),
            tags$p(style = paste0("color:", rec_color, "; font-weight:bold;"),
                checks$recommendation)
        )
    })

    # Offset application
    output$offset_ui = renderUI({
        req(data(), input$response)
        numerics = names(data())[sapply(data(), is.numeric)]
        numerics = setdiff(numerics, input$response)
        selectInput("offset_var", "Select Offset Variable", choices = c("None", numerics))
    })

    # Scale application
    output$scale_ui = renderUI({
        req(data(), input$predictors)
    
        pure_preds = input$predictors[!grepl(":", input$predictors)]
    
        # Nothing to scale if no pure predictors selected
        if (length(pure_preds) == 0) return(NULL)
    
    # Guard against subscript errors from empty/bad column names
        valid_preds = pure_preds[pure_preds %in% names(data())]
        if (length(valid_preds) == 0) return(NULL)
    
        numerics = valid_preds[sapply(data()[, valid_preds, drop = FALSE], is.numeric)]
        if (length(numerics) == 0) return(NULL)
    
        pickerInput("scale_vars", "Scale Variable(s)",
            choices = numerics,
            options = list('actions-box' = TRUE,
                        'none-selected-text' = "No scaling"),
            multiple = TRUE
        )
    })

    # Formula builder
    build_formula = reactive({
        req(input$response, input$predictors)
        preds      <- input$predictors
        pure_preds <- preds[!grepl(":", preds)]
        pred_ixns  <- preds[grepl(":", preds)]

        rhs_terms <- c(pure_preds, pred_ixns)

        formula_str = paste(input$response, "~", paste(rhs_terms, collapse = " + "))

        if(!is.null(input$offset_var) && input$offset_var != "None"){
            formula_str = paste(formula_str, "+ offset(log(", input$offset_var, "))")
        }

        formula_str
    })

    # Fitting model

    poisson_assumptions = eventReactive(input$check_poisson_assumptions, {
        req(poisson_model(), data(), input$response, input$predictors)
        check_poisson_assumptions(poisson_model(), data(), input$response, input$predictors)
    })

    # Model Resolver
    fitted_models = reactiveValues(
        Poisson           = NULL,
        Quasipoisson      = NULL,
        `Negative Binomial` = NULL,
        `Zero-Inflated Poisson` = NULL,
        `Zero-Inflated Negative Binomial` = NULL,
        `Tweedie` = NULL
    )

    observeEvent(input$fit_model, {
        req(data(), input$response, input$predictors, input$model_type)
        fml <- build_formula()

        fitted_models[[input$model_type]] <- switch(input$model_type,
            "Poisson"           = fit_poisson_model(scaled_data(), input$response, input$predictors, formula_str = fml),
            "Quasipoisson"      = fit_quasi_poisson_model(scaled_data(), input$response, input$predictors, formula_str = fml),
            "Negative Binomial" = fit_negative_binomial_model(scaled_data(), input$response, input$predictors, formula_str = fml),
            "Zero-Inflated Poisson" = fit_zip_model(scaled_data(), input$response, input$predictors, formula_str = fml),
            "Zero-Inflated Negative Binomial"  = fit_zinb_model(scaled_data(), input$response, input$predictors, formula_str = fml),
            "Tweedie" = fit_tweedie_model(scaled_data(), input$response,
                                input$predictors, formula_str = fml)
        )
    })

    resolve_model = function(type) fitted_models[[type]]

    poisson_model       = reactive(fitted_models[["Poisson"]])
    quasi_poisson_model = reactive(fitted_models[["Quasipoisson"]])
    nb_model            = reactive(fitted_models[["Negative Binomial"]])
    zip_model           = reactive(fitted_models[["Zero-Inflated Poisson"]])
    zinb_model          = reactive(fitted_models[["Zero-Inflated Negative Binomial"]])
    tweedie_model = reactive(fitted_models[["Tweedie"]])

    # Formula display 
    formula_render = function() {
        req(input$response, input$predictors)
        validate(need(length(input$predictors) > 0, "Please select at least one predictor."))
        cat(build_formula())
    }

    output$model_formula         = renderPrint(formula_render())
    output$nb_model_formula      = renderPrint(formula_render())
    output$quasi_poisson_formula = renderPrint(formula_render())
    output$zinb_model_formula    = renderPrint(formula_render())
    output$zip_model_formula     = renderPrint(formula_render())
    output$cmp_model_formula     = renderPrint(formula_render())

    # Model tables
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


    # Pairwise plots 
    output$pair_plot = renderPlot({
        req(data(), input$response, input$predictors, selected_model_type())
        plot_count_pairs(
            df         = data(),
            response   = input$response,
            predictors = input$predictors,
            model_type = selected_model_type()
        )
    })

    # Coefficient correlation matrix
    output$coeff_cor_table = renderTable({
    req(selected_model_type())
    model_to_display <- resolve_model(selected_model_type())
    req(model_to_display)
    get_coeff_correlation_matrix(model_to_display)
    }, rownames = TRUE, striped = TRUE, hover = TRUE, bordered = TRUE)

    # Residual diagnostics 
    output$assumption_residual_plot = renderPlot({
        req(poisson_model())
        plotResiduals(poisson_model())
    }, height = 700)

    # Influence Plot
    output$assumption_influence_plot = renderPlot({
        req(poisson_model())
        plotInfluence(poisson_model())
    }, height = 700)

    output$zero_inflation_test_ui = renderUI({
        req(selected_model_type())
        type <- selected_model_type()
        if (type %in% c("Zero-Inflated Poisson", "Zero-Inflated Negative Binomial",
                  "Tweedie")) return(NULL)
        model <- resolve_model(type)
        req(model)
        tagList(
            hr(),
            h4("Zero-Inflation Test"),
            downloadButton("dl_zero_inflation_plot", "Download", icon = icon("Download")),
            plotOutput("zero_inflation_plot", height = "400px")
        )
    })

    output$zero_inflation_plot = renderPlot({
        req(selected_model_type())
        type <- selected_model_type()
        req(!type %in% c("Zero-Inflated Poisson", "Zero-Inflated Negative Binomial"))
        model <- resolve_model(type)
        req(model)
        sim = DHARMa::simulateResiduals(model)
        DHARMa::testZeroInflation(sim)
    }, height = 400)


    influence_checked = eventReactive(input$check_influence, {
        list(
            leverage  = input$assume_leverage,
            cooks     = input$assume_cooks,
            dffits    = input$assume_dffits,
            residuals = input$assume_residuals
        )
    })

    output$influence_verdict_ui = renderUI({
        req(influence_checked())
        chk <- influence_checked()
        all_checked <- all(unlist(chk))

        if (all_checked) {
            tags$p(style = "color:darkgreen; font-weight:bold;",
            "   The model likely isn't affected by outliers or high-leverage points. ",
            "   Please proceed to the next step.")
    }    else {
            tags$p(style = "color:red; font-weight:bold;",
                "The model may be affected by outliers or influential points. ",
                "Consider investigating flagged observations before interpreting results.")
        }
    })

    # Interpretation
    output$interpretation_ui = renderUI({
        req(selected_model_type())
    
        model <- tryCatch(
            resolve_model(selected_model_type()),
            error = function(e) NULL
        )
    
        if (is.null(model)) {
            return(tags$p("Please fit this model first.", style = "color:grey;"))
        }
    
        interp <- interpret_count_model(model, input$response, input$predictors)
    
        #Count component
        count_items <- purrr::map(interp$count_sentences, function(s) {
            tags$li(s, style = "margin-bottom: 8px;")
        })
    
        out <- tagList(
            tags$h4("Count Component"),
            tags$ul(count_items)
        )
    
        # Zero component
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
    if (selected_model_type() == "Quasipoisson") {
        out <- tagList(out, tags$hr(), tags$p(
            tags$strong("Note: "),
            "Quasi-Poisson adjusts standard errors for overdispersion but ",
            "rate ratio point estimates are identical to Poisson. ",
            "Inference (p-values, CIs) is more reliable when overdispersion is present."
        ))
    }

    if (selected_model_type() == "Tweedie") {
    p_val <- tryCatch(round(model$tweedie_power, 3), error = function(e) "unknown")
    out <- tagList(out, tags$hr(), tags$p(
        tags$strong("Note: "),
        paste0("Tweedie GLM fitted with power parameter p = ", p_val, ". "),
        "Rate ratios are exponentiated log-link coefficients. ",
        "p \u2248 1 is Poisson-like; p \u2248 2 is Gamma-like; ",
        "1 < p < 2 is compound Poisson-Gamma (allows exact zeros with continuous positive values)."
    ))
}
    out})

    # Assumption checks
    output$vif_table_output = renderTable({
    req(selected_model_type())
    model <- resolve_model(selected_model_type())
    validate(need(!is.null(model),
        paste("Please fit the", selected_model_type(), "model first.")))
    
    # Need at least 2 predictors for VIF
    validate(need(length(input$predictors) >= 2,
        "VIF requires at least two predictors."))
    
    vif_vals <- tryCatch(car::vif(model), error = function(e) NULL)
    
    validate(need(!is.null(vif_vals),
        "VIF could not be computed for this model."))
    
    # car::vif returns vector for main effects, matrix for models with interactions
    vif_numeric <- if (is.matrix(vif_vals)) vif_vals[, "GVIF^(1/(2*Df))"]^2 else vif_vals
    
    data.frame(
        Term = names(vif_numeric),
        VIF  = round(vif_numeric, 3)
    )
}, striped = TRUE, hover = TRUE, bordered = TRUE)

    output$assumption_checks_ui = renderUI({
        req(poisson_assumptions())
        a <- poisson_assumptions()

        make_item <- function(label, result, extra_ui = NULL) {
            icon_col <- if (result$flagged) "red" else "darkgreen"
            tagList(
                tags$div(
                    style = "margin-bottom:10px;",
                    tags$span(style = paste0("color:", icon_col, "; font-size:1.1em;")),
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

    #Data preview & summary 
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

        interaction_choices <- character(0)
        if (length(c(numerics, categoricals)) >= 2) {
            pairs_list <- combn(c(numerics, categoricals), 2, simplify = FALSE)
            interaction_choices <- setNames(
                vapply(pairs_list, \(p) paste(p, collapse = ":"), character(1)),
                vapply(pairs_list, \(p) paste(p, collapse = " \u00d7 "), character(1))
            )
        }

        pickerInput("predictors", "Select Predictor Variable(s)",
            choices  = list("Numeric" = numerics, "Categorical" = categoricals, "Interactions" = interaction_choices),
            options  = list(`actions-box` = TRUE),
            multiple = TRUE)
    })

    # Jonhson-Neyman Plot
    output$jn_interaction_ui = renderUI({
        req(input$predictors)
        ixn_terms = input$predictors[grepl(":", input$predictors)]
        req(length(ixn_terms) > 0)
        selectInput("jn_interaction", "Select Interaction", choices = ixn_terms)
})

    output$jn_moderator_ui = renderUI({
        req(input$jn_interaction)
        vars = strsplit(input$jn_interaction, ":")[[1]]
        selectInput("jn_moderator", "Select Moderator Variable", choices = vars)
    })

    # Slopes plot
    output$simple_slopes_plot = renderPlot({
        req(input$jn_interaction, input$jn_moderator, selected_model_type())
        model    <- resolve_model(selected_model_type())
        req(model)
        vars      <- strsplit(input$jn_interaction, ":")[[1]]
        int.var   <- vars[vars != input$jn_moderator]
        moderator <- input$jn_moderator
        dat       <- scaled_data()

        int.vars.classes <- sapply(dat[, c(int.var, moderator)], class)

        if (all(int.vars.classes == "numeric")) {
            m.mod <- mean(unlist(model$model[moderator]), na.rm = TRUE)
            s.mod <- sd(unlist(model$model[moderator]),   na.rm = TRUE)
            terms_spec <- c(int.var, paste0(moderator, " [", round(m.mod - s.mod, 2), ",", round(m.mod + s.mod, 2), "]"))
            pred <- data.frame(ggemmeans(model, terms = terms_spec))
            pred <- pred |> mutate(group = case_when(
                group == round(m.mod - s.mod, 2) ~ paste0("Low (Mean - 1SD = ", round(m.mod - s.mod, 2), ")"),
                TRUE                             ~ paste0("High (Mean + 1SD = ", round(m.mod + s.mod, 2), ")")
            ))
            ggplot(pred, aes(x = x, y = predicted, color = group, fill = group)) +
                geom_line() +
                geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, linetype = "dotted") +
                labs(x = int.var, y = paste("Predicted", input$response), color = moderator, fill = moderator) +
                scale_color_brewer(palette = "Set1") +
                scale_fill_brewer(palette = "Set1") +
                theme_bw()
        } else {
            pred <- data.frame(ggemmeans(model, terms = c(int.var, moderator)))
            ggplot(pred, aes(x = x, y = predicted, color = group, fill = group)) +
                geom_point(position = position_dodge(0.25)) +
                geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                              position = position_dodge(0.25), width = 0.1) +
                labs(x = int.var, y = paste("Predicted", input$response), color = moderator, fill = moderator) +
                scale_color_brewer(palette = "Set1") +
                scale_fill_brewer(palette = "Set1") +
                theme_bw()
        }
    }, height = 400)

    output$jn_plot = renderPlot({
    req(input$jn_interaction, input$jn_moderator, selected_model_type())
    model = resolve_model(selected_model_type())
    req(model)

    vars = strsplit(input$jn_interaction, ":")[[1]]
    pred = vars[vars != input$jn_moderator][1]
    moderator = input$jn_moderator

    tryCatch({
        plot_johnson_neyman(model, pred, moderator)
    }, error = function(e) {
        print(e)  # shows in console
        ggplot() +
            annotate("text", x = 0.5, y = 0.5,
                     label = paste("Error:", e$message),
                     size = 5) +
            theme_void()
    })
})

    # EMM Table Output
    output$emmeans_table = renderTable({
        req(input$jn_interaction, input$jn_moderator, selected_model_type())
        model = resolve_model(selected_model_type())
        req(model)
        vars = strsplit(input$jn_interaction, ":")[[1]]
        int.var = vars[vars != input$jn_moderator]
        moderator = input$jn_moderator
        get_emmeans_table(model, int.var, moderator, data())
    })

    output$emmeans_contrasts_table = renderTable({
        req(input$jn_interaction, input$jn_moderator, selected_model_type())
        model = resolve_model(selected_model_type())
        req(model)
        vars      = strsplit(input$jn_interaction, ":")[[1]]
        int.var   = vars[vars != input$jn_moderator]
        moderator = input$jn_moderator
        get_emmeans_contrasts(model, int.var, moderator, data())
    })

    output$emtrends_table = renderTable({
        req(input$jn_interaction, input$jn_moderator, selected_model_type())
        model = resolve_model(selected_model_type())
        req(model)
        vars = strsplit(input$jn_interaction, ":")[[1]]
        int.var = vars[vars != input$jn_moderator]
        moderator = input$jn_moderator
        get_emtrends_table(model, int.var, moderator, data())
    })

    output$emtrends_contrasts_table = renderTable({
        req(input$jn_interaction, input$jn_moderator, selected_model_type())
        model = resolve_model(selected_model_type())
        req(model)
        vars = strsplit(input$jn_interaction, ":")[[1]]
        int.var = vars[vars != input$jn_moderator]
        moderator = input$jn_moderator
        get_emtrends_contrasts(model, int.var, moderator, data())
    })

    # Emm interpretation
    output$emmeans_interpretation_ui = renderUI({
        req(input$jn_interaction, input$jn_moderator, selected_model_type())
        model = resolve_model(selected_model_type())
        req(model)
        vars      = strsplit(input$jn_interaction, ":")[[1]]
        int.var   = vars[vars != input$jn_moderator]
        moderator = input$jn_moderator

        tbl = get_emmeans_table(model, int.var, moderator, data())

        # Derive outcome name from model
        outcome = as.character(formula(model)[[2]])

        bullets = interpret_emmeans(tbl, outcome, int.var, moderator)

        tags$ul(
            purrr::map(bullets, ~ tags$li(.x, style = "margin-bottom: 6px;"))
        )
    })

    #EMM Contrast Interpretation
    output$emmeans_contrasts_interpretation_ui = renderUI({
        req(input$jn_interaction, input$jn_moderator, selected_model_type())
        model = resolve_model(selected_model_type())
        req(model)
        vars      = strsplit(input$jn_interaction, ":")[[1]]
        int.var   = vars[vars != input$jn_moderator]
        moderator = input$jn_moderator

        ct      = get_emmeans_contrasts(model, int.var, moderator, data())
        outcome = as.character(formula(model)[[2]])
        bullets = interpret_emmeans_contrasts(ct, outcome)

        tags$ul(
            purrr::map(bullets, ~ tags$li(.x, style = "margin-bottom: 6px;"))
        )
    })

    #EMtrends and Contrasts Interpretation
    output$emtrends_interpretation_ui = renderUI({
        req(input$jn_interaction, input$jn_moderator, selected_model_type())
        model = resolve_model(selected_model_type())
        req(model)
        vars      = strsplit(input$jn_interaction, ":")[[1]]
        int.var   = vars[vars != input$jn_moderator][1]
        moderator = input$jn_moderator

        tbl = get_emtrends_table(model, int.var, moderator, data())
        req(!is.null(tbl))
        bullets = interpret_emtrends(tbl, int.var, moderator)
        tags$ul(purrr::map(bullets, ~ tags$li(.x, style = "margin-bottom: 6px;")))
    })

    output$emtrends_contrasts_interpretation_ui = renderUI({
        req(input$jn_interaction, input$jn_moderator, selected_model_type())
        model = resolve_model(selected_model_type())
        req(model)
        vars      = strsplit(input$jn_interaction, ":")[[1]]
        int.var   = vars[vars != input$jn_moderator][1]
        moderator = input$jn_moderator

        ct = get_emtrends_contrasts(model, int.var, moderator, data())
        req(!is.null(ct))
        bullets = interpret_emtrends_contrasts(ct, int.var, moderator)
        tags$ul(purrr::map(bullets, ~ tags$li(.x, style = "margin-bottom: 6px;")))
    })


}

shinyApp(ui, server)