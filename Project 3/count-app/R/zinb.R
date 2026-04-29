library(parameters)
library(dplyr)

fit_zinb_model = function(df, response, predictors, formula_str = NULL){
    if (length(predictors) == 0){
        stop("Please choose at least one predictor")
    }
    model_data = df |>
        drop_na()

    fml_str <- if (!is.null(formula_str)) formula_str else
        paste(response, "~", paste(predictors, collapse = " + "))

    model_formula <- as.formula(fml_str)
    pscl::zeroinfl(model_formula, data = model_data, dist = "negbin")
}

get_zinb_table = function(model){
    broom::tidy(model) %>%
        mutate(across(where(is.numeric), ~ round(.x, 4)))
}

get_zinb_irr_table = function(model) {
    params <- parameters::model_parameters(model, exponentiate = FALSE) %>%
        as.data.frame()

    params %>%
        transmute(
            component      = ifelse(Component == "conditional", "Count", "Zero-Inflation"),
            term           = Parameter,
            estimate       = round(Coefficient, 4),
            rate_ratio     = round(exp(Coefficient), 4),
            percent_change = round(100 * (exp(Coefficient) - 1), 2),
            std.error      = round(SE, 4),
            statistic      = round(z, 4),
            conf.low       = round(exp(CI_low), 4),
            conf.high      = round(exp(CI_high), 4),
            p.value        = round(p, 4)
        ) %>%
        arrange(component)
}