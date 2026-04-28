fit_negative_binomial_model = function(df, response, predictors, formula_str = NULL){
    if (length(predictors) == 0){
        stop("Please choose at least one predictor")
    }
    model_data = df |>
        drop_na()

    fml_str <- if (!is.null(formula_str)) formula_str else
        paste(response, "~", paste(predictors, collapse = " + "))

    model_formula <- as.formula(fml_str)
    MASS::glm.nb(model_formula, data = model_data)
}

get_negative_binomial_table = function(model){
    broom::tidy(model) %>%
        mutate(across(where(is.numeric), ~ round(.x, 4)))
}

get_incidence_rate_ratio_table = function(model){
    broom::tidy(model) %>%
        transmute(
            term,
            estimate,
            rate_ratio = exp(estimate),
            percent_change = 100 * (exp(estimate) - 1),
            std.error,
            statistic,
            conf.low = exp(estimate - 1.96 * std.error),
            conf.high = exp(estimate + 1.96 * std.error),
            p.value
        ) %>%
        mutate(across(where(is.numeric), ~ round(.x, 4)))
}
