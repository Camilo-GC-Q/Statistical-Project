fit_quasi_poisson_model = function(df, response, predictors){
    if (length(predictors) == 0){
        stop("Please choose at least one predictor")
    }
    vars_needed = c(response, predictors)
    model_data = df |>
        select(all_of(vars_needed)) |>
        drop_na()
    formula_text = paste(response, "~", paste(predictors, collapse = " + "))
    model_formula <- as.formula(formula_text)
    glm(model_formula, family = quasipoisson(link = "log"), data = model_data)
}

get_quasi_poisson_table = function(model){
    broom::tidy(model) %>%
        mutate(across(where(is.numeric), ~ round(.x, 4)))
}

get_quasi_rate_ratio_table = function(model){
    broom::tidy(model) %>%
        transmute(
            term,
            estimate,
            rate_ratio = exp(estimate),
            percent_change = 100 * (exp(estimate) - 1),
            conf.low = exp(estimate - 1.96 * std.error),
            conf.high = exp(estimate + 1.96 * std.error),
            p.value
        ) %>%
        mutate(across(where(is.numeric), ~ round(.x, 4)))
}