
fit_zip_model = function(df, response, predictors, formula_str = NULL){
    if(length(predictors) == 0){
        stop("Please choose at least one predictor")
    }

    model_data = df |>
        drop_na()

    fml_str <- if (!is.null(formula_str)) formula_str else
        paste(response, "~", paste(predictors, collapse = " + "))

    model_formula <- as.formula(fml_str)

    pscl::zeroinfl(model_formula, data = model_data, dist = "poisson")
}

get_zip_table = function(model){
    broom::tidy(model) |>
        mutate(across(where(is.numeric), ~ round(.x, 4)))
}

get_zip_irr_table = function(model){
    parameters::model_parameters(model, exponentiate = FALSE) |>
        as.data.frame() |>
        transmute(
            term = Parameter,
            estimate = Coefficient,
            rate_ratio = exp(Coefficient),
            percent_change = 100 * (exp(Coefficient) - 1),
            std.error = SE,
            statistic = z, 
            conf.low = exp(CI_low),
            conf.high = exp(CI_high),
            p.value = p
        ) |>
        mutate(across(where(is.numeric), ~ round(.x, 4)))
}