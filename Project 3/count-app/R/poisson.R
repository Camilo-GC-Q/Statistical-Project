
fit_poisson_model = function(df, response, predictors){

    if (length(predictors == 0)){
        stop("Please choose at least one predictor")
    }

    vars_needed = c(response, predictors)

    model_data = df |>
        select(all_of(vars_needed)) |>
        drop_na()

    formula_text = paste(response, "~", paste(predictors, collapse = " + "))

    model_formula <- as.formula(formula_text)
  
    glm(model_formula, family = poisson(link = "log"), data = model_data)
}