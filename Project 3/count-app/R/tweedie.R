library(statmod)  # required for tweedie family
library(tweedie)  # for tweedie.profile to estimate power parameter

fit_tweedie_model <- function(df, response, predictors, formula_str = NULL, var.power = 1.5) {
    if (length(predictors) == 0) {
        stop("Please choose at least one predictor")
    }

    model_data <- df |> tidyr::drop_na()

    fml_str <- if (!is.null(formula_str)) formula_str else
        paste(response, "~", paste(predictors, collapse = " + "))

    model_formula <- as.formula(fml_str)

    # Estimate optimal power parameter via profile likelihood if not specified
    # var.power = 1 is Poisson, = 2 is Gamma, between 1-2 is compound Poisson-Gamma
    fit <- tryCatch({
        statmod::tweedie.profile(
            model_formula,
            data       = model_data,
            p.vec      = seq(1.2, 1.8, by = 0.1),   # search range for p
            link.power = 0,                           # log link
            do.plot    = FALSE
        )
    }, error = function(e) NULL)

    # Use estimated power if profile succeeded, else fall back to supplied var.power
    p_use <- if (!is.null(fit)) fit$p.max else var.power

    model <- glm(
        model_formula,
        data   = model_data,
        family = statmod::tweedie(var.power = p_use, link.power = 0)  # log link
    )

    # Attach power parameter and profile result for downstream use
    model$tweedie_power  <- p_use
    model$tweedie_profile <- fit
    model
}

get_tweedie_irr_table <- function(model) {
    cf  <- coef(model)
    se  <- sqrt(diag(vcov(model)))
    z   <- cf / se
    p   <- 2 * pnorm(abs(z), lower.tail = FALSE)

    data.frame(
        term           = names(cf),
        estimate       = round(cf, 4),
        rate_ratio     = round(exp(cf), 4),
        percent_change = round(100 * (exp(cf) - 1), 4),
        std.error      = round(se, 4),
        statistic      = round(z, 4),
        conf.low       = round(exp(cf - 1.96 * se), 4),
        conf.high      = round(exp(cf + 1.96 * se), 4),
        p.value        = round(p, 4),
        stringsAsFactors = FALSE
    )
}