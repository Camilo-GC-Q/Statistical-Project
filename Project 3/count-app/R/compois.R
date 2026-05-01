library(COMPoissonReg)

fit_compois_model <- function(df, response, predictors, formula_str = NULL) {
    if (length(predictors) == 0) {
        stop("Please choose at least one predictor")
    }

    model_data <- df |> tidyr::drop_na()

    fml_str <- if (!is.null(formula_str)) formula_str else
        paste(response, "~", paste(predictors, collapse = " + "))

    model_formula <- as.formula(fml_str)

    COMPoissonReg::glm.cmp(model_formula, data = model_data)
}


get_compois_irr_table <- function(model) {
    coef_est  <- coef(model)          # log-scale coefficients (count component)
    se_est    <- sdev(model)          # standard errors for count component
    n_count   <- length(coef_est)
    se_count  <- se_est[seq_len(n_count)]

    z_val     <- coef_est / se_count
    p_val     <- 2 * pnorm(abs(z_val), lower.tail = FALSE)

    data.frame(
        term           = names(coef_est),
        estimate       = round(coef_est, 4),
        rate_ratio     = round(exp(coef_est), 4),
        percent_change = round(100 * (exp(coef_est) - 1), 4),
        std.error      = round(se_count, 4),
        statistic      = round(z_val, 4),
        conf.low       = round(exp(coef_est - 1.96 * se_count), 4),
        conf.high      = round(exp(coef_est + 1.96 * se_count), 4),
        p.value        = round(p_val, 4),
        stringsAsFactors = FALSE
    )
}