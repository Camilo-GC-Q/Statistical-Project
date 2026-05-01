check_poisson_assumptions = function(model, df, response, predictors) {
  
  vars_needed = c(response, predictors[!grepl(":", predictors)])
  model_data  = df |> dplyr::select(dplyr::all_of(vars_needed)) |> tidyr::drop_na()
  y           = model_data[[response]]
  fitted_vals = fitted(model)
  n           = length(y)
  p           = length(predictors)
  
  # 1. Count response (non-negative integers)
  is_count = all(y == floor(y)) && all(y >= 0)
  count_result = list(
    flagged = !is_count,
    message = if (is_count) {
      "Response variable contains non-negative integer counts."
    } else {
      "Response variable does not appear to be non-negative integer counts. Poisson regression may be inappropriate."
    }
  )
  
  # 2. Mean-Variance equality (equidispersion)
  mean_y   = mean(y)
  var_y    = var(y)
  mv_ratio = var_y / mean_y
  mv_result = list(
    mean     = round(mean_y, 4),
    variance = round(var_y, 4),
    ratio    = round(mv_ratio, 4),
    flagged  = mv_ratio > 1.5,
    message  = if (mv_ratio > 1.5) {
      glue::glue(
        "Variance ({round(var_y,2)}) substantially exceeds mean ({round(mean_y,2)}), ",
        "ratio = {round(mv_ratio,2)}. Equidispersion assumption likely violated. ",
        "Consider Quasi-Poisson or Negative Binomial."
      )
    } else {
      glue::glue(
        "Mean ({round(mean_y,2)}) and variance ({round(var_y,2)}) are reasonably close ",
        "(ratio = {round(mv_ratio,2)}). Equidispersion assumption is satisfied."
      )
    }
  )
  
  # 3. Overdispersion: residual deviance / df
  resid_dev  = deviance(model)
  df_resid   = df.residual(model)
  disp_ratio = resid_dev / df_resid
  disp_result = list(
    ratio   = round(disp_ratio, 4),
    flagged = disp_ratio > 1.5,
    message = if (disp_ratio > 1.5) {
      glue::glue(
        "Overdispersion detected (deviance/df = {round(disp_ratio,4)}). ",
        "Consider Quasi-Poisson or Negative Binomial model."
      )
    } else {
      glue::glue("No strong overdispersion detected (deviance/df = {round(disp_ratio,4)}).")
    }
  )
  
  # 4. Excess zeros
  obs_zeros  = mean(y == 0)
  exp_lambda = mean(fitted_vals)
  exp_zeros  = exp(-exp_lambda)
  zero_ratio = obs_zeros / (exp_zeros + 1e-10)
  zero_result = list(
    obs_zeros = round(obs_zeros, 4),
    exp_zeros = round(exp_zeros, 4),
    ratio     = round(zero_ratio, 4),
    flagged   = zero_ratio > 2,
    message   = if (zero_ratio > 2) {
      glue::glue(
        "Excess zeros detected (observed: {round(100*obs_zeros,1)}%, ",
        "expected under Poisson: {round(100*exp_zeros,1)}%, ",
        "ratio = {round(zero_ratio,2)}). Consider ZIP or ZINB model."
      )
    } else {
      glue::glue(
        "No excess zeros detected (observed: {round(100*obs_zeros,1)}%, ",
        "expected: {round(100*exp_zeros,1)}%)."
      )
    }
  )
  
  # 5. Linearity of log(mean) in predictors
  #    Test via residuals vs fitted on log scale — use a LOESS deviation check
  log_fitted   = log(fitted_vals + 1e-10)
  pearson_r    = residuals(model, type = "pearson")
  loess_fit    = tryCatch(loess(pearson_r ~ log_fitted), error = function(e) NULL)
  
  if (!is.null(loess_fit)) {
    loess_pred   = predict(loess_fit)
    loess_dev    = sqrt(mean((loess_pred - mean(loess_pred))^2))
    linearity_flagged = loess_dev > 0.3
  } else {
    loess_dev         = NA
    linearity_flagged = FALSE
  }
  
  linearity_result = list(
    loess_dev = if (!is.na(loess_dev)) round(loess_dev, 4) else NA,
    flagged   = linearity_flagged,
    message   = if (linearity_flagged) {
      glue::glue(
        "Possible non-linearity in the log-mean detected (LOESS deviation = {round(loess_dev,3)}). ",
        "Inspect the Pearson residuals vs fitted plot. Consider adding polynomial or spline terms."
      )
    } else {
      "No strong evidence of non-linearity in the log-mean relationship."
    }
  )
  
  # 6. Multicollinearity — VIF (only meaningful with 2+ predictors)
  if (length(predictors) >= 2) {
    vif_vals = tryCatch(car::vif(model), error = function(e) NULL)
    if (!is.null(vif_vals)) {
      # car::vif returns a vector or matrix; extract numeric values
      vif_numeric = if (is.matrix(vif_vals)) vif_vals[, 3] else vif_vals
      max_vif     = max(vif_numeric)
      vif_flagged = max_vif > 5
      vif_df      = data.frame(
        term = names(vif_numeric),
        VIF  = round(vif_numeric, 3)
      )
    } else {
      max_vif     = NA
      vif_flagged = FALSE
      vif_df      = NULL
    }
    multi_result = list(
      vif_table = vif_df,
      max_vif   = if (!is.na(max_vif)) round(max_vif, 3) else NA,
      flagged   = vif_flagged,
      message   = if (is.na(max_vif)) {
        "VIF could not be computed."
      } else if (vif_flagged) {
        glue::glue(
          "High multicollinearity detected (max VIF = {round(max_vif,2)}). ",
          "Consider removing or combining correlated predictors."
        )
      } else {
        glue::glue(
          "No problematic multicollinearity detected (max VIF = {round(max_vif,2)})."
        )
      }
    )
  } else {
    multi_result = list(
      vif_table = NULL,
      max_vif   = NA,
      flagged   = FALSE,
      message   = "VIF not computed (only one predictor)."
    )
  }
  
  # 7. Events per predictor variable: sum(y) >= 10 * p
  total_events    = sum(y)
  epp             = total_events / p
  epp_flagged     = total_events < 10 * p
  epp_result = list(
    total_events = total_events,
    n_predictors = p,
    epp          = round(epp, 2),
    threshold    = 10 * p,
    flagged      = epp_flagged,
    message      = if (epp_flagged) {
      glue::glue(
        "Insufficient events per predictor: sum(y) = {total_events}, ",
        "p = {p}, threshold = 10 \u00d7 p = {10*p}. ",
        "Model may be unstable; consider reducing predictors or collecting more data."
      )
    } else {
      glue::glue(
        "Adequate events per predictor: sum(y) = {total_events} \u2265 10 \u00d7 p = {10*p}."
      )
    }
  )
  
  # 8. Goodness of fit: Pearson chi-squared
  pearson_chi = sum(residuals(model, type = "pearson")^2)
  gof_p       = pchisq(pearson_chi, df = df_resid, lower.tail = FALSE)
  gof_result  = list(
    chi_sq  = round(pearson_chi, 4),
    df      = df_resid,
    p_value = round(gof_p, 4),
    flagged = gof_p < 0.05,
    message = if (gof_p < 0.05) {
      glue::glue(
        "Poor model fit (Pearson \u03c7\u00b2 = {round(pearson_chi,2)}, ",
        "df = {df_resid}, p = {round(gof_p,4)}). Model may be misspecified."
      )
    } else {
      glue::glue(
        "Acceptable model fit (Pearson \u03c7\u00b2 = {round(pearson_chi,2)}, ",
        "df = {df_resid}, p = {round(gof_p,4)})."
      )
    }
  )
  
  list(
    count_response  = count_result,
    mean_variance   = mv_result,
    overdispersion  = disp_result,
    zero_inflation  = zero_result,
    linearity       = linearity_result,
    multicollinearity = multi_result,
    events_per_pred = epp_result,
    goodness_of_fit = gof_result
  )
}