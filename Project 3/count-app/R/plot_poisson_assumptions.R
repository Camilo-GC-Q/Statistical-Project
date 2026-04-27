plot_poisson_assumptions = function(model, df, response, predictors) {
  
  vars_needed <- c(response, predictors)
  model_data  <- df |> dplyr::select(dplyr::all_of(vars_needed)) |> tidyr::drop_na()
  y           <- model_data[[response]]
  fitted_vals <- fitted(model)
  my_theme    <- ggplot2::theme_bw()

  # Observed vs fitted
  p1 <- ggplot2::ggplot(
    data.frame(observed = y, fitted = fitted_vals),
    ggplot2::aes(x = fitted, y = observed)
  ) +
    ggplot2::geom_point(alpha = 0.5, color = "#2c7bb6") +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
    ggplot2::labs(title = "Observed vs Fitted",
                  x = "Fitted Values", y = "Observed Counts") +
    my_theme

  # Pearson residuals vs fitted
  pearson_resid <- residuals(model, type = "pearson")
  p2 <- ggplot2::ggplot(
    data.frame(fitted = fitted_vals, residuals = pearson_resid),
    ggplot2::aes(x = fitted, y = residuals)
  ) +
    ggplot2::geom_point(alpha = 0.5, color = "#d7191c") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::geom_hline(yintercept = c(-2, 2), linetype = "dotted", color = "orange") +
    ggplot2::labs(title = "Pearson Residuals vs Fitted",
                  x = "Fitted Values", y = "Pearson Residuals") +
    my_theme

  # Observed count distribution vs theoretical Poisson
  lambda_hat  <- mean(y)
  count_range <- 0:max(y)
  poisson_df  <- data.frame(
    count = count_range,
    prob  = dpois(count_range, lambda = lambda_hat)
  )
  obs_df <- as.data.frame(table(y)) |>
    dplyr::mutate(y = as.numeric(as.character(y)), Freq = Freq / sum(Freq))

  p3 <- ggplot2::ggplot() +
    ggplot2::geom_col(data = obs_df,
                      ggplot2::aes(x = y, y = Freq),
                      fill = "#abdda4", color = "white") +
    ggplot2::geom_line(data = poisson_df,
                       ggplot2::aes(x = count, y = prob),
                       color = "#d7191c", linewidth = 1) +
    ggplot2::geom_point(data = poisson_df,
                        ggplot2::aes(x = count, y = prob),
                        color = "#d7191c", size = 2) +
    ggplot2::labs(title = "Observed Counts vs Poisson Distribution",
                  x = "Count", y = "Proportion") +
    my_theme

  # Cook's distance
  cooks_d <- cooks.distance(model)
  p4 <- ggplot2::ggplot(
    data.frame(obs = seq_along(cooks_d), cooks = cooks_d),
    ggplot2::aes(x = obs, y = cooks)
  ) +
    ggplot2::geom_bar(stat = "identity", fill = "#fdae61") +
    ggplot2::geom_hline(yintercept = 4 / length(cooks_d),
                        linetype = "dashed", color = "red") +
    ggplot2::labs(title = "Cook's Distance",
                  x = "Observation", y = "Cook's Distance") +
    my_theme

  gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2)
}