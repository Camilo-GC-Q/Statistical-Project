fit_custom_model <- function(formula, data, type = "poisson") {
    switch(type,
        "poisson" = glm(formula, data = data, family = poisson),
        "quasipoisson" = glm(formula, data = data, family = quasipoisson),
        "negbin" = MASS::glm.nb(formula, data = data),
        "zip" = pscl::zeroinfl(formula, data = data, dist = "poisson"),
        "zinb" = pscl::zeroinfl(formula, data = data, dist = "negbin")
    )
}

plot_count_pairs = function(df, response, predictors, model_type = "poisson"){
  
  df <- df |> mutate(across(where(is.character), as.factor))
  
  my_theme <- theme_bw() + 
    theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5))
  
  vars_needed <- c(response, predictors[!grepl(":", predictors)])
  plot_data   <- df |> dplyr::select(all_of(vars_needed)) |> drop_na()
  
  # Identify which columns are continuous vs discrete
  is_continuous <- sapply(plot_data, function(x) is.numeric(x) && !is.factor(x))
  
  model_func = function(formula, data, ...) {
    tryCatch(
      fit_custom_model(formula, data, type = model_type),
      error = function(e) lm(formula, data = data)  # fallback to lm on failure
    )
  }
  
  suppressWarnings(suppressMessages(GGally::ggpairs(
    plot_data,
    columns = 1:ncol(plot_data),
    
    lower = list(
      # Continuous-continuous: scatter + GLM smooth
      continuous = function(data, mapping) {
        ggplot(data, mapping) +
          geom_point(alpha = 0.4) +
          geom_smooth(method = model_func, se = FALSE) +
          my_theme
      },
      # Continuous-categorical: boxplot
      combo = function(data, mapping) {
        ggplot(data, mapping) +
          geom_boxplot() +
          my_theme
      },
      # Categorical-categorical: tile count
      discrete = function(data, mapping) {
        ggplot(data, mapping) +
        geom_bar(aes(fill = after_stat(count)), position = "dodge") +
        scale_fill_gradient(low = "lightblue", high = "steelblue") +
        my_theme +
        theme(legend.position = "none")
        }
    ),
    
    diag = list(
      # Continuous diagonal: histogram
      continuous = function(data, mapping) {
        ggplot(data, mapping) +
          geom_histogram(bins = 20, fill = "black", color = "white") +
          my_theme
      },
      # Categorical diagonal: bar chart
      discrete = function(data, mapping) {
        ggplot(data, mapping) +
          geom_bar(fill = "black", color = "white") +
          my_theme
      }
    ),
    
    upper = list(
      # Continuous-continuous: correlation
      continuous = function(data, mapping) {
        GGally::wrap("cor", size = 4)(data, mapping) + my_theme
      },
      # Continuous-categorical: mean ± SE dots
      combo = function(data, mapping) {
        ggplot(data, mapping) +
          geom_boxplot() +
          my_theme
      },
      # Categorical-categorical: blank
      discrete = function(data, mapping) {
        ggplot() + theme_void()
      }
    )
  )))
}