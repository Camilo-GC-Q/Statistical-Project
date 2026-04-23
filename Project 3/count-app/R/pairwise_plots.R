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
    
    # 1. Ensure factors are treated as factors
    df <- df |> mutate(across(where(is.character), as.factor))
    
    # 2. Define standard theme
    my_theme <- theme_bw() + theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5))

    vars_needed = c(response, predictors)
    plot_data = df |> select(all_of(vars_needed)) |> drop_na()

    # Define custom GLM fitter
    model_func = function(formula, data, ...) {
        fit_custom_model(formula, data, type = model_type)
    }

    suppressWarnings(suppressMessages(GGally::ggpairs(
        plot_data,
        columns = 1:ncol(plot_data),

        # LOWER: Regression for continuous, Boxplot for combos
        lower = list(
            continuous = function(data, mapping){
                ggplot(data, mapping) + geom_point(alpha = 0.4) +
                    geom_smooth(method = model_func, se = FALSE) + my_theme
            },
            combo = function(data, mapping) {
                ggplot(data, mapping) + geom_boxplot() + my_theme
            },
            discrete = function(data, mapping) {
                ggplot(data, mapping) + geom_count() + my_theme
            }
        ),

        # DIAGONAL: Histograms
        diag = list(
            continuous = function(data, mapping){
                ggplot(data, mapping) + geom_histogram(bins = 20) + my_theme
            },
            discrete = function(data, mapping){
                ggplot(data, mapping) + geom_bar() + my_theme
            }
        ),

        # UPPER: Correlations (Continuous) + Blank/Labels for Categorical
        upper = list(
            continuous = function(data, mapping){
                GGally::wrap("cor", size = 4)(data, mapping) + my_theme
            },
            combo = function(data, mapping) {
                # Return empty plot for combo
                ggplot() + theme_void()
            },
            discrete = function(data, mapping) {
                # Return empty plot for discrete
                ggplot() + theme_void()
            }
        )
    )))
}