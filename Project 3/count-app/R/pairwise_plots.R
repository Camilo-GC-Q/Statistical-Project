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
    if (length(predictors) == 0){
        stop("Please choose at least one predictor")
    }

    vars_needed = c(response, predictors)

    plot_data = df |>
        select(all_of(vars_needed)) |>
        drop_na()

    # Convert response to numeric if needed
    plot_data[[response]] = as.numeric(plot_data[[response]])

    suppressWarnings(suppressMessages(GGally::ggpairs(
        plot_data,
        columns = 1:ncol(plot_data),

        # lower: scatter with smoothing
        lower = list(continuous = function(data, mapping){
        ggplot(data, mapping) +
            geom_point(alpha = 0.4) +
            geom_smooth(
                method = function(formula, data, ...) {
                fit_custom_model(formula, data, type = model_type)
                },
                se = FALSE,
                na.rm = TRUE,
                show.legend = FALSE
            ) +
            theme_bw()
        }),

        # diagonal: histogram for counts
        diag = list(continuous = function(data, mapping){
            ggplot(data, mapping) +
            geom_histogram(bins = 20)+
            theme_bw()
        }),

        # upper: correlations
        upper = list(continuous = function(data, mapping){
            GGally::wrap("cor", size = 4)(data, mapping) +
            theme_bw() +
            theme(
                panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
            )
        })

        
    )))
}