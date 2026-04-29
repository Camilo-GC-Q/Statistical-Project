plot_johnson_neyman = function(model, pred, modx){
    dat = model$model

    if(!is.numeric(dat[[pred]]) || !is.numeric(dat[[modx]])) {
        return(ggplot() +
            annotate("text", x = 0.5, y = 0.5,
                label = "Johnson-Neyman requires both interacting variables to be continuous.",
                size = 5, hjust = 0.5) +
            theme_bw())
    }

    jn = tryCatch(
    interactions::johnson_neyman(
        model = model, pred = pred, modx = modx,
        control.fdr = TRUE, plot = FALSE
    ),
    error = function(e) NULL
    )
    
    if(is.null(jn)){
        return(ggplot() +
            annotate("text", x = 0.5, y = 0.5,
                label = "Johnson-Neyman could not be computed. \nEnsure the model is fit with the interaction term included.",
                size = 5, hjust = 0.5) +
            theme_bw())
    }

    jn$plot +
        xlab(modx) +
        ylab(paste("Slope of", pred)) +
        ggtitle("Jonhson-Neyman Plot") +
        scale_color_brewer(paste("Slope of", pred), palette = "Pastel1") +
        scale_fill_brewer(paste("Slope of", pred), palette = "Pastel1") +
        theme_bw()
}