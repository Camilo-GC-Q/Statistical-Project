plot_johnson_neyman = function(model, pred, modx) {

    if (inherits(model, "cmp"))      return(plot_jn_cmp(model, pred, modx))
    if (inherits(model, "zeroinfl")) return(plot_jn_zeroinfl(model, pred, modx))

    dat = model$model

    if (!is.numeric(dat[[pred]]) || !is.numeric(dat[[modx]])) {
        return(ggplot() +
            annotate("text", x = 0.5, y = 0.5,
                label = "Johnson-Neyman requires both interacting variables to be continuous.",
                size = 5, hjust = 0.5) +
            theme_bw())
    }

    # interactions::johnson_neyman only supports glm/lm reliably
    # For negbin and quasipoisson fall back to manual calculation
    use_manual <- inherits(model, "negbin") ||
                  (inherits(model, "glm") && 
                   !is.null(model$family) && 
                   model$family$family == "quasipoisson")

    if (!use_manual) {
        jn = tryCatch(
            rlang::inject(
                interactions::johnson_neyman(
                    model = model,
                    pred  = !!rlang::sym(pred),
                    modx  = !!rlang::sym(modx),
                    control.fdr = FALSE,
                    plot  = FALSE
                )
            ),
            error = function(e) NULL
        )
    } else {
        jn = NULL
    }

    if (!is.null(jn)) {
        return(
            jn$plot +
                xlab(modx) +
                ylab(paste("Slope of", pred)) +
                ggtitle("Johnson-Neyman Plot") +
                theme_bw()
        )
    }

    # Manual JN for models interactions:: doesn't support
    cf  <- coef(model)
    vc  <- vcov(model)

    beta_pred <- pred
    beta_ixn  <- paste0(pred, ":", modx)
    if (!(beta_ixn %in% names(cf)))
        beta_ixn <- paste0(modx, ":", pred)

    if (!(beta_ixn %in% names(cf))) {
        return(ggplot() +
            annotate("text", x = 0.5, y = 0.5,
                label = "Johnson-Neyman could not be computed.\nEnsure the interaction term is included in the model.",
                size = 5, hjust = 0.5) +
            theme_bw())
    }

    if (beta_pred %in% names(cf)) {
        b1       <- cf[beta_pred]
        var_b1   <- vc[beta_pred, beta_pred]
        cov_b1b3 <- vc[beta_pred, beta_ixn]
    } else {
        b1       <- 0
        var_b1   <- 0
        cov_b1b3 <- 0
    }
    b3     <- cf[beta_ixn]
    var_b3 <- vc[beta_ixn, beta_ixn]

    modx_vals <- seq(min(dat[[modx]], na.rm = TRUE),
                     max(dat[[modx]], na.rm = TRUE),
                     length.out = 200)

    slope    <- b1 + b3 * modx_vals
    se_slope <- sqrt(var_b1 + modx_vals^2 * var_b3 + 2 * modx_vals * cov_b1b3)
    z_crit   <- qnorm(0.975)

    plot_df <- data.frame(
        modx  = modx_vals,
        slope = slope,
        lower = slope - z_crit * se_slope,
        upper = slope + z_crit * se_slope,
        sig   = ifelse(abs(slope / se_slope) > z_crit, "Significant", "Insignificant")
    )

    p = ggplot(plot_df, aes(x = modx, y = slope)) +
        geom_ribbon(aes(ymin = lower, ymax = upper, fill = sig), alpha = 0.3) +
        geom_line(aes(color = sig), linewidth = 1) +
        geom_hline(yintercept = 0, linewidth = 1) +
        scale_color_brewer(paste("Slope of", pred), palette = "Pastel1") +
        scale_fill_brewer(paste("Slope of", pred), palette = "Pastel1") +
        xlab(modx) +
        ylab(paste("Slope of", pred)) +
        ggtitle("Johnson-Neyman Plot") +
        theme_bw()

    return(p)
}

#helper function for COM-Poisson model
plot_jn_cmp = function(model, pred, modx) {
    dat <- tryCatch(eval(model$call$data), error = function(e) NULL)
    
    if (is.null(dat) || !is.numeric(dat[[pred]]) || !is.numeric(dat[[modx]])) {
        return(ggplot() +
            annotate("text", x = 0.5, y = 0.5,
                     label = "Johnson-Neyman requires continuous variables and accessible data.",
                     size = 5, hjust = 0.5) + theme_bw())
    }
    
    cf  <- coef(model)
    vc  <- vcov(model)
    
    # CMP coef names are plain (no "count_" prefix)
    beta_pred <- pred
    beta_ixn  <- paste0(pred, ":", modx)
    if (!(beta_ixn %in% names(cf)))
        beta_ixn <- paste0(modx, ":", pred)
    
    if (!(beta_pred %in% names(cf)) || !(beta_ixn %in% names(cf))) {
        return(ggplot() +
            annotate("text", x = 0.5, y = 0.5,
                     label = "Johnson-Neyman could not be computed.\nEnsure interaction term is in the model.",
                     size = 5, hjust = 0.5) + theme_bw())
    }
    
    b1 <- cf[beta_pred]; b3 <- cf[beta_ixn]
    var_b1 <- vc[beta_pred, beta_pred]
    var_b3 <- vc[beta_ixn,  beta_ixn]
    cov_b1b3 <- vc[beta_pred, beta_ixn]
    
    modx_vals <- seq(min(dat[[modx]], na.rm = TRUE),
                     max(dat[[modx]], na.rm = TRUE), length.out = 200)
    slope    <- b1 + b3 * modx_vals
    se_slope <- sqrt(var_b1 + modx_vals^2 * var_b3 + 2 * modx_vals * cov_b1b3)
    z_crit   <- qnorm(0.975)
    
    plot_df <- data.frame(
        modx  = modx_vals,
        slope = slope,
        lower = slope - z_crit * se_slope,
        upper = slope + z_crit * se_slope,
        sig   = ifelse(abs(slope / se_slope) > z_crit, "Significant", "Insignificant")
    )
    
    p = ggplot(plot_df, aes(x = modx, y = slope)) +
        geom_ribbon(aes(ymin = lower, ymax = upper, fill = sig), alpha = 0.3) +
        geom_line(aes(color = sig), linewidth = 1) +
        geom_hline(yintercept = 0, linewidth = 1) +
        scale_color_brewer(paste("Slope of", pred), palette = "Pastel1") +
        scale_fill_brewer(paste("Slope of", pred), palette = "Pastel1") +
        xlab(modx) + ylab(paste("Slope of", pred)) +
        ggtitle("Johnson-Neyman Plot") + theme_bw()

    return(p)
}

# helper function for zeroinflation models
plot_jn_zeroinfl = function(model, pred, modx) {
    dat = model$model

    if(!is.numeric(dat[[pred]]) || !is.numeric(dat[[modx]])){
        return(ggplot() +
            annotate("text", x = 0.5, y = 0.5,
                label = "Johnson-Neyman requires both interacting variables to be continuous.",
                size = 5, hjust = 0.5) +
            theme_bw())
    }

    cf <- coef(model)
    vc <- vcov(model)

    beta_pred <- paste0("count_", pred)
    beta_ixn  <- paste0("count_", pred, ":", modx)

    if (!(beta_ixn %in% names(cf)))
        beta_ixn <- paste0("count_", modx, ":", pred)

    if (!(beta_pred %in% names(cf)) || !(beta_ixn %in% names(cf))) {
        return(ggplot() +
            annotate("text", x = 0.5, y = 0.5,
                label = "Johnson-Neyman could not be computed.\nEnsure the model is fit with the interaction term included.",
                size = 5, hjust = 0.5) +
            theme_bw())
    }

    b1       <- cf[beta_pred]
    b3       <- cf[beta_ixn]
    var_b1   <- vc[beta_pred, beta_pred]
    var_b3   <- vc[beta_ixn,  beta_ixn]
    cov_b1b3 <- vc[beta_pred, beta_ixn]

    modx_vals <- seq(min(dat[[modx]], na.rm = TRUE),
                     max(dat[[modx]], na.rm = TRUE),
                     length.out = 200)

    slope    <- b1 + b3 * modx_vals
    se_slope <- sqrt(var_b1 + modx_vals^2 * var_b3 + 2 * modx_vals * cov_b1b3)
    z_crit   <- qnorm(0.975)

    plot_df <- data.frame(
        modx  = modx_vals,
        slope = slope,
        lower = slope - z_crit * se_slope,
        upper = slope + z_crit * se_slope,
        sig   = ifelse(abs(slope / se_slope) > z_crit, "Significant", "Insignificant")
    )

    p = ggplot(plot_df, aes(x = modx, y = slope)) +
        geom_ribbon(aes(ymin = lower, ymax = upper, fill = sig), alpha = 0.3) +
        geom_line(aes(color = sig), linewidth = 1) +
        geom_hline(yintercept = 0, linewidth = 1) +
        scale_color_brewer(paste("Slope of", pred), palette = "Pastel1") +
        scale_fill_brewer(paste("Slope of", pred), palette = "Pastel1") +
        xlab(modx) +
        ylab(paste("Slope of", pred)) +
        ggtitle("Johnson-Neyman Plot") +
        theme_bw()

    return(p)
}