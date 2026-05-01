library(pscl)
library(MASS)
library(patchwork)

compute_rqr = function(mod) {
    n       = length(fitted(mod))
    rqr     = rep(NA, n)
    counts  = as.numeric(mod$y)
    lambdas = as.numeric(fitted(mod))

    is_zip   = inherits(mod, "zeroinfl") && mod$dist == "poisson"
    is_zinb  = inherits(mod, "zeroinfl") && mod$dist == "negbin"
    is_nb    = inherits(mod, "negbin")
    is_pois  = inherits(mod, "glm") && family(mod)$family == "poisson"
    is_qpois = inherits(mod, "glm") && family(mod)$family == "quasipoisson"

    for (i in 1:n) {
        if (is_pois || is_qpois) {
            ai = ppois(counts[i] - 1, lambda = lambdas[i])
            bi = ppois(counts[i],     lambda = lambdas[i])

        } else if (is_nb) {
            theta = mod$theta
            ai = pnbinom(counts[i] - 1, mu = lambdas[i], size = theta)
            bi = pnbinom(counts[i],     mu = lambdas[i], size = theta)

        } else if (is_zip) {
            pi_zero = predict(mod, type = "zero")[i]
            ai = pi_zero + (1 - pi_zero) * ppois(counts[i] - 1, lambda = lambdas[i])
            bi = pi_zero + (1 - pi_zero) * ppois(counts[i],     lambda = lambdas[i])
            if (counts[i] == 0) { ai = 0; bi = pi_zero + (1 - pi_zero) * dpois(0, lambdas[i]) }

        } else if (is_zinb) {
            theta   = mod$theta
            pi_zero = predict(mod, type = "zero")[i]
            ai = pi_zero + (1 - pi_zero) * pnbinom(counts[i] - 1, mu = lambdas[i], size = theta)
            bi = pi_zero + (1 - pi_zero) * pnbinom(counts[i],     mu = lambdas[i], size = theta)
            if (counts[i] == 0) { ai = 0; bi = pi_zero + (1 - pi_zero) * dnbinom(0, mu = lambdas[i], size = theta) }
        }

        ui      = ai + runif(1) * (bi - ai)
        ui      = max(min(ui, 1 - 1e-6), 1e-6)
        rqr[i]  = qnorm(ui)
    }
    rqr
}

# ── Assumption checks ──────────────────────────────────────────────────────────
check_rqr_assumptions = function(mod, rqr) {
    counts  = as.numeric(mod$y)
    lambdas = as.numeric(fitted(mod))
    n       = length(counts)

    is_zip   = inherits(mod, "zeroinfl") && mod$dist == "poisson"
    is_zinb  = inherits(mod, "zeroinfl") && mod$dist == "negbin"
    is_nb    = inherits(mod, "negbin")
    is_pois  = inherits(mod, "glm") && family(mod)$family == "poisson"
    is_qpois = inherits(mod, "glm") && family(mod)$family == "quasipoisson"

    current = dplyr::case_when(
        is_pois  ~ "Poisson",
        is_qpois ~ "Quasipoisson",
        is_nb    ~ "Negative Binomial",
        is_zip   ~ "Zero-Inflated Poisson",
        is_zinb  ~ "Zero-Inflated Negative Binomial"
    )

    # 1. Normality of RQRs via Shapiro-Wilk (sample max 5000)
    sw_sample = if (n > 5000) sample(rqr, 5000) else rqr
    sw        = shapiro.test(sw_sample)
    normality_ok = sw$p.value > 0.05

    # 2. Dispersion ratio
    pearson_ratio = tryCatch({
        pr = residuals(mod, type = "pearson")
        df = if (!is.null(mod$df.residual)) mod$df.residual else n - length(coef(mod))
        sum(pr^2) / df
    }, error = function(e) NA)

    overdispersed  = !is.na(pearson_ratio) && pearson_ratio > 1.2
    underdispersed = !is.na(pearson_ratio) && pearson_ratio < 0.8

    # 3. Excess zeros
    obs_zeros  = mean(counts == 0)
    exp_zeros  = if (is_pois || is_qpois) mean(dpois(0, lambdas))
                 else if (is_nb) mean(dnbinom(0, mu = lambdas, size = mod$theta))
                 else mean(counts == 0)  # ZIP/ZINB already model zeros
    excess_zeros = obs_zeros > (exp_zeros + 0.05)

    # 4. Mean-variance: rough check via correlation of abs(rqr) and fitted
    mv_cor     = cor(abs(rqr), lambdas, method = "spearman")
    mv_concern = abs(mv_cor) > 0.3

    # ── Build findings ─────────────────────────────────────────────────────────
    findings = list()

    findings[["normality"]] = list(
        flagged = !normality_ok,
        message = if (normality_ok)
            paste0("RQRs appear normally distributed (Shapiro-Wilk p = ", round(sw$p.value, 3), ")")
        else
            paste0("RQRs deviate from normality (Shapiro-Wilk p = ", round(sw$p.value, 3),
                   ") — model may be misspecified")
    )

    findings[["dispersion"]] = list(
        flagged = overdispersed || underdispersed,
        message = if (!is.na(pearson_ratio)) {
            if (overdispersed)
                paste0("Overdispersion detected (ratio = ", round(pearson_ratio, 3), ")")
            else if (underdispersed)
                paste0("Underdispersion detected (ratio = ", round(pearson_ratio, 3), ")")
            else
                paste0("Dispersion looks adequate (ratio = ", round(pearson_ratio, 3), ")")
        } else "Dispersion ratio unavailable for this model class"
    )

    findings[["zeros"]] = list(
        flagged = excess_zeros,
        message = if (excess_zeros)
            paste0("Excess zeros detected (observed = ", round(obs_zeros, 3),
                   ", expected = ", round(exp_zeros, 3), ")")
        else
            paste0("Zero proportion looks adequate (observed = ", round(obs_zeros, 3),
                   ", expected = ", round(exp_zeros, 3), ")")
    )

    findings[["mean_variance"]] = list(
        flagged = mv_concern,
        message = if (mv_concern)
            paste0("Possible mean-variance misspecification (Spearman |r| = ", round(abs(mv_cor), 3), ")")
        else
            paste0("Mean-variance relationship looks adequate (Spearman |r| = ", round(abs(mv_cor), 3), ")")
    )

    # ── Recommendation ─────────────────────────────────────────────────────────
    recommendation = dplyr::case_when(
        # Already on best model
        is_zinb ~ "Model appears well-specified. ZINB is the most flexible option available.",

        # Excess zeros + overdispersion → ZINB
        excess_zeros && overdispersed && !is_zinb ~
            "Consider Zero-Inflated Negative Binomial (ZINB): excess zeros and overdispersion detected.",

        # Excess zeros only → ZIP
        excess_zeros && !overdispersed && !(is_zip || is_zinb) ~
            "Consider Zero-Inflated Poisson (ZIP): excess zeros detected without strong overdispersion.",

        # Overdispersion only → NB or Quasipoisson
        overdispersed && !excess_zeros && is_pois ~
            "Consider Negative Binomial or Quasipoisson: overdispersion detected.",

        overdispersed && !excess_zeros && is_qpois ~
            "Consider Negative Binomial: overdispersion detected and Quasipoisson already in use.",

        # Underdispersion
        underdispersed ~
            "Underdispersion detected. Consider checking for model misspecification or data issues.",

        # Normality failure only
        !normality_ok ~
            "RQRs are non-normal despite adequate dispersion — check for missing predictors or transformations.",

        # All good
        TRUE ~ paste0("Current model (", current, ") appears adequately specified.")
    )

    list(
        current        = current,
        findings       = findings,
        recommendation = recommendation,
        pearson_ratio  = pearson_ratio
    )
}

# ── Main plot function ─────────────────────────────────────────────────────────
plotRQR = function(mod) {
    if (inherits(mod, "glm") &&
        !is.null(mod$family) &&
        grepl("Tweedie|tweedie", mod$family$family, ignore.case = TRUE)) {

        sim <- tryCatch(
            DHARMa::simulateResiduals(mod, plot = FALSE, n = 500),
            error = function(e) NULL
        )

        if (is.null(sim)) {
            p <- ggplot() +
                annotate("text", x = 0.5, y = 0.5,
                         label = "DHARMa simulation failed for this Tweedie model.\nTry refitting with fewer predictors.",
                         size = 4, hjust = 0.5) +
                theme_bw()
        } else {
            # Extract residuals and fitted quantiles from DHARMa object
            res_df <- data.frame(
                fitted    = sim$fittedPredictedResponse,
                residuals = residuals(sim)   # uniformly distributed [0,1] under correct model
            )

            # QQ plot: uniform residuals should follow diagonal
            p1 <- ggplot(res_df, aes(sample = residuals)) +
                geom_abline(slope = 1, intercept = 0,
                            color = "red", linetype = "dashed") +
                stat_qq(distribution = qunif, color = "black", size = 0.8) +
                labs(title = "QQ Plot (Uniform)",
                     x = "Expected", y = "Observed") +
                theme_bw()

            # Residuals vs fitted
            p2 <- ggplot(res_df, aes(x = fitted, y = residuals)) +
                geom_point(shape = 1, alpha = 0.6) +
                geom_hline(yintercept = c(0.25, 0.5, 0.75),
                           linetype = "dashed", color = "red", alpha = 0.5) +
                geom_smooth(method = "loess", se = FALSE,
                            color = "black", linewidth = 0.8) +
                labs(title = "Residuals vs Fitted",
                     x = "Fitted Values",
                     y = "DHARMa Residuals") +
                theme_bw()

            p <- p1 | p2
        }

        # Build checks from DHARMa tests
        if (!is.null(sim)) {
            unif_test  <- DHARMa::testUniformity(sim, plot = FALSE)
            disp_test  <- DHARMa::testDispersion(sim, plot = FALSE)
            zero_test  <- tryCatch(
                DHARMa::testZeroInflation(sim, plot = FALSE),
                error = function(e) NULL
            )

            norm_find <- list(
                flagged = unif_test$p.value < 0.05,
                message = if (unif_test$p.value < 0.05)
                    paste0("Residuals deviate from uniformity (KS p = ",
                           round(unif_test$p.value, 4), ")")
                else
                    paste0("Residuals appear uniform (KS p = ",
                           round(unif_test$p.value, 4), ")")
            )

            disp_find <- list(
                flagged = disp_test$p.value < 0.05,
                message = if (disp_test$p.value < 0.05)
                    paste0("Dispersion issue detected (p = ",
                           round(disp_test$p.value, 4), ")")
                else
                    paste0("No dispersion issue detected (p = ",
                           round(disp_test$p.value, 4), ")")
            )

            zero_find <- if (!is.null(zero_test)) list(
                flagged = zero_test$p.value < 0.05,
                message = if (zero_test$p.value < 0.05)
                    paste0("Excess zeros detected (p = ",
                           round(zero_test$p.value, 4), ")")
                else
                    paste0("No excess zeros detected (p = ",
                           round(zero_test$p.value, 4), ")")
            ) else list(flagged = FALSE,
                        message = "Zero-inflation test not available.")

            mv_find <- list(
                flagged = FALSE,
                message = paste0("Tweedie power parameter p = ",
                                 round(mod$tweedie_power, 3),
                                 ". Mean-variance handled by Tweedie family.")
            )

            rec <- if (!norm_find$flagged && !disp_find$flagged)
                paste0("Tweedie model (p = ", round(mod$tweedie_power, 3),
                       ") appears adequately specified.")
            else
                "Consider adjusting the Tweedie power parameter or checking for model misspecification."

        } else {
            dummy    <- list(flagged = FALSE, message = "DHARMa simulation unavailable.")
            norm_find <- disp_find <- zero_find <- mv_find <- dummy
            rec       <- "DHARMa simulation failed — diagnostics unavailable."
        }

        return(list(
            plot   = p,
            checks = list(
                findings = list(
                    normality     = norm_find,
                    dispersion    = disp_find,
                    zeros         = zero_find,
                    mean_variance = mv_find
                ),
                recommendation = rec
            )
        ))
    }
    
    # CMP models can't use RQR infrastructure — return informative placeholder
    if (inherits(mod, "cmp")) {
        p <- ggplot() +
            annotate("text", x = 0.5, y = 0.5,
                     label = "RQR diagnostics are not available for COM-Poisson models.",
                     size = 5, hjust = 0.5) +
            theme_bw()
        # Return the expected list structure so rqr_checks_ui doesn't crash
        dummy_finding <- list(flagged = FALSE, message = "Not applicable for COM-Poisson.")
        return(list(
            plot = p,
            checks = list(
                findings = list(
                    normality     = dummy_finding,
                    dispersion    = dummy_finding,
                    zeros         = dummy_finding,
                    mean_variance = dummy_finding
                ),
                recommendation = "COM-Poisson handles both over- and underdispersion natively."
            )
        ))
    }

    lambdas = as.numeric(fitted(mod))
    counts  = as.numeric(mod$y)
    rqr     = compute_rqr(mod)
    checks  = check_rqr_assumptions(mod, rqr)

    pearson_ratio = checks$pearson_ratio
    title_str = if (!is.na(pearson_ratio))
        paste("Dispersion Ratio =", round(pearson_ratio, 4))
    else
        "Dispersion Ratio: unavailable"

    p1 = ggplot(tibble(lambda = lambdas, e = rqr)) +
        geom_hline(yintercept = 0, linetype = "dotted") +
        geom_point(aes(x = lambda, y = e), shape = 1) +
        theme_bw() +
        xlab(bquote(lambda)) +
        ylab("Randomized Quantile Residuals")

    p2 = ggplot(tibble(e = rqr)) +
        stat_qq(aes(sample = e)) +
        stat_qq_line(aes(sample = e)) +
        theme_bw() +
        xlab("Theoretical") +
        ylab("Observed") +
        ggtitle(title_str)

    r2_vals = tryCatch(
        as.numeric(residuals(mod, type = "pearson"))^2,
        error = function(e) (counts - lambdas)^2 / lambdas
    )

    p3 = ggplot(tibble(r2 = r2_vals, lambda = lambdas)) +
        geom_point(aes(x = lambda, y = r2), shape = 1) +
        geom_hline(yintercept = 1, linetype = "dotted", color = "red") +
        geom_smooth(aes(x = lambda, y = r2), color = "black") +
        theme_bw() +
        xlab(bquote(lambda)) +
        ylab(bquote(r^2))

    list(plot = (p1 | p2) / p3, checks = checks)
}