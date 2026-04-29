library(pscl)
library(MASS)
library(patchwork)

compute_rqr = function(mod) {
    n = length(fitted(mod))
    rqr = rep(NA, n)

    # Detect model type
    is_zip  = inherits(mod, "zeroinfl") && mod$dist == "poisson"
    is_zinb = inherits(mod, "zeroinfl") && mod$dist == "negbin"
    is_nb   = inherits(mod, "negbin")
    is_pois = inherits(mod, "glm") && family(mod)$family == "poisson"
    is_qpois = inherits(mod, "glm") && family(mod)$family == "quasipoisson"

    counts  = as.numeric(mod$y)
    lambdas = as.numeric(fitted(mod))

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

        ui = ai + runif(1) * (bi - ai)
        ui = max(min(ui, 1 - 1e-6), 1e-6)
        rqr[i] = qnorm(ui)
    }
    rqr
}

plotRQR = function(mod) {
    lambdas = as.numeric(fitted(mod))
    counts  = as.numeric(mod$y)
    rqr     = compute_rqr(mod)

    # Dispersion ratio — use pearson residuals where available, fallback for zeroinfl
    pearson_ratio = tryCatch({
        pr = residuals(mod, type = "pearson")
        df = if (!is.null(mod$df.residual)) mod$df.residual else length(counts) - length(coef(mod))
        sum(pr^2) / df
    }, error = function(e) NA)

    title_str = if (!is.na(pearson_ratio)) {
        paste("Dispersion Ratio =", round(pearson_ratio, 4))
    } else {
        "Dispersion Ratio: unavailable"
    }

    # Plot 1: RQR vs fitted lambda
    p1 = ggplot(tibble(lambda = lambdas, e = rqr)) +
        geom_hline(yintercept = 0, linetype = "dotted") +
        geom_point(aes(x = lambda, y = e), shape = 1) +
        theme_bw() +
        xlab(bquote(lambda)) +
        ylab("Randomized Quantile Residuals")

    # Plot 2: QQ plot with dispersion ratio
    p2 = ggplot(tibble(e = rqr)) +
        stat_qq(aes(sample = e)) +
        stat_qq_line(aes(sample = e)) +
        theme_bw() +
        xlab("Theoretical") +
        ylab("Observed") +
        ggtitle(title_str)

    # Plot 3: Squared Pearson residuals vs lambda
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

    (p1 | p2) / p3
}