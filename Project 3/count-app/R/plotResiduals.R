plotResiduals = function(mod){
    require(tidyverse)
    require(patchwork)
    e = tryCatch(
        residuals(mod, type = "deviance"),
        error = function(err) residuals(mod, type = "response")
    )

    e_sd = sd(e)

    ggdat = data.frame(y.hat = fitted(mod), e = e) |>
        mutate(id = 1:n())

    ggdat.gaussian = data.frame(
        x = seq(min(e) - 0.25 * diff(range(e)),
                max(e) + 0.25 * diff(range(e)),
                length.out = 1000)
    ) |>
        mutate(f = dnorm(x, mean = 0, sd = e_sd))

    default.bins = round(log2(nrow(ggdat) + 1))

    p1 = ggplot(ggdat, aes(x = e)) +
        geom_histogram(aes(y = after_stat(density)),
                       bins = default.bins,
                       fill = "lightgrey", color = "black") +
        geom_hline(yintercept = 0) +
        stat_density(aes(x = e, color = "Empirical"),
                     geom = "line", position = "identity") +
        geom_line(data = ggdat.gaussian, aes(x = x, y = f, color = "Gaussian Reference")) +
        theme_bw() +
        xlab("Deviance Residual") +
        ylab("Density") +
        labs(color = "") +
        theme(legend.position = "bottom")

    p2 = ggplot(data = ggdat, aes(sample = e)) +
        geom_qq() +
        geom_qq_line() +
        theme_bw() +
        xlab("Gaussian Quantiles") +
        ylab("Sample Quantiles")
        
    ggdat.out3 = ggdat |>
        filter(abs(e) > 3 * e_sd)

    p3 = ggplot(data = ggdat, aes(x = id, y = e)) +
        geom_point(shape = 1) +
        geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
        geom_hline(yintercept = c(-3, 3) * e_sd,
                   color = "red", linetype = "dotted", linewidth = 0.75) +
        geom_point(data = ggdat.out3, aes(x = id, y = e), fill = "red", shape = 21) +
        xlab("Observation Number") +
        ylab("Deviance Residual") +
        theme_bw()

    p4 = ggplot(data = ggdat, aes(x = y.hat, y = e)) +
        geom_point(shape = 1) +
        geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
        geom_hline(yintercept = c(-3, 3) * e_sd,
                   color = "red", linetype = "dotted", linewidth = 0.75) +
        geom_point(data = ggdat.out3, aes(x = y.hat, y = e), fill = "red", shape = 21) +
        xlab(bquote("Fitted Values" ~ (hat(Y)))) +
        ylab("Deviance Residual") +
        theme_bw()

    (p1 | p2) / (p3 | p4) +
        plot_layout(guides = "collect") &
        theme(legend.position = "bottom")
}
