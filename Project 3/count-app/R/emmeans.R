library(emmeans)
library(magrittr)
library(tidyverse)

# Estimated Marginal Means

get_emmeans_table <- function(model, int.var, moderator, dat) {
  
  int.vars.classes <- sapply(dat[, c(int.var, moderator)], class)
  
  if (all(int.vars.classes == "numeric")) {
    m.mod <- mean(unlist(model$model[moderator]), na.rm = TRUE)
    s.mod <- sd(unlist(model$model[moderator]),   na.rm = TRUE)
    m.var <- mean(unlist(model$model[int.var]),   na.rm = TRUE)
    s.var <- sd(unlist(model$model[int.var]),     na.rm = TRUE)
    
    modvarat <- list(
      c(round(m.mod - s.mod, 2), round(m.mod + s.mod, 2)),
      c(round(m.var - s.var, 2), round(m.var + s.var, 2))
    )
    names(modvarat) <- c(moderator, int.var)
    
    mod.emmeans <- data.frame(
      emmeans(object = model, spec = c(int.var, moderator),
              var = int.var, at = modvarat, type = "response")
    )
    
    mod.emmeans[[int.var]] <- ifelse(
      mod.emmeans[[int.var]] == round(m.var - s.var, 2),
      paste0("Low (Mean - 1SD = ",  round(m.var - s.var, 2), ")"),
      paste0("High (Mean + 1SD = ", round(m.var + s.var, 2), ")")
    )
    mod.emmeans[[moderator]] <- ifelse(
      mod.emmeans[[moderator]] == round(m.mod - s.mod, 2),
      paste0("Low (Mean - 1SD = ",  round(m.mod - s.mod, 2), ")"),
      paste0("High (Mean + 1SD = ", round(m.mod + s.mod, 2), ")")
    )
    
  } else if (all(int.vars.classes == "factor")) {
    mod.emmeans <- data.frame(
      emmeans(object = model, spec = c(int.var, moderator),
              var = int.var, type = "response")
    )
    
  } else if (int.vars.classes[moderator] == "factor") {
    m.var <- mean(unlist(model$model[int.var]), na.rm = TRUE)
    s.var <- sd(unlist(model$model[int.var]),   na.rm = TRUE)
    modvarat <- list(c(round(m.var - s.var, 2), round(m.var + s.var, 2)))
    names(modvarat) <- int.var
    
    mod.emmeans <- data.frame(
      emmeans(model, specs = c(moderator, int.var),
              var = int.var, at = modvarat, type = "response")
    )
    
    mod.emmeans[[int.var]] <- ifelse(
      mod.emmeans[[int.var]] == round(m.var - s.var, 2),
      paste0("Low (Mean - 1SD = ",  round(m.var - s.var, 2), ")"),
      paste0("High (Mean + 1SD = ", round(m.var + s.var, 2), ")")
    )
    
  } else {
    m.mod <- mean(unlist(model$model[moderator]), na.rm = TRUE)
    s.mod <- sd(unlist(model$model[moderator]),   na.rm = TRUE)
    modvarat <- list(c(round(m.mod - s.mod, 2), round(m.mod + s.mod, 2)))
    names(modvarat) <- moderator
    
    mod.emmeans <- data.frame(
      emmeans(model, specs = c(moderator, int.var),
              var = moderator, at = modvarat, type = "response")
    )
    
    mod.emmeans[[moderator]] <- ifelse(
      mod.emmeans[[moderator]] == round(m.mod - s.mod, 2),
      paste0("Low (Mean - 1SD = ",  round(m.mod - s.mod, 2), ")"),
      paste0("High (Mean + 1SD = ", round(m.mod + s.mod, 2), ")")
    )
  }
  
  mod.emmeans |>
    set_rownames(NULL) |>
    set_colnames(c(moderator, int.var, "Estimated Marginal Mean", "SE", "df", "Lower CI", "Upper CI")) |>
    mutate_if(is.numeric, round, 4)
}

# Interpretation of EMM
interpret_emmeans <- function(emm_table, outcome, int.var, moderator) {
  
  bullets <- mapply(
    FUN = function(mod_level, var_level, emm, lower, upper) {
      sprintf(
        "The estimated marginal mean of %s for %s = %s and %s = %s is %.2f (95%% CI: %.2f, %.2f).",
        outcome,
        moderator, mod_level,
        int.var,   var_level,
        emm, lower, upper
      )
    },
    mod_level = emm_table[[moderator]],
    var_level = emm_table[[int.var]],
    emm       = emm_table[["Estimated Marginal Mean"]],
    lower     = emm_table[["Lower CI"]],
    upper     = emm_table[["Upper CI"]],
    SIMPLIFY  = TRUE
  )
  
  bullets
}


# Contrasts of Marginal Means
get_emmeans_contrasts <- function(model, int.var, moderator, dat) {
  
  int.vars.classes <- sapply(dat[, c(int.var, moderator)], class)
  
  build_contrast_table <- function(emm_obj) {
    p_obj = pairs(emm_obj)
    ct  <- data.frame(p_obj)
    cti <- data.frame(confint(p_obj))
    lower_col = names(cti)[grep("low|LCL", names(cti), ignore.case = TRUE)][1]
    upper_col = names(cti)[grep("upper|UCL", names(cti), ignore.case = TRUE)][1]
    
    if(!is.na(lower_col)) ct$lower.CL = cti[[lower_col]]
    if(!is.na(upper_col)) ct$upper.CL = cti[[upper_col]]
    
    ct$contrast <- gsub("\\.scaled", "", ct$contrast)
    ct |>
      dplyr::select(-any_of("null")) |>
      set_rownames(NULL) |>
      set_colnames(c("Contrast", "Estimate", "SE", "df", "t ratio", "p-value", "Lower CI", "Upper CI"))
  }
  
  if (all(int.vars.classes == "numeric")) {
    m.mod <- mean(unlist(model$model[moderator]), na.rm = TRUE)
    s.mod <- sd(unlist(model$model[moderator]),   na.rm = TRUE)
    m.var <- mean(unlist(model$model[int.var]),   na.rm = TRUE)
    s.var <- sd(unlist(model$model[int.var]),     na.rm = TRUE)
    
    modvarat <- list(
      c(round(m.mod - s.mod, 2), round(m.mod + s.mod, 2)),
      c(round(m.var - s.var, 2), round(m.var + s.var, 2))
    )
    names(modvarat) <- c(moderator, int.var)
    
    emm <- emmeans(object = model, spec = c(int.var, moderator),
                   var = int.var, at = modvarat, type = "response")
    emm <- add_grouping(emm, "int.var",  int.var,
                        c(paste0("(Low ", int.var, ")"), paste0("(High ", int.var, ")")))
    emm <- add_grouping(emm, "moderator", moderator,
                        c(paste0("(Low ", moderator, ")"), paste0("(High ", moderator, ")")))
    
    build_contrast_table(emmeans(emm, c("int.var", "moderator"), type = "response"))
    
  } else if (all(int.vars.classes == "factor")) {
    emm <- emmeans(model, specs = c(int.var, moderator), var = moderator, type = "response")
    build_contrast_table(emm)
    
  } else if (int.vars.classes[moderator] == "factor") {
    m <- mean(unlist(model$model[int.var]), na.rm = TRUE)
    s <- sd(unlist(model$model[int.var]),   na.rm = TRUE)
    modvarat <- list(c(round(m - s, 2), round(m + s, 2)))
    names(modvarat) <- int.var
    
    emm <- emmeans(model, specs = c(moderator, int.var),
                   var = moderator, at = modvarat, type = "response")
    emm <- add_grouping(emm, "int.var", int.var,
                        c(paste0("(Low ", int.var, ")"), paste0("(High ", int.var, ")")))
    build_contrast_table(emm)
    
  } else {
    m <- mean(unlist(model$model[moderator]), na.rm = TRUE)
    s <- sd(unlist(model$model[moderator]),   na.rm = TRUE)
    modvarat <- list(c(round(m - s, 2), round(m + s, 2)))
    names(modvarat) <- moderator
    
    emm <- emmeans(model, specs = c(moderator, int.var),
                   var = moderator, at = modvarat, type = "response")
    emm <- add_grouping(emm, "moderator", moderator,
                        c(paste0("(Low ", moderator, ")"), paste0("(High ", moderator, ")")))
    build_contrast_table(emm)
  }

}

# EMTrends

get_emtrends_table = function(model, int.var, moderator, dat) {

  int.vars.classes = sapply(dat[, c(int.var, moderator)], class)

  if (all(int.vars.classes == "numeric")) {
    m = mean(unlist(model$model[moderator]), na.rm = TRUE)
    s = sd(unlist(model$model[moderator]), na.rm = TRUE)
    modvarat = list(c(round(m - s, 2), round(m + s, 2)))
    names(modvarat) = moderator

    mod.emtrends = data.frame(emtrends(object = model, specs = moderator,
                                       var = int.var,
                                       at = modvarat))
    mod.emtrends[[moderator]] = ifelse(
      mod.emtrends[[moderator]] == round(m - s, 2),
      "Low (Mean - 1SD)", "High (Mean + 1SD)"
    )
    emtrend.test = test(emtrends(object = model, specs = moderator,
                                 var = int.var,
                                 at = modvarat))
  } else if (int.vars.classes[moderator] == "factor") {
    mod.emtrends = data.frame(emtrends(model, specs = moderator, var = int.var))
    emtrend.test = test(emtrends(model, specs = moderator, var = int.var))

  } else {
    return(NULL)
  }

  trend_col = grep("\\.trend$", names(mod.emtrends), value = TRUE)[1]
  ci_low    = grep("lower|LCL", names(mod.emtrends), ignore.case = TRUE, value = TRUE)[1]
  ci_high   = grep("upper|UCL", names(mod.emtrends), ignore.case = TRUE, value = TRUE)[1]

  names(mod.emtrends)[names(mod.emtrends) == trend_col] <- paste("Slope of", int.var)
  if (!is.na(ci_low))  names(mod.emtrends)[names(mod.emtrends) == ci_low]  <- "Lower CI"
  if (!is.na(ci_high)) names(mod.emtrends)[names(mod.emtrends) == ci_high] <- "Upper CI"
  dplyr::select(mod.emtrends, -any_of("null"))

  ratio_col = grep("t\\.ratio|z\\.ratio", names(emtrend.test), value = TRUE)[1]

  mod.emtrends = mod.emtrends |>
      mutate("t ratio" = emtrend.test[[ratio_col]],
             "p-value" = emtrend.test$p.value) |>
      relocate(c("t ratio", "p-value"), .after = df) |>
      mutate_if(is.numeric, round, 4)
  
  mod.emtrends
}

get_emtrends_contrasts <- function(model, int.var, moderator, dat) {

    int.vars.classes <- sapply(dat[, c(int.var, moderator)], class)

    build_emtrend_contrast <- function(emt_obj) {
        ct  <- data.frame(pairs(emt_obj))
        cti <- data.frame(confint(pairs(emt_obj)))
        lower_col <- names(cti)[grep("low|LCL", names(cti), ignore.case = TRUE)][1]
        upper_col <- names(cti)[grep("upper|UCL", names(cti), ignore.case = TRUE)][1]
        if (!is.na(lower_col)) ct$lower.CL <- cti[[lower_col]]
        if (!is.na(upper_col)) ct$upper.CL <- cti[[upper_col]]
        ct$contrast <- gsub("\\.scaled", "", ct$contrast)
        ct %>%
            dplyr::select(-any_of("null")) %>%
            set_rownames(NULL) %>%
            set_colnames(c("Contrast", "Estimate", "SE", "df", "t ratio", "p-value", "Lower CI", "Upper CI"))
    }

    if (all(int.vars.classes == "numeric")) {
        m.mod <- mean(unlist(model$model[moderator]), na.rm = TRUE)
        s.mod <- sd(unlist(model$model[moderator]),   na.rm = TRUE)
        m.var <- mean(unlist(model$model[int.var]),   na.rm = TRUE)
        s.var <- sd(unlist(model$model[int.var]),     na.rm = TRUE)
        modvarat <- list(c(round(m.mod - s.mod, 2), round(m.mod + s.mod, 2)),
                         c(round(m.var - s.var, 2), round(m.var + s.var, 2)))
        names(modvarat) <- c(moderator, int.var)

        emt <- emtrends(object = model, specs = moderator, var = int.var, at = modvarat)
        emt <- add_grouping(emt, "moderator", moderator,
                            c(paste0("(Low ", moderator, ")"), paste0("(High ", moderator, ")")))
        build_emtrend_contrast(emmeans(emt, spec = "moderator"))

    } else if (int.vars.classes[moderator] == "factor") {
        emt <- emtrends(model, specs = moderator, var = int.var)
        build_emtrend_contrast(emt)

    } else {
        return(NULL)
    }
}



# Interpret EMM Contrasts
  interpret_emmeans_contrasts <- function(contrast_table, outcome, alpha = 0.05) {
 
    format_p <- function(p) {
      if (is.na(p))     return("NA")
      if (p < 0.0001)   return("< 0.0001")
      if (p < 0.001)    return(paste0("= ", formatC(p, format = "f", digits = 4)))
      return(paste0("= ", formatC(p, format = "f", digits = 4)))
    }
 
    bullets <- mapply(
      FUN = function(contrast, estimate, se, df, t_ratio, p_value, lower, upper) {
 
        # Determine significance — handle "<0.0001" strings as well as numeric
        p_num <- suppressWarnings(as.numeric(as.character(p_value)))
        sig_label <- if (!is.na(p_num) && p_num < alpha) "significant" else "not significant"
 
        # Format p for display
        p_display <- if (!is.na(p_num)) {
          if (p_num < 0.0001) "< 0.0001" else formatC(p_num, format = "f", digits = 4)
        } else {
          as.character(p_value)   # already a string like "<0.0001"
        }
 
        sprintf(
          "The contrast of estimated marginal means of %s for (%s) is %s (t = %.2f, df = %g, p-value %s; 95%% CI: %.2f, %.2f).",
          outcome,
          contrast,
          sig_label,
          t_ratio,
          df,
          p_display,
          lower,
          upper
        )
      },
      contrast = contrast_table[["Contrast"]],
      estimate = contrast_table[["Estimate"]],
      se       = contrast_table[["SE"]],
      df       = contrast_table[["df"]],
      t_ratio  = contrast_table[["t ratio"]],
      p_value  = contrast_table[["p-value"]],
      lower    = contrast_table[["Lower CI"]],
      upper    = contrast_table[["Upper CI"]],
      SIMPLIFY = TRUE
    )
 
    bullets
}
# Interpret EMTrends and their Contrasts
interpret_emtrends <- function(emtrends_table, int.var, moderator, alpha = 0.05) {
    moderator_col <- names(emtrends_table)[1]
    slope_col     <- grep("Slope of", names(emtrends_table), value = TRUE)[1]

    bullets <- mapply(
        FUN = function(mod_level, slope, t_ratio, p_value) {
            p_num     <- suppressWarnings(as.numeric(as.character(p_value)))
            sig_label <- if (!is.na(p_num) && p_num < alpha) "significant" else "not significant"
            p_display <- if (!is.na(p_num)) {
                if (p_num < 0.0001) "< 0.0001" else formatC(p_num, format = "f", digits = 4)
            } else as.character(p_value)

            sprintf(
                "The effect of %s is %s when %s is %s (Slope = %.4f, t ratio = %.4f, p-value = %s).",
                int.var, sig_label, moderator, mod_level,
                slope, t_ratio, p_display
            )
        },
        mod_level = emtrends_table[[moderator_col]],
        slope     = emtrends_table[[slope_col]],
        t_ratio   = emtrends_table[["t ratio"]],
        p_value   = emtrends_table[["p-value"]],
        SIMPLIFY  = TRUE
    )
    bullets
}

interpret_emtrends_contrasts <- function(contrast_table, int.var, moderator, alpha = 0.05) {
    bullets <- mapply(
        FUN = function(contrast, estimate, t_ratio, p_value) {
            p_num     <- suppressWarnings(as.numeric(as.character(p_value)))
            sig_label <- if (!is.na(p_num) && p_num < alpha) "significantly different" else "not significantly different"
            p_display <- if (!is.na(p_num)) {
                if (p_num < 0.0001) "< 0.0001" else formatC(p_num, format = "f", digits = 4)
            } else as.character(p_value)

            sprintf(
                "The effect of %s is %s by %s (Contrast = %s, Estimate = %.4f, t ratio = %.4f, p-value = %s).",
                int.var, sig_label, moderator,
                contrast, estimate, t_ratio, p_display
            )
        },
        contrast = contrast_table[["Contrast"]],
        estimate = contrast_table[["Estimate"]],
        t_ratio  = contrast_table[["t ratio"]],
        p_value  = contrast_table[["p-value"]],
        SIMPLIFY = TRUE
    )
    bullets
}

