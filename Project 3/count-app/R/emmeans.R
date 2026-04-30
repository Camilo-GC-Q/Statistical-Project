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
  
  mod.emmeans %>%
    set_rownames(NULL) %>%
    set_colnames(c(moderator, int.var, "Estimated Marginal Mean", "SE", "df", "Lower CI", "Upper CI")) %>%
    mutate_if(is.numeric, round, 4)
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
