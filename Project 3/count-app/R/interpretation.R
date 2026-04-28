interpret_count_model = function(model, response, predictors) {
  model_class <- class(model)
  is_zeroinfl <- inherits(model, "zeroinfl")
  is_nb       <- inherits(model, "negbin")
  is_quasi    <- inherits(model, "glm") && 
                 !is.null(model$family) && 
                 model$family$family == "quasipoisson"
  is_poisson  <- inherits(model, "glm") && 
                 !is.null(model$family) && 
                 model$family$family == "poisson"
  
  # Extract coefficients 
  if (is_zeroinfl) {
    coef_df <- broom::tidy(model) %>%
      mutate(
        component = ifelse(grepl("^count_", term), "count", "zero"),
        term      = gsub("^count_|^zero_", "", term)
      )
    count_df <- coef_df %>% filter(component == "count")
    zero_df  <- coef_df %>% filter(component == "zero")
  } else {
    count_df <- broom::tidy(model)
    zero_df  <- NULL
  }
  
  # Model label
  model_label <- dplyr::case_when(
    is_zeroinfl && grepl("negbin", model$dist)  ~ "Zero-Inflated Negative Binomial",
    is_zeroinfl                                  ~ "Zero-Inflated Poisson",
    is_nb                                        ~ "Negative Binomial",
    is_quasi                                     ~ "Quasi-Poisson",
    is_poisson                                   ~ "Poisson",
    TRUE                                         ~ "Count"
  )
  
  # format p-value
  fmt_p <- function(p) {
    if (p < 0.001) "p<0.001" else paste0("p=", round(p, 4))
  }
  
  # significance phrase 
  sig_phrase <- function(p) {
    if (p < 0.05) "This effect is statistically discernible."
    else "This effect is not statistically discernible."
  }
  
  # Build count component sentences 
  count_sentences <- purrr::map_chr(
    seq_len(nrow(count_df)),
    function(i) {
      row      <- count_df[i, ]
      term     <- row$term
      est      <- row$estimate
      se       <- row$std.error
      p        <- row$p.value
      rr       <- exp(est)
      pct      <- round(100 * (rr - 1), 2)
      ci_low   <- round(exp(est - 1.96 * se), 4)
      ci_high  <- round(exp(est + 1.96 * se), 4)
      
      if (term == "(Intercept)") {
        base_count <- round(exp(est), 4)
        return(glue::glue(
          "When all continious variables are zero and all categorical variables are at their reference level, the expected {response} count is ",
          "{base_count} on average (95% CI: {ci_low}, {ci_high}). ({fmt_p(p)})"
        ))
      }
      
      # Detect if factor (has level suffix like "varname1" or "varnameLevel")
      matched_pred <- predictors[sapply(predictors, function(p) startsWith(term, p))]
      is_factor_term <- length(matched_pred) > 0 && term != matched_pred[1]
      
      if (is_factor_term) {
        pred    <- matched_pred[1]
        level   <- sub(paste0("^", pred), "", term)
        dir     <- if (rr > 1) "higher" else "lower"
        glue::glue(
          "The expected count of {response} is {round(rr, 4)} times {dir} ",
          "({abs(pct)}% {if(rr>1) 'higher' else 'lower'}) on average when {pred} = {level} ",
          "compared to the reference level accounting for simultanious change in other predictors ",
          "(95% CI: {ci_low}, {ci_high}; {fmt_p(p)}). ",
          "{sig_phrase(p)}"
        )
      } else {
        dir <- if (rr > 1) "increase" else "decrease"
        glue::glue(
          "For every one-unit increase in {term}, the expected count of {response} ",
          "is multiplied by {round(rr, 4)} on average — a {abs(pct)}% {dir} — ",
          "accounting for simultaneous change in other predictors ",
          "(95% CI: {ci_low}, {ci_high}; {fmt_p(p)}). ",
          "{sig_phrase(p)}"
        )
      }
    }
  )
  
  # ── Build zero component sentences (ZIP/ZINB only) ────────────────────────
  zero_sentences <- if (!is.null(zero_df)) {
    purrr::map_chr(
      seq_len(nrow(zero_df)),
      function(i) {
        row    <- zero_df[i, ]
        term   <- row$term
        est    <- row$estimate
        se     <- row$std.error
        p      <- row$p.value
        or     <- round(exp(est), 4)
        pct    <- round(100 * (or - 1), 2)
        ci_low  <- round(exp(est - 1.96 * se), 4)
        ci_high <- round(exp(est + 1.96 * se), 4)
        
        if (term == "(Intercept)") {
          return(glue::glue(
            "The baseline log-odds of being a structural zero is {round(est,4)} ",
            "(odds = {or}; {fmt_p(p)})."
          ))
        }
        
        dir <- if (or > 1) "increases" else "decreases"
        glue::glue(
          "A one-unit increase in {term} {dir} the odds of being a structural zero ",
          "by a factor of {or} ({abs(pct)}% {if(or>1) 'increase' else 'decrease'}), ",
          "holding others constant ",
          "(95% CI: {ci_low}, {ci_high}; {fmt_p(p)}). ",
          "{sig_phrase(p)}"
        )
      }
    )
  } else NULL
  
  list(
    model_label      = model_label,
    count_sentences  = count_sentences,
    zero_sentences   = zero_sentences,
    is_zeroinfl      = is_zeroinfl
  )
}