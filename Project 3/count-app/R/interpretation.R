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
  
  # Detect offset variable from formula
  offset_var <- tryCatch({
    tt      <- terms(model)
    off_idx <- attr(tt, "offset")
    if (is.null(off_idx)) NULL else {
      vars     <- as.character(attr(tt, "variables"))[-1]
      off_term <- vars[off_idx]
      gsub("offset\\(log\\(\\s*|\\s*\\)\\)", "", off_term)
    }
  }, error = function(e) NULL)
  has_offset <- !is.null(offset_var)

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
  
  # Format p-value
  fmt_p <- function(p) {
    if (p < 0.001) "p<0.001" else paste0("p=", round(p, 4))
  }
  
  # Significance phrase 
  sig_phrase <- function(p) {
    if (p < 0.05) "This effect is statistically discernible."
    else "This effect is not statistically discernible."
  }

  # Phrase used throughout — "rate per unit of X" vs "count"
  outcome_phrase <- if (has_offset) {
    paste0(response, " rate per unit of ", offset_var)
  } else {
    paste0("count of ", response)
  }

  # Model data frame for type checking
  mdat <- tryCatch(model$model, error = function(e) NULL)

  # Helper: is a term an interaction (contains ":")
  is_ixn <- function(term) grepl(":", term)

  # Helper: resolve variable type from model frame
  var_type <- function(varname) {
    if (!is.null(mdat) && varname %in% names(mdat) && is.numeric(mdat[[varname]])) {
      "numeric"
    } else {
      "categorical"
    }
  }
  
  # Build count component sentences — main effects only
  main_df <- count_df %>% filter(!is_ixn(term))

  count_sentences <- purrr::map_chr(
    seq_len(nrow(main_df)),
    function(i) {
      row      <- main_df[i, ]
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
        if (has_offset) {
          return(glue::glue(
            "When all continuous variables are zero and all categorical variables are at their reference level, ",
            "the expected {response} rate per unit of {offset_var} is {base_count} on average ",
            "(95% CI: {ci_low}, {ci_high}). ({fmt_p(p)})"
          ))
        } else {
          return(glue::glue(
            "When all continuous variables are zero and all categorical variables are at their reference level, ",
            "the expected {outcome_phrase} is {base_count} on average ",
            "(95% CI: {ci_low}, {ci_high}). ({fmt_p(p)})"
          ))
        }
      }
      
      # Detect if factor (has level suffix like "varname1" or "varnameLevel")
      matched_pred   <- predictors[sapply(predictors, function(p) startsWith(term, p))]
      is_factor_term <- length(matched_pred) > 0 && term != matched_pred[1]
      
      if (is_factor_term) {
        pred  <- matched_pred[1]
        level <- sub(paste0("^", pred), "", term)
        dir   <- if (rr > 1) "higher" else "lower"
        glue::glue(
          "The expected {outcome_phrase} is {round(rr, 4)} times {dir} ",
          "({abs(pct)}% {if(rr>1) 'higher' else 'lower'}) on average when {pred} = {level} ",
          "compared to the reference level, accounting for simultaneous change in other predictors ",
          "(95% CI: {ci_low}, {ci_high}; {fmt_p(p)}). ",
          "{sig_phrase(p)}"
        )
      } else {
        dir <- if (rr > 1) "increase" else "decrease"
        glue::glue(
          "For every one-unit increase in {term}, the expected {outcome_phrase} ",
          "is multiplied by {round(rr, 4)} on average — a {abs(pct)}% {dir} — ",
          "accounting for simultaneous change in other predictors ",
          "(95% CI: {ci_low}, {ci_high}; {fmt_p(p)}). ",
          "{sig_phrase(p)}"
        )
      }
    }
  )
  
  # Interaction term sentences
  ixn_df <- count_df %>% filter(is_ixn(term))

  if (nrow(ixn_df) > 0) {
    ixn_sentences <- purrr::map_chr(
      seq_len(nrow(ixn_df)),
      function(i) {
        row     <- ixn_df[i, ]
        term    <- row$term
        est     <- row$estimate
        se      <- row$std.error
        p       <- row$p.value
        rr      <- exp(est)
        pct     <- round(abs(100 * (rr - 1)), 2)
        ci_low  <- round(exp(est - 1.96 * se), 4)
        ci_high <- round(exp(est + 1.96 * se), 4)
        dir     <- if (rr > 1) "increases" else "decreases"

        raw_vars <- strsplit(term, ":")[[1]]

        match_pred <- function(raw) {
          m <- predictors[sapply(predictors, function(p) startsWith(raw, p))]
          if (length(m) == 0) raw else m[1]
        }

        var_a  <- match_pred(raw_vars[1])
        var_b  <- match_pred(raw_vars[2])
        type_a <- var_type(var_a)
        type_b <- var_type(var_b)

        if (type_a == "numeric" && type_b == "numeric") {
          glue::glue(
            "The interaction between {var_a} and {var_b} ",
            "(IRR = {round(rr, 4)}; 95% CI: {ci_low}, {ci_high}; {fmt_p(p)}): ",
            "for each additional one-unit increase in {var_a}, the multiplicative effect of a ",
            "one-unit increase in {var_b} on the expected {outcome_phrase} ",
            "{dir} by {pct}% on average, accounting for simultaneous change in other predictors ",
            "(and vice versa). {sig_phrase(p)}"
          )
        } else if (type_a == "categorical" && type_b == "categorical") {
          level_a <- sub(paste0("^", var_a), "", raw_vars[1])
          level_b <- sub(paste0("^", var_b), "", raw_vars[2])
          glue::glue(
            "The interaction between {var_a} (level: {level_a}) and {var_b} (level: {level_b}) ",
            "(IRR = {round(rr, 4)}; 95% CI: {ci_low}, {ci_high}; {fmt_p(p)}): ",
            "the expected {outcome_phrase} {dir} by an additional {pct}% ",
            "for this combination of levels beyond their individual main effects. ",
            "{sig_phrase(p)}"
          )
        } else {
          num_var <- if (type_a == "numeric") var_a else var_b
          cat_var <- if (type_a == "categorical") var_a else var_b
          cat_raw <- if (type_a == "categorical") raw_vars[1] else raw_vars[2]
          level   <- sub(paste0("^", cat_var), "", cat_raw)
          glue::glue(
            "The interaction between {cat_var} (level: {level}) and {num_var} ",
            "(IRR = {round(rr, 4)}; 95% CI: {ci_low}, {ci_high}; {fmt_p(p)}): ",
            "the effect of a one-unit increase in {num_var} on the expected {outcome_phrase} ",
            "{dir} by {pct}% when {cat_var} = {level} compared to its reference level. ",
            "{sig_phrase(p)}"
          )
        }
      }
    )

    count_sentences <- c(count_sentences, ixn_sentences)
  }
  
  # Zero component sentences (ZIP/ZINB only)
  zero_sentences <- if (!is.null(zero_df)) {
    purrr::map_chr(
      seq_len(nrow(zero_df)),
      function(i) {
        row     <- zero_df[i, ]
        term    <- row$term
        est     <- row$estimate
        se      <- row$std.error
        p       <- row$p.value
        or      <- round(exp(est), 4)
        pct     <- round(100 * (or - 1), 2)
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