fit_nb_model <- function(formula, data, offset = NULL) {
  
  if (is.null(offset)) {
    # No offset
    model <- MASS::glm.nb(formula, data = data)
  } else {
    # Add offset to the formula
    formula_with_offset <- update(formula, . ~ . + offset(offset))
    model <- MASS::glm.nb(formula_with_offset, data = data)
  }
  
  return(model)
}