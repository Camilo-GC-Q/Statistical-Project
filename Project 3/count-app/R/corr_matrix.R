get_coeff_correlation_matrix = function(model) {
  
  vcov_mat <- tryCatch(
    vcov(model),
    error = function(e) {
      stop("Could not extract variance-covariance matrix from this model type.")
    }
  )
  
  # Convert to plain matrix in case it's a special matrix class
  vcov_mat <- as.matrix(vcov_mat)
  
  # Remove the Log(theta) dispersion row/col that ZINB adds — it breaks cov2cor
  bad_names <- grepl("Log\\(theta\\)", rownames(vcov_mat))
  if (any(bad_names)) {
    vcov_mat <- vcov_mat[!bad_names, !bad_names, drop = FALSE]
  }
  
  # Drop rows/cols with any NA, NaN, or non-finite values
  bad_idx <- apply(vcov_mat, 1, function(x) any(!is.finite(x)))
  vcov_mat <- vcov_mat[!bad_idx, !bad_idx, drop = FALSE]
  
  # Drop rows/cols with zero or negative diagonal (causes cov2cor to fail)
  bad_diag <- diag(vcov_mat) <= 0
  vcov_mat <- vcov_mat[!bad_diag, !bad_diag, drop = FALSE]
  
  if (nrow(vcov_mat) < 2) {
    return(data.frame(Message = "Insufficient coefficients to compute correlation matrix."))
  }
  
  cor_mat <- tryCatch(
    cov2cor(vcov_mat),
    error = function(e) {
      return(data.frame(Message = "Correlation matrix could not be computed for this model."))
    }
  )
  
  # Clean up rownames for zeroinfl models
  if (inherits(model, "zeroinfl")) {
    rownames(cor_mat) <- gsub("^count_", "Count: ", rownames(cor_mat))
    rownames(cor_mat) <- gsub("^zero_",  "Zero: ",  rownames(cor_mat))
    colnames(cor_mat) <- rownames(cor_mat)
  }
  
  round(as.data.frame(cor_mat), 4)
}