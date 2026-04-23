get_coeff_correlation_matrix = function(model) {
    # Extract the variance-covariance matrix
    vcov_mat <- vcov(model)
    
    # Convert to correlation matrix
    cor_mat <- cov2cor(vcov_mat)
    
    # Round to 4 decimal places for readability
    cor_mat <- round(cor_mat, 4)
    
    # Return as a data frame
    return(as.data.frame(cor_mat))
}