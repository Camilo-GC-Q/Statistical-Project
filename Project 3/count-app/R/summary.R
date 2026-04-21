get_count_summary = function(df, response){
    y <- df[[response]]
    y <- y[!is.na(y)]
  
    list(
        mean = mean(y),
        variance = var(y),
        min = min(y),
        max = max(y),
        zero_prop = mean(y == 0)
    )
}