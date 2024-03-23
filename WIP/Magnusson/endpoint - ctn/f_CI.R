f_CI<- function(vector, conf_level) {
  # Standard deviation of sample
  vec_sd <- sd(vector)
  # Sample size
  n <- length(vector)
  # Mean of sample
  vec_mean <- mean(vector)
  # Error according to t distribution
  error <- qt((conf_level + 1)/2, df = n - 1) * vec_sd / sqrt(n)
  # Confidence interval as a vector
  result <- c("mean" = vec_mean ,"lower" = vec_mean - error, "upper" = vec_mean + error)
  return(result)
}
