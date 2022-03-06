rnormCTG <- function(n, k = 12) { replicate(n, sum(runif(k)) - (k/2)) }

ks_tests <- function(m, n)
{
  p_values_rnorm = NULL
  p_values_rnormCTG = NULL
  p_values_rnormCTG6 = NULL
  
  for (i in 1:m)
  {
    X <- rnorm(n)
    test_results <- ks.test(X, pnorm)
    p_values_rnorm <- append(p_values_rnorm, test_results$p.value)
    
    Y <- rnormCTG(n)
    test_results <- ks.test(Y, pnorm)
    p_values_rnormCTG <- append(p_values_rnormCTG, test_results$p.value)
    
    Z <- rnormCTG(n, 6)
    test_results <- ks.test(Z, pnorm)
    p_values_rnormCTG6 <- append(p_values_rnormCTG6, test_results$p.value)
  }
  
  title_string = sprintf("dla m = %d, n = %d", m, n)
  
  hist(p_values_rnorm, prob = TRUE,
       main = paste("p-wartosci rnorm ", title_string))
  hist(p_values_rnormCTG, prob = TRUE,
       main = paste("p-wartosci przyblizenia z CTG (k=12) ",
                    title_string))
  hist(p_values_rnormCTG6, prob = TRUE,
       main = paste("p-wartosci przyblizenia z CTG (k=6) ",
                    title_string))
  
  ks.test(p_values_rnorm, punif)
  ks.test(p_values_rnormCTG, punif)
  ks.test(p_values_rnormCTG6, punif)
}
par(mfrow = c(3,2))

ks_tests(1000, 10)
ks_tests(10000, 100)






