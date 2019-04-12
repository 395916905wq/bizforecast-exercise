library(tidyverse)

dat <- read_csv('Data/cpl_and_conversion.csv') %>% as_tibble()


set.seed(1212)
# question 1 --------------------------------------------------------------

calc_profit <- function(x = 1212){
  L <- sample(3000:4000, 30, replace = T)
  R <- rnorm(n = 30, mean = 0.04, sd = 0.005)
  M <- sample(350:400, 30, replace = T)
  H <- 20000
  Cpl <- sample(8:10, 30, replace = T)
  Total_profit <- 0
  
  for (i in 1:30) {
    Income <- L[i] * R[i] * M[i]
    Expenses <- H + Cpl[i] * L[i]
    Profit <- Income - Expenses
    Total_profit <- Profit + Total_profit}
  
  return(Total_profit)
}

trials = 1000
result <- replicate(trials, calc_profit())
result <- result %>% as_tibble() %>% 
  mutate(profits = as.integer(value)) %>% 
  select(profits)

result %>% 
  ggplot(aes(profits))+
    geom_density()

quantile(result$profits,probs = c(0.025,0.975))


# question 2 -----------------------------------------------------------------

p_up_10w = nrow(result[which(result$profits >= 100000),])/1000
  


# Question 4 --------------------------------------------------------------



