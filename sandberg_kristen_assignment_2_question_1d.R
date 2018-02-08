# Question 1d

# library(ggplot2)
x_q1d <- c(1.77, -0.23,  2.76, 3.80,  3.47, 56.75,  -1.34,  4.24, -2.44, 
       3.29,  3.71, -2.40, 4.53, -0.07, -1.05, -13.87, -2.53, -1.75)

starting_point <- c(-11, -1, 0, 1.5, 4, 4.7, 7, 8, 38, mean(x_q1d))
tolerance_err <- 0.000001

# Based on Solution to Question 1 part a
fisher_information <- length(x_q1d)/2

converges_to <- array()

for (k in 1:length(starting_point)){
  
  theta_t<-starting_point[k]
  
  theta_difference <- tolerance_err + 2
  counter <- 1
  
  # for (i in 1:1000){
  while ((theta_difference >= tolerance_err) & (counter < 10000)){
    
    l_prime_theta_t <- -2 * sum((theta_t - x_q1d)/(1 + (theta_t - x_q1d)^2))
    
    new_theta_t <- theta_t + (l_prime_theta_t / fisher_information)
    theta_difference <- abs(new_theta_t - theta_t)
    
    theta_t <- new_theta_t
    counter <- counter + 1
    
  }
  print(counter)
  converges_to[k] <- theta_t
  
}  

# Create a table of the theta starting points and the values they converge to
q1d_MLE_table <- cbind(starting_point,converges_to)

print(q1d_MLE_table)

# Find the mode of the points that the iterations converge to
calculate_mode <- function(vector_input) {
  uniqv_element <- unique(vector_input)
  uniqv_element[which.max(tabulate(match(vector_input, uniqv_element)))]
}
mode_is <- calculate_mode(MLE_table[,2])
print(mode_is)
print(-n_value * log(pi) - sum(log(1+(mode_is - x_q1d)^2)))
# print(-n_value * log(pi) - sum(log(1+(3.0213454 - x)^2)))
# print(-n_value * log(pi) - sum(log(1+(-0.5914735 - x)^2)))

# Plot the log-likelihood function
n_value <- length(x_q1d)
all_theta <-seq(-1,6,0.00025)
l_theta_t <- array()

for (j in 1:length(all_theta)){
  l_theta_t[j] <- -n_value * log(pi) - sum(log(1+(all_theta[j] - x_q1d)^2))
}

plot(all_theta,l_theta_t,type="l")
# print(max(l_theta_t))
# ggplot(aes(x=all_theta,y=l_theta_t))