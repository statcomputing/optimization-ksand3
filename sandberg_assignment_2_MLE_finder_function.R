# Kristen Sandberg
# Assignment 2

# Function to find the MLE given the starting theta, l', l'', and a tolerance

find_MLE <- function(starting_theta,l_prime,l_double_prime,tolerance_epsilon){
  
  # Keep count of iterations so the while loop does not take too long
  count_iterations <- 1
  # Initialize the difference to be greater than the tolerance so the while loop works
  theta_difference <- tolerance_epsilon + 2
  # Begin with starting_theta
  theta_t_value <- starting_theta
  
  while ((theta_difference >= tolerance_epsilon) & count_iterations < 1000){
    
    theta_t_next <- theta_t_value - (l_prime(theta_t_value) / l_double_prime(theta_t_value))
    theta_difference <- abs(theta_t_next - theta_t_value)
    theta_t_value <- theta_t_next
    count_iterations <- count_iterations + 1
  }
  
  theta_t_value
  
}

# Question 1
# x <- c(1.77, -0.23,  2.76, 3.80,  3.47, 56.75,  -1.34,  4.24, -2.44,
#        3.29,  3.71, -2.40, 4.53, -0.07, -1.05, -13.87, -2.53, -1.75)
# 
# l_prime_theta_t <- function(theta_t){
#   -2 * sum((theta_t - x)/(1 + (theta_t - x)^2))
# }
# l_double_prime_theta_t <- function(theta_t){
#   -2 * sum((1 - (theta_t - x)^2)/(1 + (theta_t - x)^2)^2)
# }
# 
# begin_theta <- 3.257778
# 
# converged_to_theta <- find_MLE(begin_theta, l_prime_theta_t, l_double_prime_theta_t,0.000001)
# 
# answer_message <- paste("The answer is: ",converged_to_theta)
# 
# print(answer_message)