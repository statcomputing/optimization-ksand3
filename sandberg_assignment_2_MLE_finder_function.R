# Kristen Sandberg
# Assignment 2

# Function to find the MLE given the starting theta, l', l'', and a tolerance

find_MLE <- function(starting_theta,l_prime,l_double_prime,tolerance_epsilon, maximum_iterations = 1000){
  
  # Keep count of iterations so the while loop does not take too long
  count_iterations <- 1
  # Initialize the difference to be greater than the tolerance so the while loop works
  theta_difference <- tolerance_epsilon + 2
  # Begin with starting_theta
  theta_t_value <- starting_theta
  
  while ((theta_difference >= tolerance_epsilon) & count_iterations < maximum_iterations){

        theta_t_next <- theta_t_value - (l_prime(theta_t_value) / l_double_prime(theta_t_value))
    theta_difference <- abs(theta_t_next - theta_t_value)
    theta_t_value <- theta_t_next
    count_iterations <- count_iterations + 1
  }
  
  theta_t_value
  
}