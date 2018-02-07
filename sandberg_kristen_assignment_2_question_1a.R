
# source('sandberg_assignment_2_MLE_finder_function.R')
x <- c(1.77, -0.23,  2.76, 3.80,  3.47, 56.75,  -1.34,  4.24, -2.44, 
       3.29,  3.71, -2.40, 4.53, -0.07, -1.05, -13.87, -2.53, -1.75)

starting_point <- c(-11, -1, 0, 1.5, 4, 4.7, 7, 8, 38, mean(x))
tolerance_err <- 0.000001

converges_to <- array()

l_prime_theta_t <- function(theta_t){ 
  -2 * sum((theta_t - x)/(1 + (theta_t - x)^2))
}
l_double_prime_theta_t <- function(theta_t){
  -2 * sum((1 - (theta_t - x)^2)/(1 + (theta_t - x)^2)^2)
}

for (k in 1:length(starting_point)){
  
  theta_t<-starting_point[k]
  
  converges_to[k] <- find_MLE(theta_t, l_prime_theta_t, l_double_prime_theta_t,tolerance_err,maximum_iterations = 100)
  
}  

# Create a table of the theta starting points and the values they converge to
MLE_table <- cbind(starting_point,converges_to)
  
print(MLE_table)

# Plot the probability density of the Cauchy distribution where theta is the sample mean
set_theta <- MLE_table[10,2]

all_x <-seq(-50,50,0.25)
prob_density <- array()

for (m in 1:length(all_x)){
  prob_density <- 1/(pi * (1 + (all_x - set_theta)^2))
}

plot(all_x,prob_density,type="l")

# Plot the log-likelihood function
n_value <- length(x)
all_theta <-seq(-100,100,1)
l_theta_t <- array()

for (j in 1:length(all_theta)){
  l_theta_t[j] <- -n_value * log(pi) - sum(log(1+(all_theta[j] - x)^2))
}

plot(all_theta,l_theta_t,type="l")