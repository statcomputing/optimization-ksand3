starting_points <- c(-11, -1, 0, 1.5, 4, 4.7, 7, 8, 38)
x <- -1

alpha_value <- c(1, 0.64, 0.25)

for (k in 1:length(starting_points)){
  
  theta_t<-starting_points[k]
  
  for (i in 1:10){
    
    print(x)
    
    l_prime_theta_t <- -2 * sum((theta_t - x)/(1 + (theta_t - x)^2))
    G_x <- alpha_value[1] * l_prime_theta_t + theta_t
    
    x <- G_x
  }
  
  converges_to[k] <- x
  
}  

# Create a table of the theta starting points and the values they converge to
MLE_fpi_table <- cbind(starting_points,converges_to)

print(MLE_fpi_table)