######################  FORWARD PROPAGATION  #########################

col1 <- c(0.7, -0.3)  # Values for the first column
col2 <- c(0.8, -0.3)  # Values for the second column
col3 <- c(0.55, -1.1)  # Values for the third colum
w2<- cbind(col1, col2, col3)  # Combine columns to create a matrix

a1 <- matrix(c(1, 2, -1.5), nrow = 3, ncol = 1)

z2 <- w2%*%a1
print(z2)


# Define the sigmoid activation function
sigmoid <- function(x) {
  1 / (1 + exp(-x))
}

# Apply the sigmoid function to the matrix
a2 <- sigmoid(z2)

# Print the result
print(a2)


col1 <- c(0.5, 0.5)  # Values for the first column
col2 <- c(0.3, 0.6)  # Values for the second column
w3 <- cbind(col1, col2)  # Combine columns to create a matrix

z3 <- w3%*%a2
print(z3)


################## MSE ######################

t1 <- 1.3
t2 <- 2

R <- 1/2 * ((z3[1, 1] - t1)^2 + (z3[2, 1] - t2)^2)
R


################## Back propagation ##################

t1 <- 1.3
t2 <- 2

t <- matrix(c(t1, t2), nrow = 2)

delta3 <- z3-t
delta3


AA=t(w3)%*%delta3
AA

BB=a2*(1-a2)
BB

delta2=AA*BB
delta2

delta2[1]*a1[3]

sum(delta3)*BB[1]*a1[3]*0.5



############## functions ###########

relu <- function(z) {
  return(ifelse(z > 0, z, 0))
}

relu_derivative <- function(z) {
  return(ifelse(z > 0, 1, 0))
}

input <- c(-2,-1,0,1,2)

relu_derivative(input)



tanh <- function(z) {
  return((exp(z) - exp(-z)) / (exp(z) + exp(-z)))
}

tanh_derivative <- function(z) {
  tanh_value <- tanh(z)
  return(1 - tanh_value^2)
}



softmax <- function(z) {
  exp_z <- exp(z)
  return(exp_z / sum(exp_z))
}

softmax_derivative <- function(z) {
  s <- softmax(z)
  return(s * (1 - s))
}




leaky_relu <- function(z, alpha) {
  return(ifelse(z > 0, z, alpha * z))
}

















