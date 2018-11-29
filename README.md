# ReadingGroup

## Draft for the algorithm - to be completed 

#######################################################################

# Fixed point algorithm RGM

#######################################################################

# Setup with two countries and two destinations:

# Guess the initial set of parameters (c_s, c_w, sigma):

library(Gumbel)

# Set the seed for replication:
set.seed(123)

# Declare parameters to be estimated
cs= rnorm(2, mean = 0, sd = 1)
cw= rnorm(2, mean = 0, sd = 1)

sigma = rGumbel(1, location = 0, scale = 1)

# Other parameters:
d= rnorm(2, mean = 0, sd = 1)
beta = 0.995
y_euler = 0.5772156649

# Set initial value v0
f <- function(cs, cw, d){

N = 10000

# Declare the ships value fucntion matrix
v <- matrix(0, 2, 2, N)

v[1][1][0] = V_10
v[2][2][0] = V_20

U <- matrix(0, 2,N)

for (l in 1/N) {
  
  for (i in 1/2){
    
    for (j in 1/2) {
  
  if (i != j){
  # Step 1:

  v[i][j][l] = (-cs[i] + d[i] * v[j][j][l]) / (1 - beta (1 -d[i]))

  # Step 2: 

  u[i][l] = sigma * log(exp(beta * v[i][i] / sigma) + (exp(v[i][j])/sigma) ) + sigma * y_euler

  #Step 3:

  v[i][i][l+1] = -cw[i] + lambda[i]* E[j] * tau[i][j] + lambda[i] * Pe[i][j] / (1 - Pe[i][0]) * v[i][j][l] + (1-lambda[i] * U[i][l])
  
  }

    if (v[i][i][l+1] -v[i][i][l] < 0.0001) {
  
      "Stop"
    }
  
  # Compute probabilities Pij

  }}}

}

# Calcul Pij selon formule 5
optim_nm(f, k=3, start = c(runif(4)), maximum = FALSE, trace = FALSE, alpha = 1, beta = 2, tol = 0.0001)
