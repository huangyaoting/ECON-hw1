library(data.table)
library(MASS)

# Set parameters
mu0 <- 5
mu1 <- 6
sigma0 <- 1
sigma1 <- 2
sigma01 <- 1.5
C <- 1

# Simulate esp0 and esp1 for N individuals
N <- 10000000
set.seed(123)
correlation <- sigma01 / (sigma0 * sigma1) # Calculate correlation coefficient
covariance <- correlation * sigma0 * sigma1 # Calculate covariance
Sigma <- matrix(c(sigma0^2, covariance, covariance, sigma1^2), ncol = 2) # Create covariance matrix
eps <- mvrnorm(n = N, mu = c(0, 0), Sigma = Sigma) # Generate correlated eps
eps0 <- eps[,1]
eps1 <- eps[,2]

# Create data.table
DT <- data.table(esp0 = eps0, esp1 = eps1)

# Calculate w0 and w1
DT[, w0 := mu0 + esp0]
DT[, w1 := mu1 + esp1]

# Generate I based on w0, w1, and C
DT[, I := ifelse(w1 > w0 + C, 1, 0)]

# Calculate means of w0 and w1 conditioned on I
E_w0_I0 <- DT[I == 0, mean(w0)]
E_w0_I1 <- DT[I == 1, mean(w0)]
E_w1_I0 <- DT[I == 0, mean(w1)]
E_w1_I1 <- DT[I == 1, mean(w1)]
E_Q0_I0 <- DT[I == 0, mean(eps0)]
E_Q0_I1 <- DT[I == 1, mean(eps0)]
E_Q1_I0 <- DT[I == 0, mean(eps1)]
E_Q1_I1 <- DT[I == 1, mean(eps1)]

# Calculate RHS of equations (1) and (2)
v <- eps1-eps0
sigmav <- sd(v)
z <- (mu0-mu1+C)/sigmav

DT[, RHS1 := mu0 + sigma0*sigma1/(sigmav)*( correlation-sigma0/sigma1)*(dnorm(z)/(1-pnorm(z)))]
DT[, RHS2 := mu1 + sigma0*sigma1/(sigmav)*(-correlation+sigma1/sigma0)*(dnorm(z)/(1-pnorm(z)))]
RHS1_I0 <- DT[I == 0, mean(RHS1)]
RHS1_I1 <- DT[I == 1, mean(RHS2)]
RHS2_I0 <- DT[I == 0, mean(RHS1)]
RHS2_I0 <- DT[I == 1, mean(RHS2)]

# Print results
cat("E_w0_I0:", E_w0_I0, "\n")
cat("E_w0_I1:", E_w0_I1, "\n")
cat("E_w1_I0:", E_w1_I0, "\n")
cat("E_w1_I1:", E_w1_I1, "\n")

cat("RHS1_I0:", RHS1_I0, "\n")
cat("RHS1_I1:", RHS1_I1, "\n")
cat("RHS2_I0:", RHS2_I0, "\n")
cat("RHS2_I1:", RHS2_I1, "\n")

cat("E_Q0_I0:", E_Q0_I0, "\n")
cat("E_Q0_I1:", E_Q0_I1, "\n")
cat("E_Q1_I0:", E_Q1_I0, "\n")
cat("E_Q1_I1:", E_Q1_I1, "\n")