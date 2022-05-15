### Inputs
# Use the sumulated data to create some fake inputs for rho and Z
df <- read.csv("df_group.csv")

# User input "J" in the main wrapper function
N_survey_items <- 4

# Each row is a profile
N_dem_profiles <- nrow(df)

# Columns before the survey items represent the demographic features that make up each profile
# i.e., race, gender, age
N_dem_features <- (ncol(df)-1)-N_survey_items


# Define Z as just the demographic part of the data
test_Z <- df[ , 1:N_dem_features]
# In practice this vector is a sample from mvrnorm, drawn in the previous step
test_rho <- invgamma::rinvgamma(N_dem_features, 4)
test_rho <- runif(N_dem_features, min=0, max=3)

### Actual function
calculateKfun <- function(Z_profiles, rho_t){
  
  # Divide each column of Z by its corresponding rho
  Z_profiles <- do.call(cbind, lapply(1:ncol(Z_profiles), function(i){Z_profiles[,i]/rho_t[i]}))
  
  # Calculate squared exponential kernel  
  rbf <- rbfdot(sigma=0.5)
  K <-kernelMatrix(kernel=rbf, as.matrix(Z_profiles))
  
  return(K)
}

# This should be an 8x8 matrix since the simulated data has 8 profiles
calculateKfun(test_Z, test_rho)


calculateParitalpifun <- function(Z, rho, a, b){
  
  # Calculate squared exponential kernel  
  K_rho <- calculateKfun(Z, rho)
  
  # Get the third row
  ## Get f
  N <- nrow(Z)
  f <- MASS::mvrnorm(1, rep(0, N), K_rho)
  thirdrow_part1 <- det(K_rho)^(-0.5)
  thirdrow_part2 <- exp(-0.5*(t(f) %*% solve(K_rho) %*% f))[1,1]
  
  # Get the fifth row, which is the density of an inverse gamma distribution
  fifthrow <- exp(sum(log(invgamma::dinvgamma(rho, shape = a, rate = b))))
  
  # Get the density
  Partialpi <- thirdrow_part1 * thirdrow_part2 * fifthrow
  
  return(Partialpi)
}

calculateParitalpifun(test_Z, test_rho, 2, 1)

step5fun <- function(Z, rho_lag, Sigma_rho, a, b){
  
  #5.1 Get lrho
  D <- length(rho_lag)
  lrho <- MASS::mvrnorm(1, mu = rho_lag, Sigma = Sigma_rho)
  
  #5.2 Get the kernel
  K_rho_lag <- calculateKfun(Z, rho_lag)
  K_lrho <- calculateKfun(Z, lrho)
  
  pi_exp_lrho <- calculateParitalpifun(Z, lrho, a, b)
  pi_exp_rho_lag <- calculateParitalpifun(Z, rho_lag, a, b)
  
  r <- pi_exp_lrho / pi_exp_rho_lag
  rho <- r^(exp(lrho)) * (1-r)^(rho_lag)
  return(rho)
}

step5fun(test_Z, test_rho, diag(0.0001,3,3), 2, 1)
