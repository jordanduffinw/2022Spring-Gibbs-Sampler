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
test_rho <- sample(1:20, N_dem_features)


### Actual function
calculateK <- function(Z_profiles, rho_t){
  
  # Divide each column of Z by its corresponding rho
  Z_profiles <- do.call(cbind, lapply(1:ncol(Z_profiles), function(i){Z_profiles[,i]/rho_t[i]}))
  
  # Calculate squared exponential kernel  
  rbf <- rbfdot(sigma=1)
  K <-kernelMatrix(kernel=rbf, as.matrix(Z_profiles))
  
  return(K)
}

# This should be an 8x8 matrix since the simulated data has 8 profiles
calculateK(test_Z, test_rho)
