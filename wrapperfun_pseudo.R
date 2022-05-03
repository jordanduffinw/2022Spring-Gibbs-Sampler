# Runs through each of the following:
source('step1_pseudo.R')
source('step2_pseudo.R')
source('step3_pseudo.R')

# Read in simulated data; test tibbles and data.frames
# df_group <- as_tibble(read.csv("df_group.csv"))
df_group <- read.csv("df_group.csv")

# Number 4 needs to be user input, replace with number of response items
# Define counts of affirimative respondents (matrix y)
y <- df_group[ , (ncol(df_group)-4):(ncol(df_group)-1)]
# and total respondents per demographic profile
n <- as.data.frame(df_group[ , ncol(df_group)])

# Store number of demographic groups and response items for convenience
groups <- nrow(y)
items <- ncol(y) # Will be a user input in the real function

# Create a fake X matrix; revisit when we figure out how to do this
X <- matrix(c(c(1,2,3,4,5,6,7,8), rep(-1, 8)), byrow = FALSE,nrow = 8,ncol = 2)

# Use step 1 to output omega matrix
omega_mat <- step1fun(y, n, items)

# Calculate kappa for steps 2 and 3
kappa <- data.matrix(do.call(cbind,apply(y,2,function(x){(x-n)/2})))

# Initiate empty beta tilde matrix with 2 columns and rows the length number of response items
beta_tilde_test <- matrix(nrow = items, ncol = 2)

# Use step 2 to sample a beta_tilde for each response item
for(j in 1:items){
  beta_tilde_test[j,] <- step2fun(X, omega_mat[,j], kappa[,j])
}

# Save as dataframe for informative names
beta_tilde_test <- data.frame(beta_tilde_test)

# Assign names to components
colnames(beta_tilde_test) <- c("beta", "alpha")
rownames(beta_tilde_test) <- paste0("item_",1:items)

# Use kappa, omega, and beta_tilde to calculate y_tilde
# This divides, elementwise, kappa_{ij}/omega_{ij}. Then, to each item in the jth column, adds the jth item in the alpha component of beta_tilde
y_tilde <- do.call(cbind, lapply(1:4,function(j){(kappa * (1/omega_mat))[,j] + beta_tilde_test$alpha[j]}))

# Create a fake f_prior vector; revisit when we figure out how to do this
f_prior <- sample(1:80, groups)


#### NOTE, sampled thetas currently output as length 2 vectors for each demographic profile, is this correct?

# Initiate empty theta matrix with 2 columns and rows the length number of demographic profiles
theta_test <- matrix(nrow = groups, ncol = 2)

# Use step 3 to sample a theta_i for each demographic profile
for(i in 1:groups){
  theta_test[i,] <- step3fun(beta_tilde_test, f_prior[i], y_tilde[i,])
}

dim(theta_test)
