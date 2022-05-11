# Simulate Data
library(tidyverse)
library(MASS) # mvrnorm
library(kernlab)
rm(list=ls())
set.seed(501)


# number of individuals
n=1000

# Create binary Demographic Profiles
x1<- rbinom(n=n,size = 1, prob=.5)
x2<- rbinom(n=n,size = 1, prob=.75)
x3<- rbinom(n=n,size = 1, prob=.15)

# If you want a trichotomous/categorical variable
# x3<- sample(x=c(1:3),size=n, replace=T) 

df_individual<-data.frame(x1,x2,x3)




# Compute the Squared Exponential Kernel
rbf <- rbfdot(sigma=1)
K<-kernelMatrix(kernel=rbf, as.matrix(df_individual))



#  Take one draw from multivariate normal distribution
y_star1<-mvrnorm(n=1, mu=rep(0,n), Sigma = K)
y_star2<-mvrnorm(n=1, mu=rep(0,n), Sigma = K)
y_star3<-mvrnorm(n=1, mu=rep(0,n), Sigma = K)
y_star4<-mvrnorm(n=1, mu=rep(0,n), Sigma = K)

j1<-rbinom(n,size=1,prob=plogis(y_star1))
df_individual$j1<-j1
j2<-rbinom(n,size=1,prob=plogis(y_star2))
df_individual$j2<-j2
j3<-rbinom(n,size=1,prob=plogis(y_star3))
df_individual$j3<-j3
j4<-rbinom(n,size=1,prob=plogis(y_star4))
df_individual$j4<-j4



# Individual Level Dataset
df_individual


# Group-Level Dataset 
df_individual %>%
  group_by(x1,x2,x3) %>%
  summarise(j1 = sum(j1),j2 = sum(j2),j3 = sum(j3),j4 = sum(j4),
            total=n())->df_group

df_group

# write_csv(df_group, file = "df_group.csv")

step1fun <- function(y, n, J_items){
  
  # Store number of unique demographic profiles and response items for convenience.
  N_groups <- nrow(y)
  
  # Define a matrix pi in which each column of y will be divided by the n_i
  # corresponding to the appropriate demographic profile--this is the observed probability of y, pi.
  pi <- data.matrix(do.call(cbind, apply(y, 2, function(x){x/n})))
  
  
  # Later on, apply qlogis() (logit) to each pi_{ij} given that pi is defined as the inverse logit of mu.
  # Cannot do it in this step because inputs are columns.
  
  # Rename columns and rows appropriately
  colnames(pi) <- paste0("item_",1:J_items)
  rownames(pi) <- paste0("group_",1:N_groups)
  
  # Initialize empty matrix omega with appropriate dimensions
  w <- matrix(NA, nrow = N_groups, ncol = J_items)
  
  # Matrix values imputed elementwise in a loop for now for transparency, this must be optimized.
  
  # For each unique demographic profile i in 1...N
  for(i in 1:N_groups){
    
    # For each response item j in 1...J
    for(j in 1:J_items){
      
      ### mu_{ij}} calculated here
      # Sample omega_{ij} from the Polya-Gamma distribution using the appropriate n_{ij} and mu_{ij}=qlogis(pi_{ij})
      
      ## When pi[i,j] == 0, qlogis(pi[i,j]) = -Inf, and
      ## rpg() cannot have negative z = Inf as an input; But as z gets arbitrarily negative, the output of rpg() converges to 0. 
      if (pi[i,j] == 0) {
        w[i,j] <- 0 
      } else {
        w[i,j] <- BayesLogit::rpg(1, as.numeric(n[i,]), qlogis(pi[i,j]))
      }
    }
  }
  
  return(w)
}

y <- df_group[,4:7]
n <- df_group[8]
J <- ncol(y)
w <- step1fun(y=y,n=n,J_items=J)










### Test step 2

step2fun <- function(X, w_j, k_j, Lambda=0.1){
  
  # Store number of demographic profiles for convenience
  N <- length(w_j)
  
  # Turn jth col vector of omega into diagonal NxN matrix
  Omega_j <- diag(w_j, nrow = N, ncol = N)
  
  # Apply definition of Lambda
  Lambda <- diag(0.1, nrow = 2, ncol = 2)
  
  # Applying the definition of V_beta
  V_beta <- solve(Lambda + (t(X) %*% Omega_j %*% X))
  
  # Applying the definition of m_beta
  m_beta <- V_beta %*% (t(X) %*% k_j)
  
  # Attempt at sampling beta from multivariate normal
  beta_tilde_j <- MASS::mvrnorm(1, mu = m_beta, Sigma = V_beta)
  
  return(beta_tilde_j)
}

X <- cbind(rnorm(8),-1)
n_dup <- cbind(n, rep(n[1],3))
k <- y - (n_dup/2)

N <- length(w[1])
step2fun(X, w[,1],k[,1],0.1)

