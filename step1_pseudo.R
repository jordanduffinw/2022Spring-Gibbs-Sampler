step1fun <- function(df, J_items){
  
  # Define a matrix y with columns corresponding to affirmative responses to each item,
  # rows corresponding to each unique demographic profile.
  y <- df[ , (ncol(df)-J_items):(ncol(df)-1)]
  
  # Define a vector n corresponding to total respondents per unique demographic profile
  n <- df[ , ncol(df)]
  
  # Define a matrix mu in which each column of y will be divided by the n_i 
  # corresponding to the appropriate demographic profile--this is the observed probability of y, pi.
  
  # Next, apply qlogis() (logit) to each pi_{ij} given that pi is defined as the inverse logit of mu.
  
  mu <- data.matrix(do.call(cbind, apply(y, 2, function(x){qlogis(x/n)})))
  
  # Rename columns and rows appropriately
  colnames(mu) <- paste0("item_",1:N_items)
  rownames(mu) <- paste0("group_",1:nrow(mu))
  
  # Initialize empty matrix omega with appropriate dimensions
  w <- matrix(nrow = nrow(mu), ncol = ncol(mu))
  
  
  # Matrix values imputed elementwise in a loop for now for transparency, this must be optimized.
  
  # For each unique demographic profile i in 1...N
  for(i in 1:nrow(mu)){
    
    # For each response item j in 1...J
    for(j in 1:ncol(mu)){
      
      # Sample omega_{ij} from the Polya-Gamma distribution using the appropriate n_{ij} and mu_{ij}
      w[i,j] <- BayesLogit::rpg(1, as.numeric(n[i,]), mu[i,j])
      
    }
  }
  
  # Rename columns and rows appropriately
  colnames(w) <- paste0("item_",1:N_items)
  rownames(w) <- paste0("group_",1:nrow(mu))
  
  # Return the matrix omega of PG-sampled values
  return(w)
}