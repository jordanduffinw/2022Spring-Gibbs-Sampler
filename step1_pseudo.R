step1fun <- function(df, J_items){
  
  y <- df[ , (ncol(df)-J_items):(ncol(df)-1)]
  
  n <- df[ , ncol(df)]
  
  mu <- data.matrix(do.call(cbind, apply(y, 2, function(x){qlogis(x/n)})))
  
  colnames(mu) <- paste0("item_",1:N_items)
  rownames(mu) <- paste0("group_",1:nrow(mu))
  
  w <- matrix(nrow = nrow(mu), ncol = ncol(mu))
  
  for(i in 1:nrow(mu)){
    
    for(j in 1:ncol(mu)){
      
      w[i,j] <- BayesLogit::rpg(1, as.numeric(n[i,]), qlogis(mu[i,j]))
      
    }
  }
  
  colnames(w) <- paste0("item_",1:N_items)
  rownames(w) <- paste0("group_",1:nrow(mu))
  
  return(w)
}