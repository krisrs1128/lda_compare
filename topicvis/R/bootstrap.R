#' Functions to support bootstrapping
 
#' @export
resampling_plans <- function(n, B = 2000) {
  P <- matrix(0, nrow = B, ncol = n)
  colnames(P) <- seq_len(n)
  ix <- matrix(0, nrow = B, ncol = n)
  
  for (i in seq_len(B)) {
    ix[i, ] <- sample(1:n, n, replace = TRUE)
    ix_ <- table(ix[i, ])
    P[i, names(ix_)] <- ix_ / sum(ix_)
  }
  
  list(P = P, ix = ix)
}


#' @export
bootstrap <- function(x, ix, theta_fun) {
  B <- nrow(ix)
  theta_star <- vector(length = B)
  for (i in seq_len(B)) {
    theta_star[i] <- theta_fun(x[ix[i, ], ])
  }
    
  theta_star
}