#' Functions for topic visualization

#' @importFrom MCMCpack rdirichlet
#' @export
simulate_topics <- function(n, betas, n0=1000, alpha_theta=0.1) {
  x <- matrix(nrow = n, ncol = ncol(betas))
  thetas <- rdirichlet(n, rep(alpha_theta, nrow(betas)))
  
  for (i in seq_len(n)) {
    x[i, ] <- rmultinom(1, n0, t(betas) %*% thetas[i, ])
  }
  x
}

#' @importFrom reshape2 melt
#' @importFrom tidytext cast_dtm tidy
#' @importFrom topicmodels LDA
#' @export
fit_lda <- function(x, K=2, alpha_theta=1, alpha_beta=1) {
  x_dtm <- melt(x, varnames = c("book", "word")) %>%
    cast_dtm(book, word, value)
  fit <- LDA(x_dtm, K, control = list(alpha = alpha_theta, estimate.alpha = FALSE))
  theta <- tidy(fit, matrix = "gamma")
  beta <- tidy(fit, matrix = "beta")
  beta <- beta %>%
    mutate(K = K, alpha_theta = alpha_theta)
  theta <- theta %>%
    mutate(K = K, alpha_theta = alpha_theta)
  list(beta = beta, theta = theta)
}

#' @export
fit_ldas <- function(x, Ks, alpha_theta=1, alpha_beta=1) {
  lda_fits <- list()
  for (k in seq_along(Ks)) {
    print(sprintf("Fitting K = %s", Ks[k]))
    lda_fits[[k]] <- fit_lda(x, Ks[k], alpha_theta, alpha_beta) 
  }
  
  betas <- do.call(rbind, lapply(lda_fits, function(x) x$beta))
  thetas <- do.call(rbind, lapply(lda_fits, function(x) x$theta))
  list(thetas = thetas, betas = betas)
}

#' @export
fit_lda_alphas <- function(x, K, alpha_thetas=1, alpha_beta=1) {
  lda_fits <- list()
  for (i in seq_along(alpha_thetas)) {
    print(sprintf("Fitting alpha = %s", alpha_thetas[i]))
    lda_fits[[i]] <- fit_lda(x, K, alpha_thetas[i], alpha_beta) 
  }
  
  betas <- do.call(rbind, lapply(lda_fits, function(x) x$beta))
  thetas <- do.call(rbind, lapply(lda_fits, function(x) x$theta))
  list(thetas = thetas, betas = betas)
}

#' @importFrom dplyr select %>%
#'@export
ca_input <- function(beta_hat, beta_true, x) {
  x <- x / rowSums(x)
  beta_hat_mat <- beta_hat %>%
    select(as.character(seq_len(ncol(beta_true))))
  
  list(
    "matrix" = data.frame(rbind(beta_hat_mat, beta_true, x)),
    "meta" = data.frame(
      type = c(rep("beta_hat", nrow(beta_hat)), rep("beta_true", nrow(beta_true)), rep("x", nrow(x))),
      ix = c(seq_len(nrow(beta_hat)), seq_len(nrow(beta_true)), seq_len(nrow(x)))
    )
  )
}

#' @importFrom FactoMineR CA
#' @export
ca_output <- function(combined_mat, combined_meta) {
  ca_result <- CA(combined_mat, graph = FALSE)
  cbind(combined_meta, ca_result$row$coord)
}
