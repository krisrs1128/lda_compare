#' Functions for topic visualization

simulate_topics <- function(n, betas, n0=1000, alpha_theta=0.1) {
  x <- matrix(nrow = n, ncol = ncol(betas))
  thetas <- rdirichlet(n, rep(alpha_theta, nrow(betas)))
  
  for (i in seq_len(n)) {
    x[i, ] <- rmultinom(1, n0, t(betas) %*% thetas[i, ])
  }
  x
}

fit_lda <- function(x, K=2, alpha_theta=1, alpha_beta=1) {
  x_dtm <- melt(x, varnames = c("book", "word")) %>%
    cast_dtm(book, word, value)
  fit <- LDA(x_dtm, K, control = list(alpha = alpha_theta))
  theta <- tidy(fit, matrix = "gamma")
  beta <- tidy(fit, matrix = "beta")
  beta$K <- K
  theta$K <- K
  list(beta = beta, theta = theta)
}

fit_ldas <- function(x, Ks) {
  lda_fits <- list()
  for (k in seq_along(Ks)) {
    print(sprintf("Fitting K = %s", Ks[k]))
    lda_fits[[k]] <- fit_lda(x, Ks[k]) 
  }
  
  betas <- do.call(rbind, lapply(lda_fits, function(x) x$beta))
  thetas <- do.call(rbind, lapply(lda_fits, function(x) x$theta))
  list(thetas = thetas, betas = betas)
}