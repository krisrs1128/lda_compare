#' Utilities for Clustering Demo


simulate_gradient <- function(n, mu0, mu1) {
  x <- matrix(nrow = n, ncol = length(mu0))
  
  for (i in seq_len(n)) {
    u <- runif(1)
    lambda <- u * mu0 + (1 - u) * mu1
    x[i, ] <- rnorm(2, lambda)
  }
  
  x
}


simulate_cluster <- function(n, mus) {
  x <- matrix(nrow = n, ncol = length(mu0))
  K <- nrow(mus)
  
  for (i in seq_len(n)) {
    x[i, ] <- rnorm(2, mus[sample(K)[1], ])
  }
  
  x
}


kmeans_df <- function(x, K = 1) {
  cluster_result <- kmeans(x, center = K)
  ix <- data.frame(
    ix = seq_len(nrow(x)),
    K = K,
    cluster = cluster_result$cluster
  )
  centroids <- data.frame(
    K = K,
    cluster_result$center
  )
  
  list(ix = ix, centroids = centroids)
}

get_links <- function(cluster1, cluster2) {
  n <- nrow(cluster1)
  result <- list()
  
  for (i in seq_len(n)) {
    result[[i]] <- data.frame(
      ix = cluster1$ix[i],
      K_parent = cluster1$K[i],
      K_child = cluster2$K[i],
      cluster_parent = cluster1$cluster[i],
      cluster_child = cluster2$cluster[i]
    ) 
  }
  
  do.call(rbind, result)
}

linked_cluster_data <- function(x, Ks = seq(2, 8)) {
  k_groups <- list()
  centroids <- list()
  
  for (k in seq_along(Ks)) {
    kres <- kmeans_df(x, Ks[k])
    k_groups[[k]] <- kres$ix
    k_groups[[k]]$K <- k_groups[[k]]$K + runif(nrow(k_groups[[k]]), 0, 0.4)
    k_groups[[k]]$cluster <- k_groups[[k]]$cluster + runif(nrow(k_groups[[k]]), 0, 0.4)
    centroids[[k]] <- kres$centroids
  }
  k_df <- do.call(rbind, k_groups)
  centroids <- do.call(rbind, centroids)
  
  k_links <- list()
  for (i in seq_along(k_groups)) {
    if (i == length(k_groups)) break
    k_links[[i]] <- get_links(k_groups[[i]], k_groups[[i + 1]])
  }
  
  list(links = do.call(rbind, k_links), groups = do.call(rbind, k_groups), centroids = centroids)
}


plot_clusters <- function(cluster_df) {
  ggplot(cluster_df$groups) +
    geom_point(aes(x = cluster, y = K)) +
    geom_segment(data = cluster_df$links, aes(x = cluster_parent, xend = cluster_child, y = K_parent, yend = K_child), alpha = 0.2) +
    coord_fixed()
}


transition_data <- function(links) {
  links %>%
    mutate(
      K_parent = round(K_parent),
      K_child = round(K_child),
      cluster_parent = round(cluster_parent),
      cluster_child = round(cluster_child),
    ) %>%
    group_by(K_parent, cluster_parent, cluster_child) %>%
    summarise(count = n())
}
