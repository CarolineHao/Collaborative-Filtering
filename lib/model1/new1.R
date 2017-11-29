train2 <- readRDS("C:/Users/ct2774/Downloads/train.movierank.RData")
write.csv(train2, "C:/Users/ct2774/Downloads/train2.csv")

weights_pearson_train1 <- readRDS("C:/Users/ct2774/Downloads/weights_pearson_train1.RData")
weights_spearman_train1 <- readRDS("C:/Users/ct2774/Downloads/weights_spearman_train1.RData")

#used with weights matrix: MSD and simrank
selectNeighborWeights <- function(df, weights, n){
  users <- rownames(df)
  m <- length(users)
  neighbors <- matrix(rep(NA, m * n), m, n)
  neighors <- data.frame(neighbors)
  rownames(neighbors) <- users
  
  for (i in 1:m){
    neighbors[i, ] <- colnames(weights)[order(weights[i, ], decreasing=TRUE)][1:n]
  }
  return (neighbors)
}

neighbors_pearson_train1 <- selectNeighborWeights(train1, weights_pearson_train1, 20)
neighbors_spearman_train1 <- selectNeighborWeights(train1, weights_spearman_train1, 20)

saveRDS(neighbors_pearson_train1, file = "C:/Users/ct2774/Downloads/neighbors_pearson_train1.RData")
saveRDS(neighbors_spearman_train1, file = "C:/Users/ct2774/Downloads/neighbors_spearman_train1.RData")

#a is the predicted user, i is the website id
computeZScore <- function(weights, neighbors, df, a, i){
  neighbors_a <- neighbors[toString(a), ] 
  num <- rep(0, length(neighbors_a))
  for (u in 1:length(neighbors_a)){
    neighborU <- neighbors_a[u] 
    r_ui <- df[toString(neighborU), toString(i)]
    r_u <- mean(df[neighborU, ])
    sd_u <- sd(df[neighborU, ])
    w_au <- weights[toString(a), neighborU]
    num[u] <- (r_ui - r_u)/sd_u * w_au
  }
  w_a <- sum(weights[toString(a), neighbors_a])
  sd_a <- sd(df[toString(a), ])
  index_not_i <- colnames(df) != i
  r_a <- mean(df[toString(a), index_not_i])
  p_ai <- r_a + sd_a * sum(num)/w_a
  return (p_ai)
}

computeZScoreMatrix <- function(weights, neighbors, df){
  a <- dim(df)[1]
  b <- dim(df)[2]
  users <- rownames(df)
  website <- colnames(df)
  computeZScoreMatrix <- matrix(rep(NA, a * b), a, b)
  for (i in 1:a){
    for (j in 1:b){
      computeZScoreMatrix[i, j] <- computeZScore(weights, neighbors, df, users[i], website[j])
    }
  }
  return (computeZScoreMatrix)
}

ZScoreMatrix_pearson_train1 <- computeZScoreMatrix(weights_pearson_train1, neighbors_pearson_train1, train1)

saveRDS(ZScoreMatrix_pearson_train1, file = "C:/Users/ct2774/Downloads/ZScoreMatrix_pearson_train1.RData")






















