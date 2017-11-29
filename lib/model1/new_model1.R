############# weights

#############pearson + spearman

weights_pearson <- cor(t(train1), method = "pearson")
saveRDS(weights_pearson, file = "weights_pearson_train1.RData")

weights_spearman <- cor(t(train1), method = "spearman")
saveRDS(weights_spearman, file = "weights_spearman_train1.RData")

weights_pearson_train2 <- cor(t(train2), use = "pairwise.complete.obs", method = "pearson")
saveRDS(weights_pearson_train2, file = "weights_pearson_train2.RData")

weights_spearman_train2 <- cor(t(train2), use = "pairwise.complete.obs", method = "spearman")
saveRDS(weights_spearman_train2, file = "weights_spearman_train2.RData")

load("train_2.RData")
dim(train2)
train2 <- readRDS("D:/cu/courses/ads/proj4/Data/train.movierank.RData")

############### MSD

meanSquareDiff <- function(df){
  m <- dim(df)[1] #user number
  dissim <- matrix(rep(NA, m * m), m, m)
  dissim <- data.frame(dissim)
  users <- rownames(df)
  colnames(dissim) <- users
  rownames(dissim) <- users
  for (i in 1:m){
    for (j in 1:m){
      r_i <- df[i,]
      r_j <- df[j,]
      dissim[i, j] <- mean((r_i - r_j)^2, na.rm = T)
    }
  }
  maxDissim <- max(dissim)
  sim <- (maxDissim - dissim)/maxDissim
  return (sim)
  
}

weights_MSD_train1 <- meanSquareDiff(train1)
saveRDS(weights_MSD_train1, file = "weights_MSD_train1.RData")
