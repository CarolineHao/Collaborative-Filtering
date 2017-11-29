train2 <- readRDS("D:/cu/courses/ads/proj4/Data/train.movierank.RData")
train1 <- readRDS("D:/cu/courses/ads/proj4/Data/train1.RData")
test2 <- readRDS("D:/cu/courses/ads/proj4/Data/test.movierank.RData")
test1 <- readRDS("D:/cu/courses/ads/proj4/Data/test1.RData")


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

################ variance and significance weighting

# Variance weighting

# Dataset 1
variance_weight_Fun = function(a,u,df) {
  
  # i is the id of movie, a and u are id of users
  
  r_a = df[a, ]
  r_u = df[u, ] 
  
  r_a_mean = mean(r_a)
  r_a_sd = sd(r_a)
  r_u_mean = mean(r_u)
  r_u_sd = sd(r_u)
  
  
  n <- ncol(df)
  
  var <- vector()
  for(i in 1:n) {
    var[i] <- sum((df[, i]-mean(df[,i]))^2)/(length(df[,i]) - 1)
  }
  var_min <- min(var)
  var_max <- max(var)
  
  v_i <- vector()
  for(i in 1:n) {
    v_i[i] <- (sum((df[, i]-mean(df[,i]))^2)/(length(df[,i]) - 1) - var_min) / var_max
  }
  
  w_au = sum((r_a - r_a_mean) %*% (r_u - r_u_mean) %*% v_i) / sum(v_i)
  return(w_au)
  
  
  
}




users <- rownames(train1)
m <- length(users)

variance_weight <- matrix(rep(NA, m * m), m, m)
variance_weight <- data.frame(variance_weight)

for (i in 1:m){
  for (j in 1:m){
    variance_weight[i,j] <- variance_weight_Fun(users[i], users[j], train1)
  }
}

variance_weight
saveRDS(variance_weight, file = "variance_weight Dataset1.RData")



# Dataset 2

variance_weight_fun = function(a,u,df) {
  
  a <- df[a, ]
  u <- df[u, ]
  
  NA_Col_a <- colnames(a)[apply(a, 2, anyNA) == FALSE]
  NA_Col_u <- colnames(u)[apply(u, 2, anyNA) == FALSE]
  
  c <- intersect(NA_Col_a,NA_Col_u)
  r_a <- as.numeric(a[ , c(c)])
  r_u <- as.numeric(u[ , c(c)])
  # i is the id of movie, a and u are id of users
  
  r_a_mean = mean(r_a)
  r_a_sd = sd(r_a)
  r_u_mean = mean(r_u)
  r_u_sd = sd(r_u)
  
  
  
  n = length(c)
  var <- vector()
  for(i in 1:n) {
    var[i] <- sum((na.omit(df[,c(i)])-mean(na.omit(df[,c(i)])))^2)/(length(na.omit(df[,c(i)])) - 1)
  }
  var_min <- min(var)
  var_max <- max(var)
  
  v_i <- vector()
  for(i in 1:n) {
    v_i[i] <- (sum((na.omit(df[,c(i)])-mean(na.omit(df[,c(i)])))^2/(length(na.omit(df[,c(i)])) - 1)) - var_min) / var_max
  }
  
  w_au = sum((r_a - r_a_mean) %*% (r_u - r_u_mean) %*% v_i) / sum(v_i)
  return(w_au)
  
}

train_2 <- readRDS("train.movierank.RData")



variance_weight_fun("1","5",train_2)



train2 <- train_2[1:100,1:100]
rownames(train2)
users <- rownames(train_2)
m <- length(users)

variance_weight <- matrix(rep(NA, m * m), m, m)
variance_weight <- data.frame(variance_weight)

for (i in 1:m){
  for (j in 1:m){
    user_i <- users[i]
    user_j <- users[j]
    tmp  = train2[user_i, user_j]
    if (is.integer(tmp)){
      variance_weight[i,j] <- variance_weight_fun(users[i], users[j], train2)
    }
  }
}
variance_weight_fun(users[17], users[52], train2)
is.na(train2[users[17], users[52]])
variance_weight
saveRDS(variance_weight, file = "variance_weight Dataset2.RData")



# Significance weighting
''
# Dataset 1
significance_weight = function(a,u,df) {
  
  # i is the id of movie, a and u are id of users
  r_a = df[a, ]
  r_u = df[u, ] 
  
  r_a_mean = mean(r_a)
  r_a_sd = sd(r_a)
  r_u_mean = mean(r_u)
  r_u_sd = sd(r_u)
  
  num_a = colnames(df)[which(df[a, ] != 0)]
  num_u = colnames(df)[which(df[u, ] != 0)]
  
  n = length(intersect(num_a, num_u))
  significant_factor = ifelse(n < 50, n/50, 1)
  
  w_au = significant_factor * sum((r_a - r_a_mean) %*% (r_u - r_u_mean)) / (r_a_sd * r_u_sd)
  return(w_au)
  
}



#significance_weight("10117","10019",train1)
#significance_weight("10019","10117",train1)

users <- rownames(train1)
m <- length(users)

significant_weight <- matrix(rep(NA, m * m), m, m)
significant_weight <- data.frame(significant_weight)

for (i in 1:m){
  for (j in 1:m){
    significant_weight[i,j] <- significance_weight(users[i], users[j], train1)
  }
}



significant_weight
saveRDS(significant_weight, file = "significant_weight Dataset1.RData")




# Dataset 2.
significance_weight = function(a,u,df) {
  a <- df[a, -1]
  u <- df[u, -1]
  
  NA_Col_a <- colnames(a)[apply(a, 2, anyNA) == FALSE]
  NA_Col_u <- colnames(u)[apply(u, 2, anyNA) == FALSE]
  
  c <- intersect(NA_Col_a,NA_Col_u)
  r_a <- as.numeric(a[ , c(c)])
  r_u <- as.numeric(u[ , c(c)])
  
  # i is the id of movie, a and u are id of users
  
  r_a_mean = mean(r_a)
  r_a_sd = sd(r_a)
  r_u_mean = mean(r_u)
  r_u_sd = sd(r_u)
  
  
  n <- length(c)
  significant_factor <- ifelse(n < 50, n/50, 1)
  
  w_au = significant_factor * sum((r_a - r_a_mean) %*% (r_u - r_u_mean)) / (r_a_sd * r_u_sd)
  return(w_au)
  
}


users <- rownames(train_new)
m <- length(users)

significant_weight <- matrix(rep(NA, m * m), m, m)
significant_weight <- data.frame(significant_weight)

for (i in 1:m){
  for (j in 1:m){
    significant_weight[i,j] <- significance_weight(users[i], users[j], train_new)
  }
}



significant_weight
saveRDS(significant_weight, file = "significant_weight Dataset2.RData")




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


#################### simrank

compute_user_cum_movie_sum = function(m1,m2,
                                      Movie_Matrix_ = Movie_Matrix) {
  
  return(Movie_Matrix_[m1,m2])
}

compute_user_sim = function(j, k, iter = i,
                            train_adj_ = train_adj,
                            Movie_Matrix_ = Movie_Matrix,
                            Users_ = Users,
                            Movies_ = Movies) {
  
  if (j == k) {
    return(1)
  } else {
    moviesj = train_adj_$Movie[which(train_adj_$User == Users_[j])]
    moviesk = train_adj_$Movie[which(train_adj_$User == Users_[k])]
    
    # Pruning
    if (length(moviesj) + length(moviesk) - 
        length(unique(c(moviesj,moviesk))) <= 0.25 * length(moviesj)) {
      return(0)
    }
    
    # First Iter
    if (iter == 1) {
      return((length(moviesj) + length(moviesk) - 
                length(unique(c(moviesj,moviesk)))) * 0.8 / length(moviesk) / length(moviesj))
    }
    
    m1s = match(sort(rep(moviesj,length(moviesk))),Movies_)
    m2s = match(rep(moviesk,length(moviesj)),Movies_)
    
    mins = (m1s+m2s - sqrt((m1s-m2s)^2) )/2
    maxs = (m1s+m2s + sqrt((m1s-m2s)^2) )/2
    
    cum_sum = sum(mapply(compute_user_cum_movie_sum,
                         m1 = maxs, 
                         m2 = mins), na.rm=T)
    
    cum_sum = cum_sum * 0.8 / length(mins)
    return(cum_sum)
  }
}

compute_movie_cum_user_sum = function(m1,m2,
                                      User_Matrix_ = User_Matrix) {
  
  return(User_Matrix_[m1,m2])
}

compute_movie_sim = function(j, k, 
                             train_adj_ = train_adj,
                             User_Matrix_ = User_Matrix,
                             Users_ = Users,
                             Movies_ = Movies) {
  
  if (j == k) {
    return(1)
  } else {
    usersj = train_adj_$User[which(train_adj_$Movie == Movies_[j])]
    usersk = train_adj_$User[which(train_adj_$Movie == Movies_[k])]
    
    if (length(usersj) + length(usersk) - 
        length(unique(c(usersj,usersk))) <= 0.25 * length(usersj)) {
      return(0)
    }
    
    m1s = match(sort(rep(usersj,length(usersk))),Users_)
    m2s = match(rep(usersk,length(usersj)),Users_)
    
    mins = (m1s+m2s - sqrt((m1s-m2s)^2) )/2
    maxs = (m1s+m2s + sqrt((m1s-m2s)^2) )/2
    
    cum_sum = sum(mapply(compute_movie_cum_user_sum,
                         m1 = maxs, 
                         m2 = mins),na.rm=T)
    
    cum_sum = cum_sum * 0.8 / length(mins)
    return(cum_sum)
  }
}

generate_simrank_rdata = function(filename="simrank_matrix") {
  
  train = read.csv("../data/eachmovie_sample/data_train.csv")
  test = read.csv("../data/eachmovie_sample/data_test.csv")
  
  train$rescore = 0
  train$rescore[which(train$Score >= 6)] = 1
  
  train_adj = train[which(train$rescore ==1),]
  
  Movies = sort(unique(train_adj$Movie))
  Users = unique(train_adj$User)
  
  Movie_Matrix = diag(length(Movies))
  User_Matrix = diag(length(Users))
  
  for( i in 1:5 ) {
    print(paste0("i = ",i))
    for (j in 1:length(Users)) {
      if (j %% 100 == 0) {
        print(Sys.time())
        print(paste0("Users j=",j))
      }
      User_Matrix[j,1:j] = mapply(compute_user_sim,j=j,k=1:j)
    }
    
    for (j in 1:length(Movies)) {
      if (j %% 50 == 0) {
        print(Sys.time())
        print(paste0("Movies j=",j))
      }
      Movie_Matrix[j,1:j] = mapply(compute_movie_sim,j=j,k=1:j)
    }
    
  }
  
  User_Matrix[upper.tri(User_Matrix)] <- t(User_Matrix)[upper.tri(User_Matrix)]
  
  save(User_Matrix,Users,file=paste0("../output/",filename,".RData"))
  
}

################ select neighbors: best-n

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
train1
train1[1:10,1:10]
a <- "10010"
neighbors <- neighbors_significant_train1
u <- 1
i <- "1000"
df <- train1
weights_significant_train1[1:100, 1:100]

######################## predict: compute z-score

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

computeZScoreMatrix <- function(weights, neighbors, df, df_test){
  a <- dim(df_test)[1]
  b <- dim(df_test)[2]
  users <- rownames(df_test)
  website <- colnames(df_test)
  computeZScoreMatrix <- matrix(rep(0, a * b), a, b)
  for (i in 1:a){
    for (j in 1:b){
      if (is.na(df_test[i, j]) == F){
        #if (df_test[i, j] != 0){
        computeZScoreMatrix[i, j] <- computeZScore(weights, neighbors, df, users[i], website[j])
      }
      #computeZScoreMatrix[i, j] <- computeZScore(weights, neighbors, df, users[i], website[j])
    }
  }
  return (computeZScoreMatrix)
}


######################## some examples of running those functions


weights_pearson_train2 <- readRDS("D:/cu/courses/ads/proj4/model1/weights/weights_pearson_train2.RData")
weights_pearson_train2 <- readRDS("D:/cu/courses/ads/proj4/model1/weights/weights_spearman_train2.RData")
weights_spearman_train1 <- readRDS("D:/cu/courses/ads/proj4/model1/weights/weights_spearman_train1.RData")

neighbors_spearman_train1 <- readRDS("D:/cu/courses/ads/proj4/model1/weights/neighbors_spearman_train1.RData")

neighbors_pearson_train2 <- selectNeighborWeights(train2, weights_pearson_train2, 20)
neighbors_spearman_train2 <- selectNeighborWeights(train2, weights_spearman_train2, 20)



neighbors_MSD_train1 <- selectNeighborWeights(train1, weights_MSD_train1, 20)
saveRDS(neighbors_MSD_train1, file = "neighbors_MSD_train1.RData")

ZScoreMatrix_spearman_train1 <- computeZScoreMatrix(weights_spearman_train1, neighbors_spearman_train1, train1)
saveRDS(ZScoreMatrix_spearman_train1, file = "ZScoreMatrix_spearman_train1.RData")

ZScoreMatrix_MSD_train1 <- computeZScoreMatrix(weights_MSD_train1, neighbors_MSD_train1, train1)
saveRDS(ZScoreMatrix_MSD_train1, file = "ZScoreMatrix_MSD_train1.RData")

weights_MSD_train2 <- meanSquareDiff(train2)
saveRDS(weights_MSD_train2, file = "weights_MSD_train2.RData")

weights_significant_train1 <- readRDS("C:/Users/socuyy/Downloads/significant_weight Dataset1.RData")
colnames(weights_significant_train1) <- rownames(train1)
rownames(weights_significant_train1) <- rownames(train1)
weights_significant_train1[1:100, 1:100]


neighbors_significant_train1 <- selectNeighborWeights(train1, weights_significant_train1, 20)
saveRDS(neighbors_significant_train1, file = "neighbors_significant_train1.RData")

ZScoreMatrix_significant_train1 <- computeZScoreMatrix(weights_significant_train1, neighbors_significant_train1, train1, test1)
saveRDS(ZScoreMatrix_significant_train1, file = "ZScoreMatrix_significant_train1.RData")


weights_significant_train2 <- readRDS("C:/Users/socuyy/Downloads/significant_weight Dataset2.RData")

train2_1 <- train2[1:100,]
test2_1 <- test2[1:100,]

neighbors_significant_train2 <- selectNeighborWeights(train2_1, weights_significant_train2, 20)
saveRDS(neighbors_significant_train2, file = "neighbors_significant_train2.RData")

ZScoreMatrix_significant_train2 <- computeZScoreMatrix(weights_significant_train2, neighbors_significant_train2, train2_1, test2_1)
saveRDS(ZScoreMatrix_significant_train1, file = "ZScoreMatrix_significant_train1.RData")