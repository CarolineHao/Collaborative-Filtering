train <- read.csv("D:/cu/courses/ads/proj4/data1/anonymous-msweb_train.txt", header = F, stringsAsFactors=FALSE)
test <- read.csv("D:/cu/courses/ads/proj4/data1/anonymous-msweb_test.txt", header = F, stringsAsFactors=FALSE)
train <- data.frame(train)
test <- data.frame(test)

#train: user: 10001- 42711
#test: user: 10001-15000
#input: raw data, output: website * user matrix

preProcess <- function(df, row_number){
  dfHeader <- df[1:7, ]
  dfWeb <- df[df[,1] == "A", ]
  dfRes <- df[df[,1] == "C" | df[,1] == "V", ]
  n_web <- dim(dfWeb)[1]
  dfMatrix <- matrix(0,ncol = n_web, nrow = row_number)
  rownames(dfMatrix) <- 10001:(10000 + row_number)
  colnames(dfMatrix) <- dfWeb[ ,2]
  userIndex = dfRes[,1] == "C"
  userIndexNum = (1:dim(dfRes)[1])[userIndex]
  for (i in 1:(length(userIndexNum)-1)){
    currIndex = userIndexNum[i]
    nextIndex = userIndexNum[i+1]
    userID <- dfRes[currIndex,2]
    if (currIndex == (nextIndex - 2)){
      webID <- dfRes[(currIndex+1), 2]
      dfMatrix[toString(userID),toString(webID)] = 1
      }
    else{
      webID <- dfRes[(currIndex+1):(nextIndex-1), 2]
      for (j in webID){
        dfMatrix[toString(userID),toString(j)] = 1
      }
    }
  }
  r <- list(website = dfWeb, visit = dfMatrix)
  return (r)
}

preProcess(train, 32711)
preProcess(test, 5000)

