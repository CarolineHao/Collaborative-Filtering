
MAE<-function(predict,true){
  
  p<-na.omit(as.vector(predict))
  t<-na.omit(as.vector(true))
  mae<-mean(abs(p-t))
  
  return(mae)
}
  
# The smaller, the better



ROC<-function(predict,true){
  p<-na.omit(as.vector(predict))
  t<-na.omit(as.vector(true))
  s<-rep(0,length(p))
  
  for(i in 1:length(p)){
    if((p[i]>3 & t[i]>3)|(p[i]<4 & t[i]<4)){
      s[i]<-1
    }
  }
  return(mean(s))
}

# The bigger, the better





RS<-function(predict,true,d=0,alpha=10){
  
  r<-rep(0,nrow(true))
  r_max<-rep(0,nrow(true))
  
  for(a in 1:nrow(true)){
    
    r[a]<-sort(r[a],decreasing = T)
    true[a,]<-sort(true[a,],decreasing = T)
    
    for(j in 1:ncol(true)){
      
      r[a]<-r[a]+max(predict[a,]-d,0)/2^((j-1)/(alpha-1))
      r_max[a]<-r_max[a]+sum(max(true[a,]-d,0)/2^((j-1)/(alpha-1)))
      
    }
  }
  
  rs<-100*sum(r)/sum(r_max)
  return(rs)
  
}

# The bigger, the better
