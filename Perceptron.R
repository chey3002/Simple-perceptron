and=matrix( 
  c( 1, 1, 1,
     1,-1,-1,
    -1, 1,-1,
    -1,-1,-1), # the data elements 
  nrow=4,              # number of rows 
  ncol=3,              # number of columns 
  byrow = TRUE)
or=matrix( 
  c( 1, 1, 1,
     1,-1, 1,
    -1, 1, 1,
    -1,-1,-1),
  nrow=4,              
  ncol=3,              
  byrow = TRUE)
xor=matrix( 
  c( 1, 1, 1,
     1,-1,-1,
    -1, 1,-1,
    -1,-1, 1),  
  nrow=4,              
  ncol=3,             
  byrow = TRUE)
toys=matrix( 
    c( -1  ,-1  ,-1,
       -5  ,-2.5,-1,
       -7.5, 7.5,-1,
       10  , 7.5, 1,
       -2.5,12.5,-1,
        5  ,10  , 1,
        5  , 5  , 1), 
  nrow=7,              
  ncol=3,              
  byrow = TRUE)
PerceptronBin <- function(data) {
  teta=runif(1,0,1)
  w=runif(ncol(data)-1,0,1)
  misclassfied= TRUE
  k=0
  while (misclassfied) {
    k=k+1
    misclassfied= FALSE
    for (i in 1:nrow(data)) {
      y=sum(c(w*data[i,1:(ncol(data)-1)],teta))
      if(sign(y)!=sign(data[i,ncol(data)])){
        w=w+(data[i,1:(ncol(data)-1)]*data[i,ncol(data)])
        teta=teta+(data[i,ncol(data)])
        misclassfied= TRUE
      }    
    }
    if(k==100000){
      misclassfied= FALSE
    }
    if(!misclassfied){
      print("Test")
      for (i in 1:nrow(data)) {
        y=sum(c(w*data[i,1:(ncol(data)-1)],teta))
        print(sign(y))    
      } 
    }
    
  }
  w=(c(w,teta))
  return(w)
}
plotTheLine <- function(data, title,a1,b1) {#y=a+b*x
  plot(data,pch= ifelse(data[, 3] == -1, 18, 19),col = ifelse(data[, 3] == -1, "blue", "red"),xlab = "X", ylab = "Y",main=title)
  abline(a=a1,b=b1)
}
w=PerceptronBin(and)
print("Pesos And: ")
print(w)
plotTheLine(and,"AND",-w[3]/w[2],-w[1]/w[2])
w=PerceptronBin(or)
print("Pesos Or: ")
print(w)
plotTheLine(or,"OR",-w[3]/w[2],-w[1]/w[2])
w=PerceptronBin(toys)
print("Pesos Toys: ")
print(w)
plotTheLine(toys,"Toys",-w[3]/w[2],-w[1]/w[2])

w=PerceptronBin(xor)

#Solo puede solucionar problemas que se solucionan linealmente
