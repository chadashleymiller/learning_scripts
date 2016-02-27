#Simulate a random walk

bernoulligen <- function(number,prob,seed=12){
    set.seed(seed)
    d <- rep(prob,each=number)
    result <- sapply(d,function(x){rbinom(1,1,x)})
    return(result)
}

changevals <- function(vector,changeto){
  out <- NULL
  for(i in 1:length(vector)){
  if(vector[i]==0){
    out[i]<- changeto
  } else {
    out[i] <- vector[i]
  }
  }
  return(out)
}

walk <- function(vector,baseline){
  out <- NULL
  for(i in 2:length(vector)){
    out[i] <- baseline + sum(vector[1:i])
  }
  out[1] <- baseline + vector[1]
  return(out)
}

#Execute this script to generate a random walk of 1000 steps with p = 0.5
bridge <- bernoulligen(1000,0.50,seed=765)
bridge <- changevals(bridge,-1)
randomwalk <- walk(bridge,0)
time <- c(1:1000)
plot(time,randomwalk,pch=20)
lines(time,randomwalk)
abline(h=0,col=2,lty=3)


bridge <- bernoulligen(10000,0.51,seed=761)
bridge <- changevals(bridge,-1)
randomwalk <- walk(bridge,0)
time <- c(1:10000)
plot(time,randomwalk,pch=20)
lines(time,randomwalk)
abline(h=0,col=2,lty=3)

