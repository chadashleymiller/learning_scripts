walk <- function(vector,baseline){
  out <- NULL
  for(i in 2:length(vector)){
    out[i] <- baseline + sum(vector[1:i])
  }
  out[1] <- baseline + vector[1]
  return(out)
}

rnormwalk <- function(steps,mean,std,baseline,seed){
  set.seed=seed
  norm <- rnorm(steps,mean,std)
  normwalk <- walk(norm,baseline)
  time <- c(1:steps)
  plot(time,normwalk,pch=20)
  lines(time,normwalk)
  abline(h=0,col=2,lty=3)
}

ranwalk.std10 <- rnormwalk(1000,0,10,0,7)
ranwalk.std100 <- rnormwalk(1000,0,100,0,5)



