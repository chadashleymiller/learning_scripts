#R code - Linear Regression Estimators Simulation

#First, let's specify a true DGP: y = alpha + beta1*x1 + beta2*x2 + epsilon

alpha <- 5
beta1 <- 10
beta2 <- 15

#Here is how I would run the simulation one time

x1 <- rnorm(200)
x2 <- rnorm(200)
epsilon <- rnorm(200)
y <- alpha + beta1*x1 + beta2*x2 + epsilon

dataframe <- data.frame(cbind(y,x1,x2))
lm1 <- lm(y ~ x1 + x2, data=dataframe)
coefficients <- as.vector(lm1[[1]])
alphahat <- coefficients[1]
beta1hat <- coefficients[2]
beta2hat <- coefficients[3]

#Now let's write a loop that does this a lot of times and spits out estimates
simulate <- function(iterations,datalength=200,alpha=5,beta1=10,beta2=15){
				coefficientmatrix <- matrix(1:(3*iterations),iterations,3)
				for(i in 1:iterations){
				x1 <- rnorm(datalength)
				x2 <- rnorm(datalength)
				epsilon <- rnorm(datalength)
				y <- alpha + beta1*x1 + beta2*x2 + epsilon
				dataframe <- data.frame(cbind(y,x1,x2))
				lm1 <- lm(y ~ x1 + x2, data=dataframe)
				coefficients <- as.vector(lm1[[1]])
				coefficientmatrix[i,] <- coefficients}
				return(coefficientmatrix)
					}
#Now let's look at histograms of each estimated coefficient with the true value overlaid
histograms <- function(simulations,alpha=5,beta1=10,beta2=15){
			par(mfrow=c(1,3))
			hist(x=simulations[,1],main="alphahat",xlab="alphahat",freq=FALSE)
			abline(v = alpha,col="red")
			lines(density(x=simulations[,1],adjust=2))
			hist(x=simulations[,2],main="beta1hat",xlab="beta1hat",freq=FALSE)
			abline(v = beta1,col="red")
			lines(density(x=simulations[,2],adjust=2))
			hist(x=simulations[,3],main="beta2hat",xlab="beta2hat",freq=FALSE)
			abline(v = beta2,col="red")
			lines(density(x=simulations[,3],adjust=2))}

#Now let's look at the difference in estimates for two estimates with different datalengths
densitycompare <- function(simulation1,simulation2,alpha=5,beta1=10,beta2=15){
			par(mfrow=c(1,3))
			hist(x=simulation1[,1],main="alphahat",xlab="alphahat",col=rgb(0,0,1,1/4),freq=FALSE)
			hist(x=simulation2[,1],col=rgb(1,0,0,1/4),freq=FALSE,add=TRUE)
			abline(v = alpha,col="red")
			lines(density(x=simulation1[,1],adjust=2))
			lines(density(x=simulation2[,1],adjust=2))
			hist(x=simulation1[,2],main="beta1hat",xlab="beta1hat",col=rgb(0,0,1,1/4),freq=FALSE)
			hist(x=simulation2[,2],col=rgb(1,0,0,1/4),freq=FALSE,add=TRUE)
			abline(v = beta1,col="red")
			lines(density(x=simulation1[,2],adjust=2))
			lines(density(x=simulation2[,2],adjust=2))
			hist(x=simulation1[,3],main="beta2hat",xlab="beta2hat",col=rgb(0,0,1,1/4),freq=FALSE)
			hist(x=simulation2[,3],col=rgb(1,0,0,1/4),freq=FALSE,add=TRUE)
			abline(v = beta2,col="red")
			lines(density(x=simulation1[,3],adjust=2))
			lines(density(x=simulation2[,3],adjust=2))}

#Sample program to operate:
estimates <- simulate(iterations=1000,datalength=300)
estimates1 <- simulate(iterations=1000,datalength=3000)
densitycompare(estimates,estimates1)


