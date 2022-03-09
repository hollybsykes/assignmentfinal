#initial guess of the sample size
#we know that alpha>=0.05 and beta>=0.2
#we know delta = 0.7- 0.5
#guess the variance as 1
initial<-function(sigma, delta, alpha, beta){
  n<-2*sigma^2*(dnorm(1-alpha)-dnorm(beta))^2/delta^2
  return(n)
}
initial(1,0.2,0.05,0.2)




#now we want to find an appropiate gamma and alpha
#finding the expected sample size for any hypothesis
evaluate_design <- function(gamma, lambda, n1, n2, theta1, theta2) {
  
  # Estimate the expected sample size of a design defined by its 
  # decision rule parameters (lambda, gamma) and sample size 
  # parameters (n1, n2), along with its standard error.
  
  # Set the number of simulations.
  M <- 10^4
#we know theta is 0.5 so we can plug this in: theta(1,0.5,0.5)
    y1 <- rbinom(M, n1, theta1)

    # Get posterior Beta(a1, b1) parameters.
    a1 <- 0.5 + y1
    b1 <- 0.5 + n1 - y1

    # Probability of futility.
    fut1 <- pbeta(0.5, a1, b1)

    
    # Threshold to determine progression, based on the decision rule.
    c1 <- 1 - lambda * (n1 / n2)^gamma
   

    y2 <- rbinom(sum(fut1>c1), n2, theta2)
    a2 <- 0.5 + y2
    b2<- 0.5 + n2 - y2
    fut2 <- pbeta(0.5, a2, b2)
    c2<- 1 - lambda * (n2 / n2)^gamma
  # Return the estimated expected sample size and its estimated standard error.
  return(c(typeI = sum(fut1>c1)/M, typeII=sum(fut2>c2)/M))
}
evaluate_design(0.4,0.75,50,70,0.5,0.7)

prob_y <- function(y,n,theta){
  choose(n, y)*theta^(y)*(1-theta)^(n-y)
}
prob_y(10,30,0.5)
#check this function works - 
n<-30
theta<-0.5
sum(prob_y(0:n, n, 0.5)) # this sums to one -so okay!

sample_szie<- function
gamma<- seq(0,1,0.2)
lambda<- seq(0,1,0.2)
theta=0.5
df<- expand.grid(gamma=gamma,lambda=lambda)
df
fun<- apply(df, 1, evaluate_design, n1=n1, n2=n2, theta=theta)
#use vectorisation- as this too long of a method -> improves efficiency
#fix n1 and n2
#okay not to do a formal optimisation but do a comparison of parameters
#compare a few designs with different n1,  n2, gamma and lambda

#then ignore all options which dont meet constraints of alpha and beta
#include graph of theta

#in our case probability of futility is the likelihood
#prob_y1(y1, n1, theta) function only including the likelihood
#for binomial outcome what is the probability of getting a certain number of outcomes