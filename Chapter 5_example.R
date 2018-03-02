library(LearnBayes)
#chapter 5
#1. beta-binomial model to better fit data with overdispersion

data("cancermortality")
attach(cancermortality)

#log of likehood


mybetabinexch0<-function(theta,data){
  eta=theta[1]
  K=theta[2]
  y=data[,1]
  n=data[,2]
  logf=function(eta, K, n, y){
    lbeta(K*eta + y, K*(1-eta) + n - y) - lbeta(K*eta, K(1-eta))
  }
  val = sum(logf(eta,K,n,y))
  val=val -log(eta) - log(1-eta) - 2* log(1+K)
  return(val)
}
mybetabinexch<-function(theta,data){
  eta=exp(theta[1]/(1 + exp(theta[1])))
  K=exp(theta[2])
  y=data[,1]
  n=data[,2]
  logf=function(eta, K, n, y){
    lbeta(K*eta + y, K*(1-eta) + n - y) - lbeta(K*eta, K(1-eta))
  }
  val = sum(logf(eta,K,n,y))
  val=val + theta[2] - 2 * log(1 + exp(theta[2]))
  return(val)
}
mycontour(betabinexch0, c(.0001, .003, 1, 20000), cancermortality, xlab = "eta", ylab="K")  #log posterior density of the transformed parameter, eta, K
mycontour(betabinexch, c(-8,-4.5,3,16.5), cancermortality, xlab = "theta1 = logit(eta)", ylab="theta2 = log(K)")  #posterior density with original parameter, theta1, theta2


