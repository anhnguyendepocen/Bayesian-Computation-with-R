library(LearnBayes)
#chapter 2
#1
p = c(0,0.125,0.250,0.375,0.5,0.625,.75,.875)
prior=c(0.001,0.001,0.95,.008,.008,.008,.008,.008)
data=c(6,4)# number of success and number of failure
pdisc(p,prior,data)
#2
midpt=seq(0.05,0.95,0.1)
prior=c(1,5.2,8,7.2,4.6,2.1,0.7,0.1,0,0)
prior=prior/sum(prior)

p=seq(0,1,length=1000)
histprob=histprior(p,midpt,prior) #simulation of prior density
post=histprob * dbeta(p,12,10)  #simulation of posterior density
ps=sample(p,replace=TRUE, prob = post) #sample from posterior density
hist(ps,xlab="p",main = "")
hist(sample(p,replace=TRUE,prob = histprob)) #sample and plot from prior density

#3
qbeta(c(.05,.95),23,8)

1-pbeta(0.6,23,8)

post=rbeta(n=1000,23,8)
y=rbinom(1000,10,post) #calculate s(#of success out of 10 trail using posterior probability,
                       #simulate 1000 times
freq=table(y)
ys=as.integer(names(freq))
predprob=freq/sum(freq)
dist=cbind(ys,predprob)

#4
ab=c(3,12)
n=12
s=seq(0,12,by=1)
s_post=pbetap(ab,n,s)
s_dist=cbind(s,s_post)
hist(s_post,xlab='sam"s posterior')
p=seq(0.1,0.5,by=0.1)
prior=c(0.5,0.2,0.2,0.05,0.05)
j_post=pdiscp(p,prior,n,s)
j_dist=cbind(s,j_post)
hist(j_post,xlab='Joe"s posterior')

#5
mu=c(20,30,40,50,60,70)
prior=c(.1,.15,.25,.25,.15,.1)
y=c(38.6,42.4,57.5,40.5,51.7,67.1,33.4,60.9,64.1,40.1,40.7,6.4)
ybar=mean(y)
n=length(y)
sigma=10
like=exp(-(n*(mu-ybar)^2/(2*sigma^2)))
post=prior * like
post=post/sum(post)
dist=cbind(mu,post)
discint(dist,0.8)

#6
lamda=c(.5,1,1.5,2,2.5,3)
prior=c(.1,.2,.3,.2,.15,.05)
t=6
y=12
post=prior*exp(-t*lamda)*(t*lamda)^y
post=post/sum(post)
dist=cbind(lamda,post)
cond_prob=exp(-7*lamda)
prob=sum(cond_prob * post)

