library(LearnBayes)
data("footballscores")
attach(footballscores)
d = favorite - underdog - spread
n = length(d)
v = sum(d^2)

# let P = 1/(sigma)^2
# by theory, P is distributed as U/v, where U has a chi-square distr. with n degree of freedom
P=rchisq(1000,n)/v # drawn 1000 sample data point from chi(n) density
s = sqrt(1/P)
hist(s, main="")
quantile(s, probs = c(0.25,0.5,0.75))


#model the mortality rate(lamda) where y(number of death given time intermal) is distributed
# by Poission(e*Lamda))
# probability density define the probability of a given interval. When the given interval is fixed, the probability
# varies from different density. Density function is used to represent different shape/type of density.
# by multiple the probability density by interval, one can get the probability of a given interval.
alpha = 16; beta = 15174  # weighted average from 10 hospitals
yobs = 1; ex = 66 # observation from the target hospital
y = 0:10 #possible y value
lam = alpha / beta 
dpois_ = dpois(y, 66 * lam) # sample density 
dgamma_ = dgamma(lam, alpha, beta)  #prior lamda density distr. given a gamma distr.
dgammapost_ = dgamma(lam, alpha + yobs, beta + ex) # posterior lamda density
py = dpois_ * dgamma_ / dgammapost_ # predictive density of y
cbind(y, round(py, 3))
