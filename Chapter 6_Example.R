
# Example from chapter 6 MCMC
# chain is defined under P
P = matrix(c(.5,.5,0,0,0,0,.25,.5,.25,0,0,0,0,.25,.5,.25,0,0,0,0,.25,.5,.25,0,0,0,0,.25,.5,.25,0,0,0,0,.5,.5),
           6,6,byrow = TRUE)
# array holds individual simulation resultes based on the chain
s = array(0,c(50000,1))
s[1] = 3
for (i in 2:50000){
  s[i] = sample(1:6, size = 1, prob = P[s[i-1],])
}
#check summary in m steps of the chain, when simulation is large enough, the probability starts to converge
# to the stationary distribution. 
m = c(500,2000,8000,50000)
for (i in 1:length(m)){
  print(table(s[1:m[i]])/m[i])
}
#stationary distribution of the chain
w = c(0.1,0.2,0.2,0.2,0.2,0.1)
#checking whether w is stationary distribution of this chain
w %*% P
#equal to w, therefore, w is the stationary distribution of P
