library(LearnBayes)
# chapter4 examples
data("election.2008")
attach(election.2008)
data=election.2008
M.pct = as.numeric(data$M.pct)
O.pct = as.numeric(data$O.pct)
prob.Obama=function(j){
  # p=rdirichlet(5000,
  #              c(251,201,51))
  p=rdirichlet(5000,
               500*c(M.pct[j], O.pct[j], 100 - M.pct[j] - O.pct[j])/100 + 1)
  # for  each state j, simulate 5000 data points from dirichlet distribution parameterized by
  # the ratio calculated from the sample poll.
  # for each state, a 5000 * 3 matrix is created
  # col[i,1] represent the win prob of McCain in ith simulation
  # col[i,2] represent the win prob of Obama in ith simulation
  mean(p[,2]>p[,1]) #ratio that total Obama winning over McCain winning.
                    #a winning is counted when col[i,2]>col[i,1]
                    #we get the avg.winning prob for Obama in state j from 5000 simulation.

}
Obama.win.probs = sapply(1:51,prob.Obama)
rbinom(51,1,Obama.win.probs)
