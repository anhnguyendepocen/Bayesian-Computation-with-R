library(LearnBayes)

data("hearttransplants")
attach(hearttransplants)

plot(log(e), y/e, xlim=c(6,9.7), xlab = "log(e)", ylab = "y/e")
text(log(e), y/e, labels = as.character(y), pos = 4)

#plot(e, y/e, xlab = "log(e)", ylab = "y/e")
#text(e, y/e, labels = as.character(y), pos = 4)

#assume common mortality rate lamda with standard noninformative prior
a = sum(y)
b = sum(e)
