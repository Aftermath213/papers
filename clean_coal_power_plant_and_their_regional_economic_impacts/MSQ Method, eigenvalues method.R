#Use this code to check if a matrix is stable. See source paper for more information.
#Source: Wolff Reiner (2005) - A Global Robustness Measure for Input-Output Projections from ESA and SNA Tables
#set your workspace, pc
setwd("c:/your/path/")
#set your workspace, mac
#setwd("your/path/")
#make a csv table that solely includes a square matrix
mydata <- read.csv("your_file.csv", header=FALSE, sep=",")
A <- as.matrix(mydata)
B <- diag(43) - A
H <- t(B) %*% B
lamdan <- max(Re(eigen(H)$values[abs(Im(eigen(H)$values)) < 1e-8]))
lamda1 <- min(Re(eigen(H)$values[abs(Im(eigen(H)$values)) < 1e-8]))
kappa <- lamdan/lamda1
kappa
tau <- 1/kappa
tau
G <- t(solve(B)) %*% solve(B)
sigman <- max(Re(eigen(G)$values[abs(Im(eigen(G)$values)) < 1e-8]))
sigma1 <- min(Re(eigen(G)$values[abs(Im(eigen(G)$values)) < 1e-8]))
k <- sigman/sigma1
t <- 1/k
t
tausqrt <- 1/(sqrt(lamdan)/sqrt(lamda1))
tausqrt