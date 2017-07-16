#Use this code to check if a matrix is stable. See source paper for more information.
#Source: Wolff Reiner (2005) - A Global Robustness Measure for Input-Output Projections from ESA and SNA Tables
#set your workspace, pc
setwd("c:/your/path/")
#set your workspace, mac
#setwd("your/path/")
#make a csv table that solely includes a square matrix
mydata <- read.csv("your_file.csv", header=FALSE, sep=",")
A <- as.matrix(mydata)
is.matrix(A)
B <- diag(43) - A
H <- t(B) %*% B
lamda <- 1
z <- rep(1, 43)
k <- 0
repeat
{
    k <- k + 1
    tetha <- lamda
    y <- H %*% z
    z <- y/as.numeric(sqrt(t(y) %*% y))
    lamda <- as.numeric(t(z) %*% H %*% z)
    if (abs(lamda - tetha) < 10^-8 | k > 500)
    {
        print(lamda)
        break
    }
}