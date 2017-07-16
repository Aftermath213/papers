mydata <- read.csv("your/path/your_file.csv", header=FALSE, sep=",")
A <- as.matrix(mydata)
is.matrix(A)
#download and load libraries
library("SQUAREM")
library("zoo")
library("sandwich")
library("strucchange")
power.method <- function(x, A) {
# x = starting guess for dominant eigenvector
# A = a square matrix
ax <- as.numeric(A %*% x)
f <- ax / sqrt(as.numeric(crossprod(ax)))
f
}
B <- diag(43) - A
root.matrix(B)
H <- t(B) %*% B
eigen(H)
p0 <- rnorm(43)
ans1 <- fpiter(p0, fixptfn=power.method, A=H)
ans1$par <- ans1$par / sqrt(sum(ans1$par^2))
# dominant eigenvector
ans1
# dominant eigenvalue
sigman <- c(t(ans1$par) %*% H %*% ans1$par) / c(crossprod(ans1$par))


#Wolff PM code
mydata <- read.csv("your/path/your_file.csv", nrow=43, header=FALSE, sep=",")
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

#My code big/small
mydata <- read.csv("your/path/your_file.csv", header=FALSE, sep=",")
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