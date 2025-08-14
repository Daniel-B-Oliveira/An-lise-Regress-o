y<-c(12, 13, 3, 3, 11, 19, 1, 14, 15, 17, 2, 15)

x1<-c(31, 16, 29, 19, 27, 21, 24, 11, 26, 18, 12, 3)
x2<-c(4, 5, 3, 0, 2, 6, 2, 3, 6, 6, 1, 5)

X <- cbind(rep(1, 12), x1, x2)

b <- solve(t(X) %*% X) %*% t(X) %*% y

A <- rbind(c(0,0,1))

Ab <- A %*% b

Abeta <- c(0)

# Teste F

num <- t(Ab - Abeta) %*% solve(A %*% solve(t(X) %*% X) %*% t(A)) %*% (Ab - Abeta) / (length(b) - 1)

QMRes <- t(y-X %*% b) %*% (y-X %*% b) / (length(y) - length(b))
den <- QMRes

valor.F <- num / den

1 - pf(valor.F, 1, length(y) - length(b))
qf(1-0.05, length(b) - 1,  length(y) - length(b))

