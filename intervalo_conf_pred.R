# Dados
y <- c(12, 13, 3, 3, 11, 19, 1, 14, 15, 17, 2, 15)
x1 <- c(31, 16, 29, 19, 27, 21, 24, 11, 26, 18, 12, 3)
x2 <- c(4, 5, 3, 0, 2, 6, 2, 3, 6, 6, 1, 5)

# Matriz X
X <- cbind(1, x1, x2)

# Estimativa dos betas
b <- solve(t(X) %*% X) %*% t(X) %*% y

# SQ totais
sq.total <- sum((y - mean(y))^2)
y_est <- X %*% b
sq.res <- sum((y - y_est)^2)
sq.reg <- sq.total - sq.res

# Quadrados médios
qm.reg <- sq.reg / (length(b) - 1)
qm.res <- sq.res / (length(y) - length(b))

# Teste H0: beta2 - beta1 = 0
A <- matrix(c(0, -1, 1), nrow = 1)
beta0 <- 0
variancia <- as.numeric(A %*% solve(t(X) %*% X) %*% t(A) * qm.res)
estatistica.t <- ((A %*% b) - beta0) / sqrt(variancia)

alpha <- 0.05
tcrit <- qt(alpha/2, length(y) - length(b), lower.tail = FALSE)

# Intervalo de confiança
x.h <- X[1, ]
x.b <- as.numeric(t(x.h) %*% b)
var.x.b <- as.numeric(t(x.h) %*% solve(t(X) %*% X) %*% x.h * qm.res)
limite.inferior <- x.b - tcrit * sqrt(var.x.b)
limite.superior <- x.b + tcrit * sqrt(var.x.b)

# Intervalo de predição
var.dif <- as.numeric((1 + t(x.h) %*% solve(t(X) %*% X) %*% x.h) * qm.res)
limite.inferior.pred <- x.b - tcrit * sqrt(var.dif)
limite.superior.pred <- x.b + tcrit * sqrt(var.dif)
