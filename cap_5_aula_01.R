x <- rep(c(0, 1, 2, 3), 3)
y <- c(4, 7, 6, 9, 3, 4, 6, 5, 3, 2, 4, 5)

# Questão 5.8

# a) Determine as estimativas de alpha_h

# Gera as categorias (levels) por meio da função gl
trat <- gl(3,4)

# Cria o modelo com intercepto, a_2 e a_3
# alpha_1 ~= intercepto; alpha_2 ~= intercepto - a_3; alpha_3 ~= intercepto - a_2
modelo.intercepto <- lm(y ~ x + trat)

# Cria o modelo com a_1, a_2, e a_3
modelo <- lm(y ~ x + trat - 1)
summary(modelo)

X <- model.matrix(modelo)

# b) Quais as propriedades dessas estimativas?
# Os estiamadores de alpha_h são não viesados e de variância mínima

# c) Teste a hipótese   H_0: beta == 0
#                       H_1: beta != 0

estatistica.t <- (1 - 0) / 0.2582

SQRes <- sum((y - predict(modelo))^2)
QMRes <- SQRes / (length(y) - ncol(X))

cov.coef.matriz <- solve(t(X) %*% X) * QMRes

b <- matrix(as.vector(modelo$coefficients), 4, 1)
A <- matrix(c(1, 0, 0, 0), 1, 4)

hipotese.item.c <- 0

estatistica.t.item.c <- (A %*% b - hipotese.item.c) / sqrt(A %*% cov.coef.matriz %*% t(A))
qt(0.99, 11)

# d)  H_0: alpha_1 == 0
#     H_1: alpha_1 != 0

A <- matrix(c(0, 1, 0, 0), 1, 4)

hipotese.item.d <- 0

estatistica.t.item.c <- (A %*% b - hipotese.item.c) / sqrt(A %*% cov.coef.matriz %*% t(A))
qt(0.99, 11)