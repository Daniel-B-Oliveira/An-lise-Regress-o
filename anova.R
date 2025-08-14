# Dados
y <- c(12, 13, 3, 3, 11, 19, 1, 14, 15, 17, 2, 15)
x1 <- c(31, 16, 29, 19, 27, 21, 24, 11, 26, 18, 12, 3)
x2 <- c(4, 5, 3, 0, 2, 6, 2, 3, 6, 6, 1, 5)

# Matriz do modelo completo (com intercepto)
X <- cbind(1, x1, x2)

# Estimativa dos coeficientes
beta <- solve(t(X) %*% X) %*% t(X) %*% y

# SQ total, regressão e resíduos (modelo completo)
sq.total <- t(y) %*% y - length(y) * mean(y)^2
sq.reg <- t(X %*% beta) %*% y - length(y) * mean(y)^2
sq.res <- sq.total - sq.reg

# Quadrados médios
qm.reg <- sq.reg / (length(beta) - 1)  # p = nº parâmetros - 1
qm.res <- sq.res / (length(y) - length(beta))  # gl = n - p

# Estatística F do modelo completo
F.completo <- qm.reg / qm.res
F.crit.completo <- qf(0.99, length(beta) - 1, length(y) - length(beta))

#-----------------------------------------------
# Teste parcial para x2 (H0: beta2 = 0)
#-----------------------------------------------
X.reduzido <- X[,-3]

beta.reduzido <- solve(t(X.reduzido) %*% X.reduzido) %*% t(X.reduzido) %*% y
sq.reg.reduzido <- t(X.reduzido %*% beta.reduzido) %*% y - length(y) * mean(y)^2

# Diferença entre SQs de regressão
sq.reg.beta2 <- sq.reg - sq.reg.reduzido
qm.reg.beta2 <- sq.reg.beta2 / (length(beta) - length(beta.reduzido))

# Estatística F parcial para x2
F.beta2 <- qm.reg.beta2 / qm.res
F.crit.beta2 <- qf(0.99, length(beta) - length(beta.reduzido), length(y) - length(beta))

#-----------------------------------------------
# Teste parcial para x1 (H0: beta1 = 0)
#-----------------------------------------------
X.reduzido2 <- X[,-2]
beta.reduzido2 <- solve(t(X.reduzido2) %*% X.reduzido2) %*% t(X.reduzido2) %*% y
sq.reg.reduzido2 <- t(X.reduzido2 %*% beta.reduzido2) %*% y - length(y) * mean(y)^2

# Diferença entre SQs de regressão
sq.reg.beta1 <- sq.reg - sq.reg.reduzido2
qm.reg.beta1 <- sq.reg.beta1 / (length(beta) - length(beta.reduzido2))

# Estatística F parcial para x1
F.beta1 <- qm.reg.beta1 / qm.res
F.crit.beta1 <- qf(0.99, length(beta) - length(beta.reduzido2), length(y) - length(beta))

#-----------------------------------------------
# Resultados
#-----------------------------------------------
cat("\n--- Modelo completo ---\n")
cat("F =", F.completo, " | F crítico =", F.crit.completo, "\n") # Para ser significativo deve ser maior

cat("\n--- Teste parcial para x2 ---\n")
cat("F =", F.beta2, " | F crítico =", F.crit.beta2, "\n")

cat("\n--- Teste parcial para x1 ---\n")
cat("F =", F.beta1, " | F crítico =", F.crit.beta1, "\n")

# Conferência com funções prontas
cat("\n--- Conferência com aov/anova ---\n")
print(summary(aov(y ~ x1 + x2)))
print(summary(aov(y ~ x2 + x1)))
