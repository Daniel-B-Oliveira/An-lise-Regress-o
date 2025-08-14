x1 <- c(0, 1, 1, 2, 2, 3)
x2 <- c(0, 1, 2, 1, 2, 3)
y  <- c(-0.5, 3.5, 7, 7, 7.5, 11.5)

# Matriz de planejamento (X) com intercepto
X <- cbind(rep(1, length(y)), x1, x2)

# Estimativa dos coeficientes pelo método MQO
b <- solve(t(X) %*% X) %*% t(X) %*% y

# ------------------------
# 2. Teste da hipótese conjunta
#    H0: beta1 = 3 e beta2 = 5
# ------------------------
A <- rbind(c(0, 1, 0),  # seleciona beta1
           c(0, 0, 1))  # seleciona beta2

Ab     <- A %*% b             # valores estimados
Abeta  <- c(3, 5)             # valores sob H0

# Numerador do teste F
num <- t(Ab - Abeta) %*% solve(A %*% solve(t(X) %*% X) %*% t(A)) %*% (Ab - Abeta) /
  (length(b) - 1)

# Denominador: Quadrado Médio dos Resíduos
QMRes <- t(y - X %*% b) %*% (y - X %*% b) / (length(y) - length(b))
den   <- QMRes

# Valor de F calculado
valor.F <- num / den

# p-valor do teste
p.valor <- 1 - pf(valor.F, length(b) - 1, length(y) - length(b))

# Valor crítico para alfa = 0.05
F.crit <- qf(1 - 0.05, length(b) - 1, length(y) - length(b))

cat("Valor de F:", valor.F, "\n")
cat("p-valor:", p.valor, "\n")
cat("Valor crítico F (5%):", F.crit, "\n\n") # Se valor F maior que valor crítico rejeitamos a hipotese nula

# ------------------------
# 3. Conferência usando 'car::linearHypothesis'
# ------------------------
library(car)

modelo <- lm(y ~ x1 + x2)

# Teste conjunto beta1 = 0 e beta2 = 0
cat("Teste conjunto beta1 = 0 e beta2 = 0:\n")
print(linearHypothesis(modelo, c("x1=0", "x2=0")))

# Teste conjunto intercepto = 0, beta1 = 0 e beta2 = 0
cat("\nTeste intercepto = 0, beta1 = 0, beta2 = 0:\n")
print(linearHypothesis(modelo, c("(Intercept)=0", "x1=0", "x2=0")))

# Teste individual beta1 = 0
cat("\nTeste beta1 = 0:\n")
print(linearHypothesis(modelo, c("x1=0")))

# ------------------------
# 4. Região de confiança para (beta1, beta2)
# ------------------------
confidenceEllipse(modelo,
                  fill       = TRUE,
                  lwd        = 2,
                  levels     = 0.95,
                  which.coef = c("x1", "x2"),
                  main       = "Região de 95% de Confiança",
                  xlab       = expression(beta[1]),
                  ylab       = expression(beta[2]))

# Adiciona outra região de confiança ao gráfico
confidenceEllipse(modelo,
                  fill       = TRUE,
                  lwd        = 2,
                  levels     = 0.96,
                  which.coef = c("x1", "x2"),
                  add        = TRUE,
                  col        = "steelblue")
