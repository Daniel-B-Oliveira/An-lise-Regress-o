# Exemplo de dados
set.seed(123)
n <- 30
X1 <- rnorm(n)
X2 <- rnorm(n)
X3 <- rnorm(n)
Y  <- 0.5*X1 + 0.3*X2 - 0.2*X3 + rnorm(n)

# 1. Regressão de X1 contra as demais variáveis explicativas
modelo_v <- lm(X1 ~ X2 + X3)
v <- residuals(modelo_v)  # v_j

# 2. Regressão de Y contra as demais variáveis explicativas (exceto X1)
modelo_z <- lm(Y ~ X2 + X3)
z <- residuals(modelo_z)  # z_j

# 3. Cálculo do coeficiente de correlação parcial
r_parcial <- sum(v * z) / sqrt(sum(v^2) * sum(z^2))

# 4. Cálculo do coeficiente de determinação parcial
r2_parcial <- r_parcial^2

# 5. Exibir resultados
cat("Coeficiente de correlação parcial (r):", r_parcial, "\n")
cat("Coeficiente de determinação parcial (r^2):", r2_parcial, "\n")
