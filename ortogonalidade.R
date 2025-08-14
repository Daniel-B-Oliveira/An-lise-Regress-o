X1=c(0,0,0,0,10,-10)
X2=c(-5,-5,5,5,0,0)
y=c(0.5,1,2,3,4,5)


y=c(1.5, 8.7, 2.8, 9.4, 4.4, 8.4, 3.2, 0.9) # CO2
pib=c(13.2, 197.0, 128.6, 286.4, 72.6, 167.8, 114.4, 58.0) # PIB
pop=c(3.2, 35.5, 19.1, 40.4, 3.1, 22.3, 8.4, 9.0) # Pop.


# Ortogonalização de Gram-Schmidt

# centralizando
pib_c <- pib - mean(pib)
pop_c <- pop - mean(pop)

# Ortogonalizando
phi1 <- pib_c
proj <- sum(pop_c * phi1) / sum(phi1^2) * phi1
phi2 <- pop_c - proj
# retira de pop_c o que é "comum" (ou colinear) com pib_c

# verificando
cor(phi1,phi2)

# Regressão com variáveis ortogonais
modelo2 <- lm(y ~ phi1 + phi2)
summary(modelo2)

# Neste modelo beta2 representa o efeito da população
# no CO2 que não é explicado pelo PIB.