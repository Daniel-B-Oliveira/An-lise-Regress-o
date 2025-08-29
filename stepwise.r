

rm(list=ls())
library(readxl)


#setwd("~/UFU/2025.1/Análise de Regressão")
dados <- read.table("preco_frangos.txt", header = T, dec = ",")

attach(dados)
names(dados)

#########################
## Método stepwise


modelo0=lm(Y~1)

step(modelo0,scope=~X1+X2+X3+X4+X4,direction="forward")
summary(modelo0) # modelo só com uma constante beta0 (média)


# Etapa de entrada.
add1(modelo0, scope=~X1+X2+X3+X4+X5,test="F") # X1 entra no modelo


# Etapa de entrada.
modelo1=lm(Y~X1)
add1(modelo1, scope=~X1+X2+X3+X4+X5,test="F") # X2 entra no modelo a 10%


# Etapa de saida.  
# X1 deve sair?? Perdeu "importância"?
modelo2=lm(Y~X1+X2)
drop1(modelo2, test="F") # X1 permanece 


#Etapa de entrada
# X3, X4 ou X5 entra no modelo? 
add1(modelo2, scope=~X1+X2+X3+X4+X5,test="F")  # X3 entra.


# Etapa de saida.
# X1 ou X2 sai?
modelo3=lm(Y~X1+X2+X3)


drop1(modelo3, test="F") # Nenhuma sai.


# Etapa de entrada.
# X4 ou X5 entra?
add1(modelo3, scope=~X1+X2+X3+X4+X5,test="F") # Nenhuma entra. SELEÇÃO ENCERRADA.






## OBS.: Verificar pressupostos de normalidade, independência
## e homogeneidade





###################### Outro exemplo

# Dados: Indicadores socioeconômicos e de fertilidade
# da Suíça (1888)

data(swiss)

names(swiss)
help("swiss")
attach(swiss)

head(swiss)


modelo0 <- lm(Fertility ~ 1, data = swiss)
summary(modelo0)

# Etapa de entrada
add1(modelo0, scope=~Agriculture+Examination+Education+Catholic+Infant.Mortality,test="F") # 
# Educação entra no modelo

modelo1 <- lm(Fertility ~ Education, data = swiss)
summary(modelo1)

# Etapa de entrada
add1(modelo1, scope=~Agriculture+Examination+Education+Catholic+Infant.Mortality,test="F")
# Católico entra no modelo

modelo2 <- lm(Fertility ~ Education + Catholic, data = swiss)
summary(modelo2)


# Etapa de retirada/saída 
drop1(modelo2, test="F")  # Nenhuma sai.


# Etapa de entrada
add1(modelo2, scope=~Agriculture+Examination+Education+Catholic+Infant.Mortality,test="F")
# Mortalidade infantial entra no modelo


modelo3 <- lm(Fertility ~ Education + Catholic +Infant.Mortality , data = swiss)
summary(modelo3)

# Etapa de retirada/saída
drop1(modelo3, test="F") # Nenhuma sai.


# Etapa de entrada
add1(modelo3, scope=~Agriculture+Examination+Education+Catholic+Infant.Mortality,test="F")
# Agicultura entra no modelo


modelo4 <- lm(Fertility ~ Education + Catholic +Infant.Mortality+Agriculture, data = swiss)
summary(modelo4)


# Etapa de retirada/saida
drop1(modelo4, test="F") # nenhuma sai


# Etapa de entrada
add1(modelo4, scope=~Agriculture+Examination+Education+Catholic+Infant.Mortality,test="F")
# nenhuma mais entra

# seleção encerrada
# avaliar os pressupostos de normalidade, independência e homogeneidade de variância





