

rm(list=ls())

library(readxl)

dados <- read.table("preco_frangos.txt", header = T, dec = ",")

names(dados)
attach(dados)
head(dados)


#########################
## M?todo backward


# Passo 1
modelo = lm(Y~X1+X2+X3+X4+X5,data=dados) # Modelo completo* 
modelo1 = lm(Y~X2+X3+X4+X5,data=dados) # verificando se X1 é uma candidata a sair

# (*) conceito relativo - o modelo completo pode depender do analista

summary(aov(modelo))
summary(aov(modelo1))

SQRes_c = 63.5
SQRes_r = 67 

F1=(SQRes_r - SQRes_c)/3.7 # 3.7 = QMRes_c modelo completo
F1

length(Y)

qf(0.95,1,17) # Valor crítico (X1 pode sair do modelo)


# Forma mais rápida ainda
drop1(modelo,test="F") # X5 sai

summary(modelo)

# Passo 2 

modelo2=lm(Y~X1+X2+X3+X4,data=dados) # modelo completo neste passo
summary(aov(modelo2))
drop1(modelo2,test="F") # X1 sai

# Passo 3
modelo3=lm(Y~X2+X3+X4,data=dados) # modelo completo neste passo
drop1(modelo3,test="F") # Variáveis no modelo final: X2, X3 e X4 


# o modelo final atende os pressupostos de normalidade, independência e 
# homogeneidade 

modelo=lm(Y~X2+X3+X4,data=dados)
shapiro.test(residuals(modelo))
library(lmtest)
dwtest(modelo) # o pressuposto de independência é rejeitado


plot(residuals(modelo))
plot(predict(modelo),residuals(modelo))
attach(dados)
plot(dados$X2,residuals(modelo))
plot(dados$X3,residuals(modelo))
plot(dados$X4,residuals(modelo))


modelo=lm(Y~X2+X3+X4+I(X3^2),data=dados)
shapiro.test(residuals(modelo))
library(lmtest)
dwtest(modelo) # o pressuposto de independ?ncia ? rejeitado

plot(residuals(modelo))
plot(predict(modelo),residuals(modelo))
plot(X2,residuals(modelo))
plot(X3,residuals(modelo))
plot(X4,residuals(modelo))


modelo=lm(Y~X2+X3+X4+I(X3^2)+I(X4^2))
shapiro.test(residuals(modelo))
library(lmtest)
dwtest(modelo) # o pressuposto de independ?ncia ? rejeitado

modelo=lm(Y~X2+X3+x4+I(X2^2)+I(X3^2)+I(X4^2))
shapiro.test(residuals(modelo))
library(lmtest)
dwtest(modelo) 
bptest(modelo)
# pressupostos ok

drop1(modelo,test="F")

# Exploração gráfica dos resíduos
plot(residuals(modelo))
plot(predict(modelo),residuals(modelo))

qqnorm(residuals(modelo)) # quanto mais próximo da bissetriz do
#primeiro e terceiro quadrante melhor
qqline(residuals(modelo))

sort(residuals(modelo))
qqnorm(residuals(modelo),ylim=c(-3,3),xlim=c(-3,3)) # quanto mais próximo da bissetriz do
#primeiro e terceiro quadrante melhor
qqline(residuals(modelo))





#########################
## Método forward

mean(Y)

rm(list=ls())

library(readxl)
setwd("~/UFU/2024.2/Análise de Regressão")

dados <- read_excel("preco_frangos.xls")
names(dados)
attach(dados)



# Passo 1 (O modelo "mínimo" será composto por qual variável regressora???)

modelo1=lm(Y~X1)
summary(aov(modelo1)) # X1 é uma candidata a compor o modelo

modelo2=lm(Y~X2)
summary(aov(modelo2)) # X2 é uma candidata a compor o modelo

modelo3=lm(Y~X3)
summary(aov(modelo3)) # X3 é uma candidata a compor o modelo

modelo4=lm(Y~X4)
summary(aov(modelo4)) # X4 é uma candidata a compor o modelo

modelo5=lm(Y~X5)
summary(aov(modelo5)) # X5 é uma candidata a compor o modelo

# Conclus?o passo 1: X1 selecionada


# Passo 2 (alguma outra variável, além de X1, entra no modelo?)

# X2 entra no modelo?
modelo6=lm(Y~X1+X2)
summary(aov(modelo1))
summary(aov(modelo6))

F= ((127.4 - 109.1)/1)/(109.1/20)
F
qf(0.95,1,20) # valor cr?tico a 5%
qf(0.90,1,20) # valor cr?tico a 10%

# A 10% de significância X2 é 
# uma candidata a entrar no modelo junto com X1

# forma um pouco mais rápida
anova(modelo1,modelo6) 

# X3 entra no modelo junto com X1?
modelo7=lm(Y~X1+X3)
anova(modelo1,modelo7) # X3 não entra no modelo

# X4 entra no modelo junto com X1?
modelo8=lm(Y~X1+X4)
anova(modelo1,modelo8) # X4 não entra no modelo

# X5 entra no modelo junto com X1?
modelo9=lm(Y~X1+X5)
anova(modelo1,modelo9) # X5 não entra no modelo

# conclusão do passo 2: X2 entra no modelo a 10% de significância


# Passo 3 (alguma outra variável, além de X1 e X2, entra no modelo?)

modelo10=lm(Y~X1+X2) # ... modelo base agora, ... continua
# testando se X3, X4 e X5 são candidatas a entrar no modelo ...


modelo11=lm(Y~X1+X2+X3)
anova(modelo10,modelo11) # X3 é uma candidata a entrar no modelo


modelo12=lm(Y~X1+X2+X4)
anova(modelo10,modelo12) # X4 não é uma candidata a entrar no modelo


modelo13=lm(Y~X1+X2+X5)
anova(modelo10,modelo13) # X5 não é uma candidata a entrar no modelo

# conclus?o passo 3: X3 entra no modelo junto com X1 e X2


# Passo 4

modelo14=lm(Y~X1+X2+X3) # ... modelo base agora, ... continua
# testando se X4 e X5 são candidatas a entrar no modelo ...


# Etapa 4 (alguma outra variável, al?m de X1, X2 e X3, entra no modelo?)

modelo15=lm(Y~X1+X2+X3+X4)
anova(modelo14,modelo15) # X4 não é uma candidata a entrar no modelo

modelo16=lm(Y~X1+X2+X3+X5)
anova(modelo14,modelo16) # X5 não é uma candidata a entrar no modelo


# ENCERRA-SE O PROCESSO FORWARD E O MODELO FINAL ?: modelo14=lm(Y~X1+X2+X3)
summary(modelo14)


mod=lm(Y ~ X1*X2)
summary(mod)

# A análise deve continuar verificando os pressupostos de normalidade, 
# independência e homogeneidade. 


# Forma mais rápida (ainda com o procedimento forward)

modelo0=lm(Y~1)
summary(modelo0) # modelo só com a média (beta0)

add1(modelo0, scope=~X1+X2+X3+X4+X5,test="F") # X1 entra no modelo

modelo1=lm(Y~X1)
add1(modelo1, scope=~X1+X2+X3+X4+X5,test="F") # X2 entra no modelo a 10%

modelo2=lm(Y~X1+X2)
add1(modelo2, scope=~X1+X2+X3+X4+X5,test="F") # X3 entra no modelo

modelo3=lm(Y~X1+X2+X3)
add1(modelo3, scope=~X1+X2+X3+X4+X5,test="F") # X4 e X5 não entram no modelo


# modelo final: modelo=lm(Y~X1+X2+X3)

# Verificar pressupostos ...


# Análise do modelo final

modelo=lm(Y~X1+X2+X3)
summary(modelo)

# pressupostos

shapiro.test(residuals(modelo))
library(lmtest)
dwtest(modelo) # o pressuposto de independência é rejeitado (os resíduos são correlacionados)

plot(residuals(modelo))
plot(predict(modelo),residuals(modelo))
plot(X1,residuals(modelo))
plot(X2,residuals(modelo))
plot(X3,residuals(modelo))

modelo2=lm(Y~X1+X2+X3+I(X3^2))
shapiro.test(residuals(modelo2))
dwtest(modelo2)
bptest(modelo2) # o pressuposto de homogeneidade é rejeitado

# Tentativa
plot(predict(modelo),residuals(modelo2))
plot(X1,residuals(modelo2))
plot(X2,residuals(modelo2))
plot(X3,residuals(modelo2))

# tentativa
modelo3=lm(Y~X1+X2+X3+I(X1^2)+I(X3^2))
shapiro.test(residuals(modelo3))
dwtest(modelo3)
bptest(modelo3) 
# Todos os pressupostos s?o atendidos a 5% de signific?ncia


# mas, se considerar que os valores p estão "muito no limite"
# dando margem para rejeião dos pressupostos a 10%, por exemplo,
# continue com as tentativas

# outra tentativa (Não obrigatória)
modelo3=lm(Y~X1+X2+X3+I(X1^2)+I(X2^2)+I(X3^2))
shapiro.test(residuals(modelo3))
dwtest(modelo3)
bptest(modelo3) 

# exploração gráfica dos resíduos
plot(residuals(modelo3))
plot(predict(modelo3),residuals(modelo3))

# explora??o da normalidade graficamente
qqnorm(residuals(modelo3)) # quanto mais próximo da bissetriz do primeiro
# e terceiro quadrantes melhor 
qqline(residuals(modelo3))

qqnorm(residuals(modelo3),ylim=c(-2.2,2.2)) #
qqline(residuals(modelo3))
