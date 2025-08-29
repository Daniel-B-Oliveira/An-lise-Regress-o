
rm(list=ls())

############## forward; backaward e stepwise com AIC

# Dados: Indicadores socioeconômicos e de fertilidade
# da Suíça (1888)

data(swiss)

names(swiss)
help("swiss")
attach(swiss)

head(swiss)


# Método FORWARD (PASSO A FRENTE)

## Primeira variável - Utilizando a função add1
add1(lm(Fertility ~1),scope=~Education + Catholic +Infant.Mortality+
               Agriculture+Examination,data = swiss)

# Conferindo
extractAIC(lm(Fertility ~1)) # AIC modelo nulo (AIC de referência)
extractAIC(lm(Fertility ~ Education)) # AIC modelo só com Education

extractAIC(lm(Fertility ~Education))
#ou
modelo=lm(Fertility ~Education)
n=nrow(swiss)
n*log(sum(resid(modelo)^2)/n) + 2*2

extractAIC(lm(Fertility ~Catholic))
modelo=lm(Fertility ~Catholic)
n=nrow(swiss)
n*log(sum(resid(modelo)^2)/n) + 2*2


extractAIC(lm(Fertility ~ Infant.Mortality))
extractAIC(lm(Fertility ~ Agriculture))
extractAIC(lm(Fertility ~ Examination))

# Variável selecionada: Education


## Segunda - Utilizando a função add1
add1(lm(Fertility ~Education),scope=~Education + Catholic +Infant.Mortality+
       Agriculture+Examination,data = swiss)

# Conferindo - segunda variável a 
# ser inserida no modelo
extractAIC(lm(Fertility ~ Education)) # AIC de referência
extractAIC(lm(Fertility ~ Education + Catholic ))
#ou
modelo=lm(Fertility ~ Education + Catholic )
n*log(sum(resid(modelo)^2)/n) + 2*3

extractAIC(lm(Fertility ~ Education + Infant.Mortality ))
extractAIC(lm(Fertility ~ Education + Agriculture ))
extractAIC(lm(Fertility ~ Education + Examination))

# Variável selecionada: catholic



## Terceira - Utilizando a função add1
add1(lm(Fertility ~Education+Catholic),scope=~Education + Catholic +Infant.Mortality+
       Agriculture+Examination,data = swiss)

# Conferindo 
extractAIC(lm(Fertility ~ Education + Catholic )) # AIC de referência

extractAIC(lm(Fertility ~ Education + Catholic +Infant.Mortality))
#ou
modelo=lm(Fertility ~ Education + Catholic +Infant.Mortality )
n*log(sum(resid(modelo)^2)/n) + 2*4

extractAIC(lm(Fertility ~ Education + Catholic +Agriculture))
extractAIC(lm(Fertility ~ Education + Catholic + Examination))

# Infant.Mortality enta no modelo.



## Quarta - Utilizando a função add1
add1(lm(Fertility ~Education+Catholic +Infant.Mortality),scope=~Education + Catholic +Infant.Mortality+
       Agriculture+Examination,data = swiss)

# Conferindo - quarta variável a 
# ser inserida no modelo
extractAIC(lm(Fertility ~ Education + Catholic + Infant.Mortality )) # AIC de referência

extractAIC(lm(Fertility ~ Education + Catholic +Infant.Mortality + Agriculture))
#ou
modelo=lm(Fertility ~ Education + Catholic +Infant.Mortality + Agriculture )
n*log(sum(resid(modelo)^2)/n) + 2*5

extractAIC(lm(Fertility ~ Education + Catholic + Infant.Mortality + Examination))

# Agriculture entra no modelo


## Quinta - Utilizando a função add1
add1(lm(Fertility ~Education+Catholic +Infant.Mortality+Agriculture),scope=~Education + Catholic +Infant.Mortality+
       Agriculture+Examination,data = swiss)

# Conferindo - quinta variável a 
# ser inserida no modelo
extractAIC(lm(Fertility ~ Education + Catholic + Infant.Mortality + Agriculture )) # AIC de referência

extractAIC(lm(Fertility ~ Education + Catholic +Infant.Mortality + Agriculture + Examination))
#ou
modelo=lm(Fertility ~ Education + Catholic +Infant.Mortality + Agriculture + Examination)
n*log(sum(resid(modelo)^2)/n) + 2*6

# Examination não entra no modelo AIC maior do que o AIC de referência


# Ou, de forma rápida 
lm1 <- lm(Fertility ~ 1, data = swiss)
slm1 <- step(lm1, direction="forward", 
             scope=formula(lm(Fertility ~ ., data = swiss)))

summary(slm1)

# Verificar pressupostos ...


# Método BACKAWARD (PASSO ATRÁS)

## Primeiro passo
## Primeira variável a ser retirada- Utilizando a função drop1
drop1(lm(Fertility ~ Education + Catholic +Infant.Mortality+
           Agriculture+Examination,data = swiss))

# Espera-se AIC baixo. Quanto maior o AIC assoaciado
# a uma determinada variável "mais ela faz falta" no modelo e 
# quanto menor o AIC "menos a variável faz falta no modelo"

# Examination sai do modelo

# Conferindo

extractAIC(lm(Fertility ~ Education + Catholic +Infant.Mortality + 
                Agriculture + Examination)) # AIC de referência
#ou
modelo=lm(Fertility ~ Education + Catholic +Infant.Mortality +
            Agriculture + Examination)
n*log(sum(resid(modelo)^2)/n) + 2*6

# Education sai?
extractAIC(lm(Fertility ~ Catholic +Infant.Mortality + 
                Agriculture + Examination)) # Não

# Catholic sai?
extractAIC(lm(Fertility ~  Education + Infant.Mortality + 
                Agriculture + Examination)) # Não

# Infant.Mortality  sai?
extractAIC(lm(Fertility ~ Education + Catholic + 
                Agriculture + Examination)) # Não


# Agriculture sai?
extractAIC(lm(Fertility ~ Education + Catholic +Infant.Mortality + 
                 Examination)) # Não


#  Examination sai?
extractAIC(lm(Fertility ~ Education + Catholic +Infant.Mortality + 
                Agriculture)) # Sim


# Segundo passo
drop1(lm(Fertility ~Education + Catholic +Infant.Mortality+
           Agriculture,data = swiss)) # Nenhuma sai. Processo encerrado


# Forma rápida: backaward (todos os passos)
lm2 <- lm(Fertility ~ Education + Catholic +Infant.Mortality + 
            Agriculture + Examination, data = swiss)
slm2 <- step(lm2, direction="backward")
summary(slm2)



# Método STEPWISE (PASSO A FRENTE E PASSO ATRÁS)

# Decidinto primeira variável de entrada
add1(lm(Fertility ~1),scope=~Education + Catholic +Infant.Mortality+
       Agriculture+Examination,data = swiss)

# Entra aquela com menor AIC: Education
# Ainda não tem etapa de saída

# Decidindo segunda variável de entrada
add1(lm(Fertility ~Education),scope=~Education + Catholic +Infant.Mortality+
       Agriculture+Examination,data = swiss)
# Entra aquela com menor AIC: Catholic


# Education sai?
drop1(lm(Fertility ~Education+Catholic),data = swiss)
# A saida de Education faz aumentar o AIC para além do valor
# de referência. Conclusão: Education não sai.

# Conferindo
extractAIC(lm(Fertility ~Education+Catholic)) # AIC de referência
extractAIC(lm(Fertility ~Education)) # AIC sem Catholic (contribuição de Catholic)
extractAIC(lm(Fertility ~Catholic)) # AIC sem Education


# Decidinto terceira variável de entrada
# Avalia o AIC no modelo base com cada uma
# das candidatas
add1(lm(Fertility ~Education + Catholic),scope=~Education + Catholic +Infant.Mortality+
       Agriculture+Examination,data = swiss)
# Infant.Mortality entra no modelo


# Education ou Catholic sai do modelo?
drop1(lm(Fertility ~Education+Catholic+Infant.Mortality),data = swiss)
# Ninguém sai

# Decidinto quarta variável de entrada
# Avalia o AIC no modelo base com cada uma
# das candidatas
add1(lm(Fertility ~Education + Catholic+Infant.Mortality),scope=~Education + Catholic +Infant.Mortality+
       Agriculture+Examination,data = swiss)
# Agriculture entra no modelo


# Education, Catholic ou Infant.Mortality sai do modelo?
drop1(lm(Fertility ~Education+Catholic+Infant.Mortality + Agriculture),data = swiss)
# Ninguém sai


# Decidinto quintaa variável de entrada
# Avalia o AIC no modelo base com cada uma
# das candidatas
add1(lm(Fertility ~Education + Catholic+Infant.Mortality+Agriculture),scope=~Education + Catholic +Infant.Mortality+
       Agriculture+Examination,data = swiss)
# Ninguém entra modelo

# Processo encerrado

# Forma rápida
lm3 <- lm(Fertility ~ Education + Catholic +Infant.Mortality + 
            Agriculture + Examination, data = swiss)
slm3 <- step(lm3)

summary(slm3)

