y <- c(4,7,6,9,3,4,6,5,3,2,4,5)
x <- rep(0:3, 3)
trat <- gl(3,4)

modelo <- lm(y~x+trat-1)
summary(modelo)

# e)
library(car)

linearHypothesis(modelo, c("trat1-0.5*trat2-0.5*trat3=0"))

# ex:
#     A + B = C + D + E
#     (1/2)A + (1/2) B -(1/3)C -(1/3)D -(1/3)E = 0

# f)

linearHypothesis(modelo, c("trat2-trat3=0", "trat1-0.5*trat2-0.5*trat3=2"))

# Questão 21 cap 5

x=c(1,3,5,7,3,5,7,9)
y=c(35,57,73,83,73,91,115,145)
z=c(0,0,0,0,1,1,1,1)

setor=c('Agr.','Agr.','Agr.','Agr.','Urb.','Urb.','Urb.','Urb.')

df=data.frame(cbind(x,y))
df$setor<-setor

summary(df)

library(ggplot2)

ggplot(df, aes(x = x, y = y, color = setor)) +
  geom_point(size = 3) +
  labs(title = "Gráfico de Dispersão",
       x = "x",
       y = "y") +
  # theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), 
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1) 
  )

modelo1 <- lm(y~x+z+setor, subset = setor)
