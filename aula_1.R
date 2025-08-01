X <- matrix(c(1,1,1,1,0,1,2,3,3,1,2,0),4,3)
y <- c(-4,5,4,11) 

b <- solve(t(X)%*%X)%*%t(X)%*%y

SQTotal <- sum((y-mean(y))^2)

y_est <- X%*%b

SQRes <- sum((y-y_est)^2)

SQReg <- SQTotal - SQRes


# e)

var_b2 <- t(c(0,0,1))%*%solve(t(X)%*%X)%*%c(0,0,1)*(SQRes/1)

t <- (-3-4)/sqrt(var_b2)

qt(0.025,1);qt(0.975,1)

p <- 2*min(pt(t,1), 1 - pt(t,1)) # ou p-valor

# f)

A <- rbind(c(0,1,0),c(0,0,1))
ABeta <- c(-2,4)

Ab <- A%*%b

num <- t(Ab- ABeta)%*%solve(A%*%solve(t(X)%*%X)%*%t(A))%*%(Ab - ABeta)

den <- (SQRes/1)*2

F <- num/den

1 - pf(F,2,1) 

qf(0.95,2,1) # Ou quartil tabela F

# Região de confiânça 

A <- rbind(c(0,1,1),c(0,0,1))
A%*%b

# h)

# H_0: beta_1 = beta_2 ou beta_1 - beta_2 = 0
# H_a: beta_1 < beta_2 ou beta_1 - beta_2 < 0

A <- rbind(c(0,1,-1))
ABeta <- 0
Ab <- A%*%b
var_Ab <- A%*%solve(t(X)%*%X)%*%t(A)*(SQRes/1)

t <- (Ab-0)/sqrt(var_Ab)

1 - pt(t,1)

# i)

xh <- c(1, 2.5, 2.5)

yh <- xh%*%b

QMRes <- SQRes/1

var_yh <- t(xh)%*%solve(t(X)%*%X)%*%xh*QMRes

LI <- yh - qt(0.95,1)*sqrt(var_yh)
LS <- yh + qt(0.95,1)*sqrt(var_yh)

LI;LS

# j)

Y_hat <- X%*%b
cor(y, Y_hat)^2















