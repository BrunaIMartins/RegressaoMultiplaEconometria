################ REGRESS?O
setwd("C:\\Users\\Bruna Ivna\\Documents\\UFSJ\\5sem\\Econometria\\Exerc?cios e respostas\\artigo")

library(readr)

dados1<- read.csv('dadosindice.csv', head=T, dec=',',sep=';')

colnames(dados1)

PIB<-dados1$pib
Selic<-dados1$selic
C?mbio<-dados1$cambio

reg <- lm(Selic~C?mbio+PIB)
summary(reg)


########### GR?FICOS

dad <- cbind(PIB, C?mbio, Selic) 

plot(dad_ts <- ts(dad, start=c(2000,1), freq=12))

par(mfrow=c(1,3))
Dados<-dad_ts
plot(Dados)

plot(PIB)
plot(C?mbio)
plot(Selic)


library("ggplot2")


plot(PIB)
plot(Selic)
plot(C?mbio)

ggplot(dados1, aes(x=PIB, y=Selic)) +
  geom_point(shape=1) +    
  geom_smooth(method=lm) +
  xlab('PIB') +
  ylab('Selic') +
  ggtitle('Selic vs. PIB')

ggplot(dados1, aes(x=C?mbio, y=Selic)) +
  geom_point(shape=1) +    
  geom_smooth(method=lm) +
  xlab('C?mbio') +
  ylab('Selic') +
  ggtitle('Selic vs. C?mbio')


########################## TESTES
cor.test(C?mbio,Selic)

cor.test(PIB,Selic)

library(tseries)

resid <- residuals(reg)

residuals(reg)

plot(resid)

jarque.bera.test(resid)

# H0: res?duos normais
# H1: res?duos n?o normais
# se p-valor < 0,05 (5%) indica rejei??o de H0, logo res?duos n?o s?o normais


# INTERVALO DE CONFIAN?A
#bo

alpha <- 0.05 # pode escolher o n?vel de signific?ncia
b0 <- coef(reg)[[1]]
df <- df.residual(reg) # graus de liberdade (n-2)
smod0 <- summary(reg)
seb0 <- coef(smod0)[2,2] # desvio padr?o de beta 1
tc <- qt(1-alpha/2, df) # estat?stica t tabelado
lowb <- b0-tc*seb0  # limite inferior
upb <- b0+tc*seb0   # limite superior

lowb
upb

#b1
alpha <- 0.05 # pode escolher o n?vel de signific?ncia
b1 <- coef(reg)[[2]]
df <- df.residual(reg) # graus de liberdade (n-2)
smod0 <- summary(reg)
seb1 <- coef(smod0)[2,2] # desvio padr?o de beta 1
tc <- qt(1-alpha/2, df) # estat?stica t tabelado
lowb <- b1-tc*seb1  # limite inferior
upb <- b1+tc*seb1   # limite superior

lowb
upb


#b2

alpha <- 0.05 # pode escolher o n?vel de signific?ncia
b2 <- coef(reg)[[3]]
df <- df.residual(reg) # graus de liberdade (n-2)
smod2 <- summary(reg)
seb2 <- coef(smod2)[2,2] # desvio padr?o de beta 1
tc <- qt(1-alpha/2, df) # estat?stica t tabelado
lowb <- b2-tc*seb2  # limite inferior
upb <- b2+tc*seb2   # limite superior

lowb
upb


#teste homocedasticidade Breusch-Pagan-Godfrey

#H0 : modelo homoced?stico
#H1 : modelo n?o-homoced?stico (presen?a de heterocedasticidade)

install.packages("lmtest")
library("lmtest")
bptest(reg)
citation("car")

#### autocorrela??o
# se p-valor < 0,10 (10%) indica rejei??o de H0
#H0 : P1=p2=...=pn=0
#H1 : p1 diferente de zero
library(car)
bgtest(reg)  #Breusch-Godfrey
dwtest(reg)  #Durbin-Watson Test
