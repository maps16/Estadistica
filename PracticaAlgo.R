library(MASS)
library(marg)

#DATOS
venice
vAno = venice$year
vMar = venice$sea
x = vMar

plot(vAno , vMar , "o" )
hist(vMar)

n=length(vMar)
sumvMar = sum(vMar)

#EMV
mMar = sumvMar/n
sd2Mar = (1/n)*sum((vMar-mMar)**2)
sdMar = sd2Mar**(1/2)

#MODELO
hist(vMar , freq = FALSE)
curve(dnorm(x, mean = mMar, sd = sdMar), col = "royalblue2", lty = 2, lwd = 2, add = TRUE)


