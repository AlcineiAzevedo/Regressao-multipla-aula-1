#Apagar a memoria do r
remove(list=ls())
#indicar a pasta de trabalho (Onde esta o arquivo de dados)
setwd("D:/Backup Pendrive/Pratica 1")
#Abrir o arquivo txt
D=read.table("Dados1.txt",head=TRUE)

# Zi=a+bxi+cyi+ei
#Criando as matrizes
Y=D$Area
X=cbind(1,D$Comp,D$Larg)
#Estimando os coeficientes de regressao -> Inv(x'x)X'Y
solve(t(X)%*%X)%*%t(X)%*%Y

#Estimano os coeficientes pela funcao lm
m=lm(Area~Comp+Larg,data=D)
summary(m)
# Z=-5.47+1.59x+2.52y


###################################
####################################
# Refazendo os passos anteriores com outro modelo de regressão
# # Zi=a+bxi+cyi+dxiyi+ei

Y=D$Area
X=cbind(1,D$Comp,D$Larg,D$Comp*D$Larg)

solve(t(X)%*%X)%*%t(X)%*%Y

m=lm(Area~Comp+Larg+Comp:Larg,data=D)
summary(m)


##### lm
?lm

