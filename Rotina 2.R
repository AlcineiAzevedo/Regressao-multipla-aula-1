remove(list=ls())
setwd("D:/Backup Pendrive/UFMG/Disciplinas/Tópicos Avançados em Estatística experimental/2021/Pratica 1")
D=read.table("Dados1.txt",head=TRUE)

Y=D$Area
X=cbind(1,D$Comp,D$Comp^2,D$Larg)
B=solve(t(X)%*%X)%*%t(X)%*%Y
B

#Obtendo a correcao
C=sum(Y)^2/350

#Soma de quadrados da regressão
Yp=X%*%B
SQreg=t(Yp)%*%Yp-C

#Soma de quadrados total
SQtotal=t(Y)%*%Y-C

#Soma de quadros do resíduo
SQres=SQtotal-SQreg

#Soma de quadrados do efeito linear de X
X=cbind(1,D$Comp)
B=solve(t(X)%*%X)%*%t(X)%*%Y
B
Yp=X%*%B
SQ1c=t(Yp)%*%Yp-C
SQELx=SQ1c


#Soma de quadrados do efeito quadratico de X
X=cbind(1,D$Comp,D$Comp^2)
B=solve(t(X)%*%X)%*%t(X)%*%Y
B
Yp=X%*%B
SQ2c=t(Yp)%*%Yp-C
SQEQx=SQ2c-SQ1c


#Soma de quadrados do efeito Linear de Y
X=cbind(1,D$Comp,D$Comp^2,D$Larg)
B=solve(t(X)%*%X)%*%t(X)%*%Y
B
Yp=X%*%B
SQ3c=t(Yp)%*%Yp-C
SQELy=SQ3c-SQ2c

GL=c(3,1,1,1,346,349)
SQ=c(SQreg,SQELx,SQEQx,SQELy,SQres,SQtotal)
QM=SQ/GL

cbind(GL,SQ,QM)

########################################
#############################
m=lm(Area~Comp+I(Comp^2)+Larg,data = D)
anova(m)
summary(m)


###################################################
####################################
###
Y=D$Area
X=cbind(1,D$Comp,D$Comp^2,D$Larg)
B=solve(t(X)%*%X)%*%t(X)%*%Y
B

Var=solve(t(X)%*%X)*0.4491
VarCoef=diag(Var)
sqrt(VarCoef)

Tc=B/sqrt(VarCoef)

(1-pt(abs(Tc),346))*2
