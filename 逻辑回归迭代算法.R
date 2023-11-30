rm(list = ls())
set.seed(123)
setwd('/Users/haoshuaima/Desktop/mhs的文件/R')
data(iris)
X1=as.matrix(subset(iris, Species == "setosa")[1:4])
X2=as.matrix(subset(iris, Species == "versicolor")[1:4])
X3=as.matrix(subset(iris, Species == "virginica")[1:4])
XX12<-rbind(X1,X2)
XX13<-rbind(X1,X3)
XX23<-rbind(X2,X3)
#散点图矩阵——Iris(GGally_ggpairs)
# library(GGally)
# ggpairs(iris, columns=1:5, aes(color=Species)) + 
#   ggtitle("matrix plot——Iris(GGally_ggpairs)")+
#   theme_bw() 

y=c(rep(1,50),rep(0,50))

lr12<-glm(y~XX12,family ='binomial')
lr13<-glm(y~XX13,family ='binomial')
lr23<-glm(y~XX23,family ='binomial')
lr12$coefficients
summary(lr12)
# lr13$coefficients
# lr23$coefficients

new_XX12<-cbind(rep(1,nrow(XX12)),XX12)
new_XX13<-cbind(rep(1,nrow(XX12)),XX13)
new_XX23<-cbind(rep(1,nrow(XX12)),XX23)


descent_fun12=function(beita){
  s=matrix(0,nrow = 5)
  for(i in 1:100){
    s=s+matrix(new_XX12[i,])*
      (1/(1+exp(-t(as.matrix(new_XX12[i,]))%*%beita))-y[i])[1,1]
  }
  return(s)
}


i=1
## 梯度下降迭代
beita0=matrix(c(-6.556265,9.878866,7.417640,-19.053588,-25.032928 ))
learn_state=0.001
beita1=beita0-learn_state*descent_fun12(beita0)
while (norm((beita1-beita0),"2")>0.0001) {
  next_top=beita1-learn_state*descent_fun12(beita1)
  beita0=beita1
  beita1=next_top
  i=i+1
}
print(beita0)
print(beita1)

hessian_fun12=function(beita){
  s=matrix(0,nrow = 5,ncol = 5)
  for(i in 1:100){
    s=s+(exp(t(as.matrix(new_XX12[i,]))%*%beita)*
            1/(1+exp(t(as.matrix(new_XX12[i,]))%*%beita))^2)[1,1]*
      as.matrix(new_XX12[i,])%*%t(as.matrix(new_XX12[i,]))
  }
  return(s)
}

descent_fun12(matrix(c(-6.556265,9.878866,7.417640,-19.053588,-25.032928 )))


## 牛顿迭代
beita2=matrix(c(-6,9,7,-19,-25 ))

beita3=beita2-solve(hessian_fun12(beita2))%*%descent_fun12(beita2)
while (norm((beita3-beita2),"2")>1e-4 ) {
  next_top=beita3-solve(hessian_fun12(beita3))%*%descent_fun12(beita3)
  beita2=beita3
  beita3=next_top
  i=i+1
}
print(beita3)
