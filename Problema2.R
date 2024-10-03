#regresió lineal
mis_dades<- iris

#fer un gràfic primer
x<- iris$Petal.Length
y<- iris$Sepal.Length
plot(x,y)

#per fer la recta cal derivar
med_x<- mean(x)
med_y<- mean(y)
m<-sum((x-med_x)*(y-med_y))/sum((x-med_x)^2)
b<- med_y - m*med_x

#linia de regressió
x_pred<- x
y_pred<- m*x_pred+b
plot(x,y)
lines(x_pred,y_pred)

#coeficient de determinació
Rsq<- sum((y_pred-med_y)^2)/sum((y-med_y)^2)
cor<- sqrt(Rsq)
cor.test(x,y) #coef. correlació amb R

#per fer-ho amb R
mod<-lm(y~x)
summary(mod)
#intercapt 1r és b
#x és el pendent

#predicció de y
y_pred2<-predict(mod,data.frame(x=x))
#per m=1,5
y_pred2<-predict(mod,data.frame(x=1.5))
