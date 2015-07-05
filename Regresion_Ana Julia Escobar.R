
dir()
library(readxl)
ls("package:readxl")
excel_sheets("data_rls_uti.xlsx")
data<-read_excel("data_rls_uti.xlsx",sheet =1,col_names = TRUE, na="")
str(data)
View(data)
nrow(data)
regresion<-lm(Utilidad ~ Ventas,data)
summary(regresion)
anova<-aov(regresion)
summary(anova)
#calcular el fractil de 0.025 con 38 grados de libertad
qt(0.975,df=38)
#intervalos de confianza
confint(regresion,level=0.95)
(a<-mean(data[,"Utilidad"]))
(b<-mean(data[,"Ventas"]))
names(regresion)
res<-regresion[["residuals"]]
(predicciones<-regresion[["fitted.values"]])
data2<-data.frame(data,predicciones,res)
View(data2)
hist(res,20)
mean(res)
data3<-data.frame(res)
View(data3)
qqnorm(res)
qqline(res,col="red")
plot(res,predicciones)
plot(data[,1],data[,2])
####datos centrados######
utilidad1<-data[,"Utilidad"]-a
ventas1<-data[,"Ventas"]-b



regresion1<-lm(utilidad1 ~ ventas1)
summary(regresion1)
anova1<-aov(regresion1)
summary(anova1)
confint(regresion1,level=0.95)
res1<-regresion1[["residuals"]]
(predicciones1<-regresion1[["fitted.values"]])
data4<-data.frame(utilidad1,ventas1,predicciones1,res1)
View(data4)
hist(res1,20)
mean(res1)
data5<-data.frame(res1)
View(data5)
qqnorm(res1)
qqline(res1,col="red")
plot(res1,predicciones1)
plot(utilidad1,ventas1)
####
