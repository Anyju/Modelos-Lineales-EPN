library(readxl)
datarls <- read_excel("data_poblacion.xlsx",sheet = 1,col_names = TRUE,na = "")
str(datarls)
View(datarls)
names(datarls)
summary(datarls)
list.files()

#familia de funciones apply
#sapply
class(datarls[ ,9])

clase <- sapply(datarls, class)
datanum <- datarls[,clase=="numeric"]
str(datanum)
View(datanum)

reg0 <- lm(poblacion~., datanum)
summary(reg0)

reg <- step(reg0, direction = "backward")#step sirve para seleccionar la mejor de las regresiones anteriores
summary (reg)
