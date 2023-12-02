library(readxl)
library(epiDisplay)

# Leer el archivo de Excel y almacenar los datos en la variable "datos"
datos <- read_excel("C:/Users/wilca/OneDrive/Desktop/Estadistica/DISTRITO_CHARACATO_ESTADISTICA.xlsx")

variable_vivencia <- datos$"10. Tiempo que vive (años)"
str(variable_vivencia)  # numero de observaciones y tipo de variable
anios <- variable_vivencia
k<-nclass.Sturges(anios)
k
intervalos<-cut(anios,breaks = k)

FA<-table(intervalos)
FA
hist(anios)
#18
variable_interes <- datos$"18.Principal fuente de abastecimiento de agua"

# Definir las categorías y etiquetas
categorias <- c("CAMION CISTERNA", "OTRA VIVIENDA", "PILETA PUBLICA", "POZO TUBULAR")
etiquetas <- c("CAMION CISTERNA", "OTRA VIVIENDA", "PILETA PUBLICA", "POZO TUBULAR")

# Crear un factor con las etiquetas
y <- factor(variable_interes, levels = categorias, labels = etiquetas)

# Crear la tabla de frecuencia y el gráfico
#tab1(y, cum.percent = TRUE, graph = TRUE, col = c("skyblue", "pink", "orange", "yellow"), main = "PRINCIPAL FUENTE DE ABASTECIMIENTO DE AGUA")

fi=table(y)
hi = prop.table(fi)
pi= prop.table(fi)*100

tabla1 = cbind(fi, hi, pi)
tabla1

barplot(fi)

bp=barplot(fi,names=categorias,ylim=c(0,max(fi)*1.1))
text(x=bp, y=fi, labels=fi, pos=3)

