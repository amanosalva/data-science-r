rm(list=ls())
##########################################################################
##### -- Programa de Especialización en Data Science - Nivel II -- #######
##########################################################################
######## Tema : KOHONEN #######################################
######## Autores: Jose Cardenas - Andre Chavez  ########################## 
##########################################################################

#### -- 1) Librerias a usar ####
library(kohonen)

# Se calcula una distancia ponderada sobre todas las capas 
# para determinar las unidades ganadoras durante el entrenamiento. 
# Las funciones som y xyf son simplemente envolturas para superestructuras 
# con una y dos capas, respectivamente

#### -- 2) cargar la data ####
#USO DE LA FUNCI�N SOM DE VINOS
datos=read.table("VINOS.txt",header=T)
head(datos,3)

# estandarizamos
set.seed(7)
vinos.sc=scale(datos[,1:13])

#definimos la forma y el numero de neuronas
vino.som=som(vinos.sc, grid = somgrid(5,5,"hexagonal"))
names(vino.som)
summary(vino.som)
vino.som$unit.classif #unidades ganadoras para todos los objetos de datos
vino.som$codes #una lista de matrices que contienen vectores de libro de c�digos.
plot(vino.som, main="Datos de vino")


#USO DE LA FUNCI�N xyf DE VINOS
attach(datos)
set.seed(7)
kohmap = xyf(vinos.sc, classvec2classmat(clase),grid = somgrid(5, 5, "hexagonal"), rlen=100)
plot(kohmap,type="codes",main=c("Distribuci�n de variables","Clases de c�digos"))
plot(kohmap,type="mapping",,col=clase+1,main="Mapa de clases")
plot(kohmap,type="mapping",labels=clase,col=clase+1,main="Mapa de clases")
plot(kohmap,type="counts",main="Diagrama de conteos")
plot(kohmap,type="quality",labels=clase,col=clase+1,main="Mapa de calidad")





