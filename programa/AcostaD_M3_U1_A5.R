## Daniel Acosta Chávez
## Diplomado en Estadística Migratoria
## Módulo 3 Unidad 1 Actividad 3
# Estimación del Índice absoluto de intensidad migratoria (IAIM)

## Limpiar 
rm(list=ls())

#########################################
#########################################

##Lista de paquetes
install.packages(c("tidyverse","psych","psych"))
library("tidyverse")
library("readxl")
library("psych")


## Importar base de datos
setwd("F:\\repositorio\\migracion_conapo")
IMM <- read.csv("datos\\IIM_datos.csv", header=TRUE, encoding = "UTF-8")

## Explorar base
head(IMM)
str(IMM)

####################################################
################### PARTE 1 ########################
####################################################

# Definir matriz de datos
matrix <- as.matrix(IMM[,4:7])

# Matriz de correlaciones
cor<-as.matrix(cor(matrix));cor

# Matriz de datos estandarizados
scale<-scale(as.matrix(IMM[,4:7]),center=TRUE,scale=TRUE)

## Coeficientes de los componentes
pca <- principal(cor,nfactors = 1, rotate="none")
b<-c(pca[["weights"]]);b

# Porcentaje de varianza explicada
pca[["values"]] / sum(pca[["values"]])*100

## Indice de intensidad migratoria
## Estimaci?n con variables estandarizadas
IMM<-IMM%>% mutate(iim=scale%*%b)

## Explorar resultados
IMM%>%select(entidad,iim)

####################################################
################### PARTE 2 ########################
####################################################

## IIM reescalado
mean<-as.matrix(c(sapply(IMM[,4:7],mean)));mean
mean<-round(mean,2);mean
sd<-as.matrix(c(sapply(IMM[,4:7],sd)));sd
sd<-round(sd,2);sd

c<-as.matrix(round(b,10));c

## IIM Nulo
nulo1<-((0-mean[1,])/sd[1,])
nulo2<-((0-mean[2,])/sd[2,])
nulo3<-((0-mean[3,])/sd[3,])
nulo4<-((0-mean[4,])/sd[4,])

iim_nulo<-c(nulo1*c[1,]+nulo2*c[2,]+nulo3*c[3,]+nulo4*c[4,]);iim_nulo

## IIM Máximo
max1<-((100-mean[1,])/sd[1,]);max1
max2<-((100-mean[2,])/sd[2,]);max2
max3<-((100-mean[3,])/sd[3,]);max3
max4<-((100-mean[4,])/sd[4,]);max4

iim_max<-c(max1*c[1,]+max2*c[2,]+max3*c[3,]+max4*c[4,]);iim_max

IMM<-IMM%>%mutate(IMM,reescalado=((iim-iim_nulo) /(iim_max-iim_nulo)*100))

## Explorar resultados
IMM%>%select(entidad,iim,reescalado)

####################################################
################### PARTE 3 ########################
####################################################

## Índice Absoluto de Intensidad Migratoria
IMM<-IMM%>%mutate(iaim=(remesas+emigrantes+circulares+retorno)/4)

## Explorar resultados
IMM%>%select(entidad,iim,reescalado,iaim)




