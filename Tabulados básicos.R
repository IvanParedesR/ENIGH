##################################################################

#Tabulados basicos

##################################################################

#Todas las bases de datos del Modelo Estad?stico 2016 para la continuidad del ENIGH pueden ser obtenidas en la pagina del INEGI

library(foreign)
library(car)
library(doBy)
library(reshape)
library(data.table)
library(stats)

# limpiamos la base
rm(list = ls())

#Cargamos la base de hogares
hogares<- read.dbf('~/ENIGH/hogares.dbf',as.is = TRUE)

#Se ordena por folioviv
hogares <- orderBy(~+folioviv, data=hogares)

#cargamos vivienda
vivienda <- read.dbf('~/ENIGH/viviendas.dbf',as.is = TRUE)

#unimos bases de datos por medio del folio
hogares2 = merge(hogares, vivienda,by=c( "folioviv"), all.x = TRUE)

# Convierta textos o tokens a minúsculas (o mayúsculas)
names(hogares2) =  tolower(names(hogares2))

#ordenamos la nueva base con folioviv
hogares2 <- orderBy(~+folioviv, data=hogares2)

#volvemos la variable numerica
hogares2$folioviv <- as.numeric(hogares2$folioviv)

#generamos una variable de entidad
hogares2$ent=substr(10000000000 + hogares2$folioviv,2,3)

hogares2$vivienda_ind = (hogares2$tipo_viv=1)

#### tabla de huespedes

# Nacional

tabstat_sum=as.data.frame(matrix(0,nr=1,nc=6))
names(tabstat_sum)[1:6]=cbind("huespedes 0","huespedes 1","huespedes 2","huespedes 3","huespedes 4","huespedes 5")

tabstat_sum[1,1]=sum(hogares2$factor[hogares2$huespedes==0], na.rm=TRUE)
tabstat_sum[1,2]=sum(hogares2$factor[hogares2$huespedes==1], na.rm=TRUE)
tabstat_sum[1,3]=sum(hogares2$factor[hogares2$huespedes==2], na.rm=TRUE)
tabstat_sum[1,4]=sum(hogares2$factor[hogares2$huespedes==3], na.rm=TRUE)
tabstat_sum[1,5]=sum(hogares2$factor[hogares2$huespedes==4], na.rm=TRUE)
tabstat_sum[1,6]=sum(hogares2$factor[hogares2$huespedes==5], na.rm=TRUE)

tabstat_sum

#### tipo vivienda

# Valor Etiqueta
# 1 Casa independiente
# 2 Departamento en edificio
# 3 Vivienda en vecindad
# 4 Vivienda en cuarto de azotea
# 5 Local no construido para habitación

tabstat_sum=as.data.frame(matrix(0,nr=1,nc=5))
names(tabstat_sum)[1:5]=cbind("Casa independiente","Departamento en edificio","Vivienda en vecindad","Vivienda en cuarto de azotea","Local no construido para habitación")

tabstat_sum[1,1]=sum(vivienda$factor[vivienda$tipo_viv==1], na.rm=TRUE)
tabstat_sum[1,2]=sum(vivienda$factor[vivienda$tipo_viv==2], na.rm=TRUE)
tabstat_sum[1,3]=sum(vivienda$factor[vivienda$tipo_viv==3], na.rm=TRUE)
tabstat_sum[1,4]=sum(vivienda$factor[vivienda$tipo_viv==4], na.rm=TRUE)
tabstat_sum[1,5]=sum(vivienda$factor[vivienda$tipo_viv==5], na.rm=TRUE)

tabstat_sum

#Estatal 

tabstat_sum=as.data.frame(matrix(0,nr=33,nc=5))

names(tabstat_sum)[1:5]=cbind("c_segsoc","ic_cev","ic_sbv","ic_ali", "plb_m","plb")

row.names(tabstat_sum)[1:33]=cbind("Aguascalientes","Baja California","Baja California Sur","Campeche","Coahuila","Colima"
                                   ,"Chiapas","Chihuahua","Ciudad de México","Durango","Guanajuato","Guerrero","Hidalgo","Jalisco",
                                   "México","Michoacán","Morelos","Nayarit","Nuevo León","Oaxaca","Puebla","Querétaro","Quintana Roo",
                                   "San Luis Potosí","Sinaloa","Sonora","Tabasco","Tamaulipas","Tlaxcala","Veracruz","Yucatán","Zacatecas","NACIONAL")
# NACIONAL