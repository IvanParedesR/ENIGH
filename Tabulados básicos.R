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

names(tabstat_sum)[1:5]=cbind("c_segsoc","ic_cev","ic_sbv","ic_ali", "plb_m")

row.names(tabstat_sum)[1:33]=cbind("Aguascalientes","Baja California","Baja California Sur","Campeche","Coahuila","Colima"
                                   ,"Chiapas","Chihuahua","Ciudad de México","Durango","Guanajuato","Guerrero","Hidalgo","Jalisco",
                                   "México","Michoacán","Morelos","Nayarit","Nuevo León","Oaxaca","Puebla","Querétaro","Quintana Roo",
                                   "San Luis Potosí","Sinaloa","Sonora","Tabasco","Tamaulipas","Tlaxcala","Veracruz","Yucatán","Zacatecas","NACIONAL")
# NACIONAL

#ordenamos la nueva base con folioviv
vivienda <- orderBy(~+folioviv, data=vivienda)

#volvemos la variable numerica
vivienda$folioviv <- as.numeric(vivienda$folioviv)

#generamos una variable de entidad
vivienda$ent=substr(10000000000 + vivienda$folioviv,2,3)

plb=by(vivienda, vivienda[ ,"ent"], 
       function(vivienda)sum(vivienda$factor[vivienda$tipo_viv==5], na.rm=TRUE))


for(i in 1:32){
  tabstat_mean[i,1]=p.pobreza[[i]][1]
  tabstat_mean[i,2]=p.pobreza_m[[i]][1]
  tabstat_mean[i,3]=p.pobreza_e[[i]][1]
  tabstat_mean[i,4]=p.vul_car[[i]][1]
  tabstat_mean[i,5]=p.vul_ing[[i]][1]
  tabstat_mean[i,6]=p.no_pobv[[i]][1]
  tabstat_mean[i,7]=p.carencias[[i]][1]
  tabstat_mean[i,8]=p.carencias3[[i]][1]
  tabstat_mean[i,9]=p.ic_rezedu[[i]][1]
  tabstat_mean[i,10]=p.ic_asalud[[i]][1]
  tabstat_mean[i,11]=p.ic_segsoc[[i]][1]
  tabstat_mean[i,12]=p.ic_cev[[i]][1]
  tabstat_mean[i,13]=p.ic_sbv[[i]][1]
  tabstat_mean[i,14]=p.ic_ali[[i]][1]
  tabstat_mean[i,15]=p.plb_m[[i]][1]
  tabstat_mean[i,16]=p.plb[[i]][1]
}
