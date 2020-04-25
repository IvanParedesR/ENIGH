##################################################################

#Tabulados b?sicos

##################################################################

#Todas las bases de datos del Modelo Estad?stico 2016 para la continuidad del MCS-ENIGH pueden ser obtenidas en la p?gina de 
#

library(foreign)
library(car)
library(doBy)
library(reshape)
library(data.table)
library(stats)

rm(list = ls())

#Cargamos la base de hogares
hogares<- read.dbf('~/ENIGH/hogares.dbf',as.is = TRUE)

#Se ordena por folioviv
hogares <- orderBy(~+folioviv, data=hogares)

#cargamos vivienda
vivienda <- read.dbf('~/ENIGH/viviendas.dbf',as.is = TRUE)

#unimos bases de datos por medio del folio
hogares2 = merge(hogares, vivienda,by=c( "folioviv"), all.x = TRUE)
names(hogares2) =  tolower(names(hogares2))

#ordenamos la nueva base con folioviv
hogares2 <- orderBy(~+folioviv, data=hogares2)

#volvemos la variable numerica
hogares2$folioviv <- as.numeric(hogares2$folioviv)

#generamos una variable de entidad
hogares2$ent=substr(10000000000 + hogares2$folioviv,2,3)

hogares2$vivienda_ind = (hogares2$tipo_viv=1)
# Nacional

#### tabla de huespedes
tabstat_sum=as.data.frame(matrix(0,nr=1,nc=6))
names(tabstat_sum)[1:6]=cbind("huespedes 0","huespedes 1","huespedes 2","huespedes 3","huespedes 4","huespedes 5")

tabstat_sum[1,1]=sum(hogares2$factor[hogares2$huespedes==0], na.rm=TRUE)
tabstat_sum[1,2]=sum(hogares2$factor[hogares2$huespedes==1], na.rm=TRUE)
tabstat_sum[1,3]=sum(hogares2$factor[hogares2$huespedes==2], na.rm=TRUE)
tabstat_sum[1,4]=sum(hogares2$factor[hogares2$huespedes==3], na.rm=TRUE)
tabstat_sum[1,5]=sum(hogares2$factor[hogares2$huespedes==4], na.rm=TRUE)
tabstat_sum[1,6]=sum(hogares2$factor[hogares2$huespedes==5], na.rm=TRUE)

tabstat_sum

#Estatal 

tabstat_sum=as.data.frame(matrix(0,nr=33,nc=16))

names(tabstat_sum)[1:16]=cbind("pobreza","pobreza_m","pobreza_e","vul_car","vul_ing","no_pobv","carencias","carencias3",
                               "ic_rezedu","ic_asalud","ic_segsoc","ic_cev","ic_sbv","ic_ali", "plb_m","plb")

row.names(tabstat_sum)[1:33]=cbind("Aguascalientes","Baja California","Baja California Sur","Campeche","Coahuila","Colima"
                                   ,"Chiapas","Chihuahua","Ciudad de México","Durango","Guanajuato","Guerrero","Hidalgo","Jalisco",
                                   "México","Michoacán","Morelos","Nayarit","Nuevo León","Oaxaca","Puebla","Querétaro","Quintana Roo",
                                   "San Luis Potosí","Sinaloa","Sonora","Tabasco","Tamaulipas","Tlaxcala","Veracruz","Yucatán","Zacatecas","NACIONAL")
# NACIONAL