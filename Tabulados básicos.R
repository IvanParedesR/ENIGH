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

