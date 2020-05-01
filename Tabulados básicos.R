##################################################################

#Tabulados basicos

##################################################################

#Todas las bases de datos del Modelo Estad?stico 2016 para la continuidad del ENIGH pueden ser obtenidas en la pagina del INEGI

#librerias necesarias
library(foreign)
library(car)
library(doBy)
library(reshape)
library(data.table)
library(stats)
library(survey) 
# opción para tratar los casos de los estratos con una sola una UPM
options(survey.lonely.psu="adjust")

# limpiamos R de bases previas y preparamos espacio de trabajo.
rm(list = ls())

#Cargamos la base de hogares
hogares<- read.dbf('~/ENIGH/hogares.dbf',as.is = TRUE)

#Se ordena por folioviv
hogares <- orderBy(~+folioviv, data=hogares)

#cargamos vivienda ya que ahí esta el factor de expansión
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

# 1.1 VIVIENDAS POR ENTIDAD FEDERATIVA, SEGÚN TIPO DE VIVIENDA 				

Entidades<-c("Estados Unidos Mexicanos", "Aguascalientes", "Baja California", "Baja California Sur", "Campeche", "Coahuila de Zaragoza", "Colima", "Chiapas", "Chihuahua", "Ciudad de México", "Durango", "Guanajuato", "Guerrero", "Hidalgo", "Jalisco", "Estado de México", "Michoacán de Ocampo", "Morelos", "Nayarit", "Nuevo León", "Oaxaca", "Puebla", "Querétaro", "Quintana Roo", "San Luis Potosí", "Sinaloa", "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz de Ignacio de la Llave", "Yucatán", "Zacatecas")

# se crea una bandera para numerar a los hogares
hogares2$Nhog <- 1

#volvemos la variable numerica
hogares2$acc_alim1 <- as.numeric(hogares2$acc_alim1)

#se carga el diseño muestral
mydesign <- svydesign(id=~upm,strata=~est_dis,data=hogares2,weights=~factor)

######## HOGARES QUE EN LOS ÚLTIMOS TRES MESES EXPERIMENTARON DIFICULTADES PARA SATISFACER SUS NECESIDADES ALIMENTARIAS, 
######## POR FALTA DE DINERO O RECURSOS* POR ENTIDAD FEDERATIVA,  SEGÚN TIPO DE DIFICULTAD

M_acc1 <-svytotal(~acc_alim1 ==1, mydesign)#Total promedio
M_acc1Ent <- svyby(~acc_alim1==1,by=~ent,mydesign,svytotal) # Estatal promedio

M_acc2 <-svytotal(~acc_alim2 ==1, mydesign)#Total promedio
M_acc2Ent <- svyby(~acc_alim2==1,by=~ent,mydesign,svytotal) # Estatal promedio

M_acc3 <-svytotal(~acc_alim3 ==1, mydesign)#Total promedio
M_acc3Ent <- svyby(~acc_alim3==1,by=~ent,mydesign,svytotal) # Estatal promedio

M_acc4 <-svytotal(~acc_alim4 ==2, mydesign)#Total promedio
M_acc4Ent <- svyby(~acc_alim4==2,by=~ent,mydesign,svytotal) # Estatal promedio

M_acc5 <-svytotal(~acc_alim5 ==2, mydesign)#Total promedio
M_acc5Ent <- svyby(~acc_alim5==2,by=~ent,mydesign,svytotal) # Estatal promedio

M_acc6 <-svytotal(~acc_alim6 ==2, mydesign)#Total promedio
M_acc6Ent <- svyby(~acc_alim6 ==2,by=~ent,mydesign,svytotal) # Estatal promedio

#M_accTo <- svytotal(~Nhog, mydesign) # Total


ES_M_acc1 <- M_acc[[1]]
ES_M_acc1Ent <- M_accEnt [[2]]

ES_M_acc2 <- M_acc[[1]]
ES_M_acc2Ent <- M_accEnt [[2]]

ES_M_acc3 <- M_acc[[1]]
ES_M_acc3Ent <- M_accEnt [[2]]

ES_M_acc4 <- M_acc[[1]]
ES_M_acc4Ent <- M_accEnt [[2]]

ES_M_acc5 <- M_acc[[1]]
ES_M_acc5Ent <- M_accEnt [[2]]

ES_M_acc6 <- M_acc[[1]]
ES_M_acc6Ent <- M_accEnt [[2]]

# Creamos la base a mostrar
c_ent_ES <- data.frame(c(ES_M_acc1 ,ES_M_acc1Ent), c(ES_M_acc2 ,ES_M_acc2Ent), c(ES_M_acc3 ,ES_M_acc3Ent), c(ES_M_acc4 ,ES_M_acc4Ent), c(ES_M_acc5 ,ES_M_acc5Ent), c(ES_M_acc6 ,ES_M_acc6Ent))
# Agregamos nombres
row.names(c_ent_ES)<- Entidades
c_ent_ES

###################################
# Nacional

tabstat_sum=as.data.frame(matrix(0,nr=1,nc=6))
names(tabstat_sum)[1:6]=cbind("0 huespedes","1 huesped","2 huespedes","3 huespedes","4 huespedes","5 huespedes")

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
       function(vivienda)sum(vivienda$factor[vivienda$tipo_viv==1], na.rm=TRUE))
tabstat_sum[1,5]=sum(vivienda$factor[vivienda$tipo_viv==5], na.rm=TRUE)

for(i in 1:32){
  tabstat_mean[i,1]=plb[[i]][1]
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

##### PRUEBA COMPLETA

#"HOGARES QUE EN LOS ÚLTIMOS TRES MESES EXPERIMENTARON DIFICULTADES PARA SATISFACER SUS NECESIDADES ALIMENTARIAS, 
#POR FALTA DE DINERO O RECURSOS* POR ENTIDAD FEDERATIVA,  SEGÚN TIPO DE DIFICULTAD"																						
  
# limpiamos R de bases previas y preparamos espacio de trabajo.
rm(list = ls())
  
#Cargamos la base de hogares
hogares<- read.dbf('~/ENIGH/hogares.dbf',as.is = TRUE)
  
#Se ordena por folioviv
hogares <- orderBy(~+folioviv, data=hogares)

#ordenamos la nueva base con folioviv
hogares <- orderBy(~+folioviv, data=hogares)

#volvemos la variable numerica
hogares$folioviv <- as.numeric(hogares$folioviv)

#generamos una variable de entidad
hogares$ent=substr(10000000000 + hogares$folioviv,2,3)

acc_alim=by(hogares, hogares[ ,"ent"], 
       function(hogares)sum(hogares$factor[hogares$acc_alim1==1], na.rm=TRUE))

tabstat_sum=as.data.frame(matrix(0,nr=33,nc=1))

names(tabstat_sum)[1:1]=cbind("acc_alim1")

row.names(tabstat_sum)[1:33]=cbind("Aguascalientes","Baja California","Baja California Sur","Campeche","Coahuila","Colima"
                                   ,"Chiapas","Chihuahua","Ciudad de México","Durango","Guanajuato","Guerrero","Hidalgo","Jalisco",
                                   "México","Michoacán","Morelos","Nayarit","Nuevo León","Oaxaca","Puebla","Querétaro","Quintana Roo",
                                   "San Luis Potosí","Sinaloa","Sonora","Tabasco","Tamaulipas","Tlaxcala","Veracruz","Yucatán","Zacatecas","NACIONAL")
# NACIONAL

hogares$acc_alim1 <- as.numeric(hogares$acc_alim1)

tabstat_sum=sum(hogares2$factor[hogares2$acc_alim1==1], na.rm=TRUE)

tabstat_sum

for(i in 1:32){
  tabstat_sum[i,1]=acc_alim[[i]][1]
}

tabstat_sum