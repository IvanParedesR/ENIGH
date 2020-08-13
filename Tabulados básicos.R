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
Entidades<-c("Estados Unidos Mexicanos", "Aguascalientes", "Baja California", "Baja California Sur", "Campeche", "Coahuila de Zaragoza", "Colima", "Chiapas", "Chihuahua", "Ciudad de México", "Durango", "Guanajuato", "Guerrero", "Hidalgo", "Jalisco", "Estado de México", "Michoacán de Ocampo", "Morelos", "Nayarit", "Nuevo León", "Oaxaca", "Puebla", "Querétaro", "Quintana Roo", "San Luis Potosí", "Sinaloa", "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz de Ignacio de la Llave", "Yucatán", "Zacatecas")

#ordenamos la nueva base con folioviv
# se crea una bandera para numerar a los hogares
hogares2$Nhog <- 1

#volvemos la variable numerica
hogares2$acc_alim1 <- as.numeric(hogares2$acc_alim1)

#se carga el diseño muestral
mydesign <- svydesign(id=~upm,strata=~est_dis,data=hogares2,weights=~factor)

vivienda <- orderBy(~+folioviv, data=vivienda)

#volvemos la variable numerica
vivienda$folioviv <- as.numeric(vivienda$folioviv)

#generamos una variable de entidad
vivienda$ent=substr(10000000000 + vivienda$folioviv,2,3)

##################################################################
####### 1.1 VIVIENDAS POR ENTIDAD FEDERATIVA, SEGÚN TIPO DE VIVIENDA 				

Entidades<-c("Estados Unidos Mexicanos", "Aguascalientes", "Baja California", "Baja California Sur", "Campeche", "Coahuila de Zaragoza", "Colima", "Chiapas", "Chihuahua", "Ciudad de México", "Durango", "Guanajuato", "Guerrero", "Hidalgo", "Jalisco", "Estado de México", "Michoacán de Ocampo", "Morelos", "Nayarit", "Nuevo León", "Oaxaca", "Puebla", "Querétaro", "Quintana Roo", "San Luis Potosí", "Sinaloa", "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz de Ignacio de la Llave", "Yucatán", "Zacatecas")

mydesign <- svydesign(id=~upm,strata=~est_dis,data=vivienda,weights=~factor)

M_tipo_viv <-svytotal(~tipo_viv ==1, mydesign)#Total promedio
M_tipo_viv1Ent <- svyby(~tipo_viv==1,by=~ent,mydesign,svytotal, na.rm=TRUE) # Estatal promedio

M_tipo_viv2 <-svytotal(~tipo_viv ==2 | tipo_viv ==3 | tipo_viv ==4 | tipo_viv ==5, mydesign)#Total promedio
M_tipo_viv2Ent <- svyby(~tipo_viv ==2 | tipo_viv ==3 | tipo_viv ==4 | tipo_viv ==5,by=~ent,mydesign,svytotal, na.rm=TRUE) # Estatal promedio
M_tipo_viv2Ent

ES_M_tipo_viv <- M_tipo_viv[[2]]
ES_M_tipo_viv1Ent <- M_tipo_viv1Ent[[3]]

ES_M_tipo_viv2 <- M_tipo_viv2[[2]]
ES_M_tipo_viv2Ent <- M_tipo_viv2Ent[[3]]
ES_M_tipo_viv2Ent

# Creamos la base a mostrar
c_ent_ES1 <- data.frame(c(ES_M_tipo_viv ,ES_M_tipo_viv1Ent), c(ES_M_tipo_viv2 ,ES_M_tipo_viv2Ent))
# Agregamos nombres
colnames(c_ent_ES1) <- c("CASA INDEPENDIENTE", "OTRO")
row.names(c_ent_ES1)<- Entidades
c_ent_ES1

##################################################################
################ 1.2 VIVIENDAS DE TIPO INDEPENDIENTE POR ENTIDAD FEDERATIVA, SEGÚN TAMAÑO DE LOCALIDAD
M_tam_loc  <-svytotal(~tam_loc ==4 & tipo_viv==1, mydesign)#Total promedio
M_tam_locEnt <- svyby(~tam_loc ==4 & tipo_viv==1,by=~ent,mydesign,svytotal, na.rm=FALSE) # Estatal promedio

M_tam_loc2  <-svytotal(~(tam_loc ==1 | tam_loc ==2 | tam_loc ==3) & tipo_viv==1, mydesign)#Total promedio
M_tam_locEnt2 <- svyby(~(tam_loc ==1 | tam_loc ==2 | tam_loc ==3) & tipo_viv==1,by=~ent,mydesign,svytotal, na.rm=FALSE) # Estatal promedio
M_tam_locEnt2

ES_M_tam_loc <- M_tam_loc[[2]]
ES_M_tam_locEnt <- M_tam_locEnt[[3]]
ES_M_tam_locEnt 

ES_M_tam_loc2 <- M_tam_loc2[[2]]
ES_M_tam_locEnt2 <- M_tam_locEnt2[[3]]
ES_M_tam_locEnt2 
# Creamos la base a mostrar
c_ent_ES2 <- data.frame(c(ES_M_tam_loc ,ES_M_tam_locEnt), c(ES_M_tam_loc2 ,ES_M_tam_locEnt2))
# Agregamos nombres
colnames(c_ent_ES2) <- c("DE MENOS DE 2 500 HABITANTES", "DE MÁS DE 2 500 HABITANTES")
row.names(c_ent_ES2)<- Entidades
c_ent_ES2

##################################################################
#### 1.3 VIVIENDAS POR ENTIDAD FEDERATIVA, SEGÚN MATERIAL DE LAS PAREDES O MUROS							

M_mat_pared  <-svytotal(~mat_pared ==8, mydesign)#Total promedio
M_mat_paredEnt <- svyby(~mat_pared ==8,by=~ent,mydesign,svytotal, na.rm=FALSE) # Estatal promedio

M_mat_pared2  <-svytotal(~(mat_pared ==7 | mat_pared ==6 | mat_pared ==5 | mat_pared ==4 | mat_pared ==3 | mat_pared ==2 | mat_pared ==1), mydesign)#Total promedio
M_mat_paredEnt2 <- svyby(~(mat_pared ==7 | mat_pared ==6 | mat_pared ==5 | mat_pared ==4 | mat_pared ==3 | mat_pared ==2 | mat_pared ==1),by=~ent,mydesign,svytotal, na.rm=FALSE) # Estatal promedio

ES_M_mat_pared <- M_mat_pared[[1]]
ES_M_mat_paredEnt <- M_mat_paredEnt[[2]]

ES_M_mat_pared2 <- M_mat_pared2[[1]]
ES_M_mat_paredEnt2 <- M_mat_paredEnt2[[2]]

# Creamos la base a mostrar
c_ent_ES3 <- data.frame(c(ES_M_mat_pared ,ES_M_mat_paredEnt), c(ES_M_mat_pared2 ,ES_M_mat_paredEnt2))
# Agregamos nombres
colnames(c_ent_ES3) <- c("OTRO", "TABIQUE, LADRILLO, BLOCK, PIEDRA, CANTERA, CEMENTO O CONCRETO")
row.names(c_ent_ES3)<- Entidades
c_ent_ES3

##################################################################
#### 1.4 VIVIENDAS CON PAREDES DE TABIQUE, LADRILLO, BLOCK, PIEDRA, CANTERA, CEMENTO 
#O CONCRETO POR ENTIDAD FEDERATIVA, SEGÚN TAMAÑO DE LOCALIDAD"							
							
M_mat_pared_loc  <-svytotal(~tam_loc ==4 & mat_pared ==8, mydesign)#Total promedio
M_mat_pared_locEnt <- svyby(~tam_loc ==4 & mat_pared ==8,by=~ent,mydesign,svytotal, na.rm=FALSE) # Estatal promedio

M_mat_pared_loc2  <-svytotal(~(tam_loc ==1 | tam_loc ==2 | tam_loc ==3) & (mat_pared ==8), mydesign)#Total promedio
M_mat_pared_locEnt2 <- svyby(~(tam_loc ==1 | tam_loc ==2 | tam_loc ==3) & (mat_pared ==8),by=~ent,mydesign,svytotal, na.rm=FALSE) # Estatal promedio

ES_M_mat_pared_loc <- M_mat_pared_loc[[2]]
ES_M_mat_pared_locEnt <- M_mat_pared_locEnt[[3]]

ES_M_mat_pared_loc2 <- M_mat_pared_loc2[[2]]
ES_M_mat_pared_locEnt2 <- M_mat_pared_locEnt2[[3]]

# Creamos la base a mostrar
c_ent_ES4 <- data.frame(c(ES_M_mat_pared_loc ,ES_M_mat_pared_locEnt), c(ES_M_mat_pared_loc2 ,ES_M_mat_pared_locEnt2))
# Agregamos nombres
colnames(c_ent_ES4) <- c("OTRO", "TABIQUE, LADRILLO, BLOCK, PIEDRA, CANTERA, CEMENTO O CONCRETO")
row.names(c_ent_ES4)<- Entidades
c_ent_ES4


##################################################################
#### 1.5 VIVIENDAS POR ENTIDAD FEDERATIVA, SEGÚN MATERIAL DEL TECHO							

M_mat_techo  <-svytotal(~mat_techos =="10", mydesign)#Total promedio
M_mat_techoEnt <- svyby(~mat_techos =="10", by=~ent,mydesign,svytotal, na.rm=TRUE) # Estatal promedio
M_mat_techoEnt

M_mat_techo2  <-svytotal(~(mat_techos =="01" | mat_techos =="02" | mat_techos =="03" | mat_techos =="04" | mat_techos =="05" | mat_techos =="06" | mat_techos =="07" | mat_techos =="08" | mat_techos =="09"), mydesign)#Total promedio
M_mat_techoEnt2 <- svyby(~(mat_techos =="01" | mat_techos =="02" | mat_techos =="03" | mat_techos =="04" | mat_techos =="05" | mat_techos =="06" | mat_techos =="07" | mat_techos =="08" | mat_techos =="09"),by=~ent,mydesign,svytotal, na.rm=FALSE) # Estatal promedio

ES_M_mat_techo <- M_mat_techo[[2]]
ES_M_mat_techoEnt <- M_mat_techoEnt[[3]]

ES_M_mat_techo2 <- M_mat_techo2[[2]]
ES_M_mat_techoEnt2 <- M_mat_techoEnt2[[3]]

# Creamos la base a mostrar
c_ent_ES5 <- data.frame(c(ES_M_mat_techo ,ES_M_mat_techoEnt), c(ES_M_mat_techo2 ,ES_M_mat_techoEnt2))
# Agregamos nombres
colnames(c_ent_ES5) <- c("OTRO", "TABIQUE, LADRILLO, BLOCK, PIEDRA, CANTERA, CEMENTO O CONCRETO")
row.names(c_ent_ES5)<- Entidades
c_ent_ES5

##################################################################
#### 1.6 VIVIENDAS CON MATERIAL DE LOSA DE CONCRETO O VIGUETA CON BOVEDILLA 
#EN EL TECHO POR ENTIDAD FEDERATIVA, SEGÚN TAMAÑO DE LOCALIDAD						

M_mat_techoloc  <-svytotal(~tam_loc ==4 & mat_techos =="10", mydesign)#Total promedio
M_mat_techolocEnt <- svyby(~tam_loc ==4 & mat_techos =="10", by=~ent,mydesign,svytotal, na.rm=FALSE) # Estatal promedio

M_mat_techoloc2  <-svytotal(~(tam_loc ==1 | tam_loc ==2 | tam_loc ==3) & mat_techos =="10", mydesign)#Total promedio
M_mat_techolocEnt2 <- svyby(~(tam_loc ==1 | tam_loc ==2 | tam_loc ==3) & mat_techos =="10",by=~ent,mydesign,svytotal, na.rm=FALSE) # Estatal promedio


ES_M_mat_techoloc <- M_mat_techoloc[[2]]
ES_M_mat_techolocEnt <- M_mat_techolocEnt[[3]]

ES_M_mat_techoloc2 <- M_mat_techoloc2[[2]]
ES_M_mat_techolocEnt2 <- M_mat_techolocEnt2[[3]]
ES_M_mat_techolocEnt2

# Creamos la base a mostrar
c_ent_ES6 <- data.frame(c(ES_M_mat_techoloc ,ES_M_mat_techolocEnt), c(ES_M_mat_techoloc2 ,ES_M_mat_techolocEnt2))
# Agregamos nombres
colnames(c_ent_ES6) <- c("DE MENOS DE 2 500 HABITANTES", "DE MÁS DE 2 500 HABITANTES")
row.names(c_ent_ES6)<- Entidades
c_ent_ES6

##################################################################
#### 1.7 VIVIENDAS POR ENTIDAD FEDERATIVA, SEGÚN MATERIAL DEL PISO DE LA VIVIENDA										

M_mat_pisoloc  <-svytotal(~mat_pisos=="3", mydesign)#Total promedio
M_mat_pisolocEnt <- svyby(~mat_pisos=="3", by=~ent,mydesign,svytotal, na.rm=FALSE) # Estatal promedio

M_mat_pisoloc2  <-svytotal(~mat_pisos=="2", mydesign)#Total promedio
M_mat_pisolocEnt2 <- svyby(~mat_pisos=="2",by=~ent,mydesign,svytotal, na.rm=FALSE) # Estatal promedio

M_mat_pisoloc3  <-svytotal(~mat_pisos=="1", mydesign)#Total promedio
M_mat_pisolocEnt3 <- svyby(~mat_pisos=="1",by=~ent,mydesign,svytotal, na.rm=FALSE) # Estatal promedio


ES_M_mat_pisoloc <- M_mat_pisoloc[[2]]
ES_M_mat_pisolocEnt <- M_mat_pisolocEnt[[3]]

ES_M_mat_pisoloc2 <- M_mat_pisoloc2[[2]]
ES_M_mat_pisolocEnt2 <- M_mat_pisolocEnt2[[3]]

ES_M_mat_pisoloc3 <- M_mat_pisoloc3[[2]]
ES_M_mat_pisolocEnt3 <- M_mat_pisolocEnt3[[3]]

# Creamos la base a mostrar
c_ent_ES7 <- data.frame(c(ES_M_mat_pisoloc ,ES_M_mat_pisolocEnt), c(ES_M_mat_pisoloc2 ,ES_M_mat_pisolocEnt2), c(ES_M_mat_pisoloc3 ,ES_M_mat_pisolocEnt3))
# Agregamos nombres
colnames(c_ent_ES7) <- c("MADERA, MOSAICO U OTRO RECUBRIMIENTO", "CEMENTO O FIRME", "TIERRA")
row.names(c_ent_ES7)<- Entidades
c_ent_ES7
##################################################################
#### 1.8 VIVIENDAS POR ENTIDAD FEDERATIVA, SEGÚN ANTIGÜEDAD DE LA VIVIENDA									
### generamos variables variables

# modificamos el data frame para separar los datos por sexenio
vivienda$antiguedad_1<- ifelse((vivienda$antiguedad >= 0 & vivienda$antiguedad  <= 5), 1,+
                  ifelse((vivienda$antiguedad >= 6 & vivienda$antiguedad <= 10),  2,+
                           ifelse((vivienda$antiguedad  >= 11 & vivienda$antiguedad  <= 15),  3,+
                                    ifelse((vivienda$antiguedad  >= 16 & vivienda$antiguedad <= 20), 4,+
                                             ifelse((vivienda$antiguedad >= 21 & vivienda$antiguedad <= 25), 5,+
                                                      ifelse((vivienda$antiguedad == "NA"), 7,+
                                                               ifelse((vivienda$antiguedad  >= 26), 6, 0)))))))


vivienda$antiguedad_1[is.na(vivienda$antiguedad_1)] <- 7 
vivienda$antiguedad_1 <- as.character(vivienda$antiguedad_1)

mydesign <- svydesign(id=~upm,strata=~est_dis,data=vivienda,weights=~factor)


### establecemos variables
M_antigue  <-svytotal(~antiguedad_1=="1", mydesign)#Total promedio
M_antigueEnt <- svyby(~antiguedad_1=="1", by=~ent,mydesign,svytotal, na.rm=FALSE) # Estatal promedio

M_antigue
M_antigueEnt


M_antigue2  <-svytotal(~antiguedad_1==2, mydesign)#Total promedio
M_antigueEnt2 <- svyby(~antiguedad_1==2,by=~ent,mydesign,svytotal, na.rm=FALSE) # Estatal promedio

M_antigue3  <-svytotal(~antiguedad_1==3, mydesign)#Total promedio
M_antigueEnt3 <- svyby(~antiguedad_1==3,by=~ent,mydesign,svytotal, na.rm=FALSE) # Estatal promedio


ES_M_antigue <- M_antigue[[2]]
ES_M_antigueEnt <- M_antigueEnt[[3]]

ES_M_antigue2 <- M_antigue2[[2]]
ES_M_antigueEnt2 <- M_antigueEnt2[[3]]

ES_M_antigue3 <- M_antigue3[[2]]
ES_M_antigueEnt3 <- M_antigueEnt3[[3]]

# Creamos la base a mostrar
c_ent_ES8 <- data.frame(c(ES_M_antigue, ES_M_antigueEnt), c(ES_M_antigue2, ES_M_antigueEnt2), c(ES_M_antigue3 ,ES_M_antigueEnt3))
# Agregamos nombres
colnames(c_ent_ES8) <- c("MADERA, MOSAICO U OTRO RECUBRIMIENTO", "CEMENTO O FIRME", "TIERRA")
row.names(c_ent_ES8)<- Entidades
c_ent_ES8

##################################################
#### 1.9 VIVIENDAS POR ENTIDAD FEDERATIVA, SEGÚN DISPONIBILIDAD DE CUARTO PARA COCINAR Y USO PARA DORMIR

### establecemos variables
M_mat_cocinaloc  <-svytotal(~cocina=="1", mydesign)#Total promedio
M_mat_cocinalocEnt <- svyby(~cocina=="1", by=~ent,mydesign,svytotal, na.rm=FALSE) # Estatal promedio

M_mat_cocina_dorloc2  <-svytotal(~cocina_dor=="2", mydesign)#Total promedio
M_mat_cocina_dorlocEnt2 <- svyby(~cocina_dor=="2",by=~ent,mydesign,svytotal, na.rm=FALSE) # Estatal promedio

M_mat_pisoloc3  <-svytotal(~mat_pisos=="1", mydesign)#Total promedio
M_mat_pisolocEnt3 <- svyby(~mat_pisos=="1",by=~ent,mydesign,svytotal, na.rm=FALSE) # Estatal promedio


ES_M_mat_pisoloc <- M_mat_pisoloc[[2]]
ES_M_mat_pisolocEnt <- M_mat_pisolocEnt[[3]]

ES_M_mat_pisoloc2 <- M_mat_pisoloc2[[2]]
ES_M_mat_pisolocEnt2 <- M_mat_pisolocEnt2[[3]]

ES_M_mat_pisoloc3 <- M_mat_pisoloc3[[2]]
ES_M_mat_pisolocEnt3 <- M_mat_pisolocEnt3[[3]]

# Creamos la base a mostrar
c_ent_ES7 <- data.frame(c(ES_M_mat_pisoloc ,ES_M_mat_pisolocEnt), c(ES_M_mat_pisoloc2 ,ES_M_mat_pisolocEnt2), c(ES_M_mat_pisoloc3 ,ES_M_mat_pisolocEnt3))
# Agregamos nombres
colnames(c_ent_ES7) <- c("MADERA, MOSAICO U OTRO RECUBRIMIENTO", "CEMENTO O FIRME", "TIERRA")
row.names(c_ent_ES7)<- Entidades
c_ent_ES8
######################################################################
######## 2.1 HOGARES QUE EN LOS ÚLTIMOS TRES MESES EXPERIMENTARON DIFICULTADES PARA SATISFACER SUS NECESIDADES ALIMENTARIAS, 
######## POR FALTA DE DINERO O RECURSOS* POR ENTIDAD FEDERATIVA,  SEGÚN TIPO DE DIFICULTAD

M_acc1 <-svytotal(~acc_alim1 ==2, mydesign)#Total promedio
M_acc1Ent <- svyby(~acc_alim1==2,by=~ent,mydesign,svytotal) # Estatal promedio

M_acc2 <-svytotal(~acc_alim2 ==2, mydesign)#Total promedio
M_acc2Ent <- svyby(~acc_alim2==2,by=~ent,mydesign,svytotal) # Estatal promedio

M_acc3 <-svytotal(~acc_alim3 ==2, mydesign)#Total promedio
M_acc3Ent <- svyby(~acc_alim3==2,by=~ent,mydesign,svytotal) # Estatal promedio

M_acc4 <-svytotal(~acc_alim4 ==2, mydesign)#Total promedio
M_acc4Ent <- svyby(~acc_alim4==2,by=~ent,mydesign,svytotal) # Estatal promedio

M_acc5 <-svytotal(~acc_alim5 ==2, mydesign)#Total promedio
M_acc5Ent <- svyby(~acc_alim5==2,by=~ent,mydesign,svytotal) # Estatal promedio

M_acc6 <-svytotal(~acc_alim6 ==2, mydesign)#Total promedio
M_acc6Ent <- svyby(~acc_alim6 ==2,by=~ent,mydesign,svytotal) # Estatal promedio

ES_M_acc1 <- M_acc1[[1]]
ES_M_acc1Ent <- M_acc1Ent[[2]]

ES_M_acc2 <- M_acc2[[1]]
ES_M_acc2Ent <- M_acc2Ent[[2]]

ES_M_acc3 <- M_acc3[[1]]
ES_M_acc3Ent <- M_acc3Ent[[2]]

ES_M_acc4 <- M_acc4[[1]]
ES_M_acc4Ent <- M_acc4Ent [[2]]

ES_M_acc5 <- M_acc5[[1]]
ES_M_acc5Ent <- M_acc5Ent[[2]]

ES_M_acc6 <- M_acc6[[1]]
ES_M_acc6Ent <- M_acc6Ent[[2]]

# Creamos la base a mostrar
c_ent_ES <- data.frame(c(ES_M_acc1 ,ES_M_acc1Ent), c(ES_M_acc2 ,ES_M_acc2Ent), c(ES_M_acc3 ,ES_M_acc3Ent), c(ES_M_acc4 ,ES_M_acc4Ent), c(ES_M_acc5 ,ES_M_acc5Ent), c(ES_M_acc6 ,ES_M_acc6Ent))
# Agregamos nombres
colnames(c_ent_ES) <- c("CON PREOCUPACIÓN DE QUE LA COMIDA SE ACABARA", "QUE SE QUEDARON SIN COMIDA", "SIN ALIMENTACIÓN SANA Y VARIADA", "ALIMENTACIÓN DE ADULTOS BASADA EN MUY POCA VARIEDAD DE ALIMENTOS", "ADULTOS QUE DEJARON DE DESAYUNAR, COMER O CENAR", "ADULTOS QUE COMIERON MENOS DE LO QUE PIENSA DEBÍA COMER", "HOGARES QUE HAN EXPERIMENTADO ALGUNA DIFICULTAD PARA SATISFACER SUS NECESIDADES ALIMENTARIAS")
row.names(c_ent_ES)<- Entidades
c_ent_ES

## 2.2 HOGARES QUE EN LOS ÚLTIMOS TRES MESES EXPERIMENTARON DIFICULTADES PARA SATISFACER SUS NECESIDADES 
### ALIMENTARIAS, POR FALTA DE DINERO O RECURSOS* POR ENTIDAD FEDERATIVA, SEGÚN TIPO DE DIFICULTAD"										

M_acc7 <-svytotal(~acc_alim7 ==2, mydesign, na.rm=TRUE)#Total promedio
M_acc7Ent <- svyby(~acc_alim7 ==2,by=~ent,mydesign,svytotal, na.rm=TRUE) # Estatal promedio

M_acc8 <-svytotal(~acc_alim8 ==2, mydesign, na.rm=TRUE)#Total promedio
M_acc8Ent <- svyby(~acc_alim8 ==2,by=~ent,mydesign,svytotal, na.rm=TRUE) # Estatal promedio

M_acc9 <-svytotal(~acc_alim9 ==2, mydesign, na.rm=TRUE)#Total promedio
M_acc9Ent <- svyby(~acc_alim9 ==2,by=~ent,mydesign,svytotal, na.rm=TRUE) # Estatal promedio

ES_M_acc7 <- M_acc7[[1]]
ES_M_acc7Ent <- M_acc7Ent[[2]]

ES_M_acc8 <- M_acc8[[1]]
ES_M_acc8Ent <- M_acc8Ent[[2]]

ES_M_acc9 <- M_acc9[[1]]
ES_M_acc9Ent <- M_acc9Ent[[2]]

# Creamos la base a mostrar
c_ent_ES2 <- data.frame(c(ES_M_acc7 ,ES_M_acc7Ent), c(ES_M_acc8 ,ES_M_acc8Ent), c(ES_M_acc9 ,ES_M_acc9Ent))
# Agregamos nombres
colnames(c_ent_ES2) <- c("ALGÚN ADULTO SINTIÓ HAMBRE PERO NO COMIÓ", "ALGÚN ADULTO COMIÓ UNA VEZ AL DÍA O DEJÓ DE COMER TODO UN DÍA", "TUVIERON QUE HACER ALGO QUE HUBIERAN PREFERIDO NO HACER PARA CONSEGUIR COMIDA")
row.names(c_ent_ES2)<- Entidades
c_ent_ES2

## 2.3 HOGARES CON MENORES DE 18 AÑOS, QUE EN LOS ÚLTIMOS TRES MESES EXPERIMENTARÓN DIFICULTADES PARA SATISFACER LAS NECESIDADES  
#ALIMENTARIAS DE LOS MENORES, POR FALTA DE DINERO O RECURSOS* POR ENTIDAD FEDERATIVA, SEGÚN TIPO DE DIFICULTADHOGARES QUE EN LOS ÚLTIMOS TRES MESES EXPERIMENTARON DIFICULTADES PARA SATISFACER SUS NECESIDADES 

M_acc10 <-svytotal(~acc_alim10 ==2, mydesign, na.rm=TRUE)#Total promedio
M_acc10Ent <- svyby(~acc_alim10 ==2,by=~ent,mydesign,svytotal, na.rm=TRUE) # Estatal promedio

M_acc11 <-svytotal(~acc_alim11 ==2, mydesign, na.rm=TRUE)#Total promedio
M_acc11Ent <- svyby(~acc_alim11 ==2,by=~ent,mydesign,svytotal, na.rm=TRUE) # Estatal promedio

M_acc12 <-svytotal(~acc_alim12 ==2, mydesign, na.rm=TRUE)#Total promedio
M_acc12Ent <- svyby(~acc_alim12 ==2,by=~ent,mydesign,svytotal, na.rm=TRUE) # Estatal promedio

M_acc13 <-svytotal(~acc_alim13 ==2, mydesign, na.rm=TRUE)#Total promedio
M_acc13Ent <- svyby(~acc_alim13 ==2,by=~ent,mydesign,svytotal, na.rm=TRUE) # Estatal promedio

M_acc14 <-svytotal(~acc_alim14 ==2, mydesign, na.rm=TRUE)#Total promedio
M_acc14Ent <- svyby(~acc_alim14 ==2,by=~ent,mydesign,svytotal, na.rm=TRUE) # Estatal promedio

M_acc15 <-svytotal(~acc_alim15 ==2, mydesign, na.rm=TRUE)#Total promedio
M_acc15Ent <- svyby(~acc_alim15 ==2,by=~ent,mydesign,svytotal, na.rm=TRUE) # Estatal promedio

M_acc16 <-svytotal(~acc_alim16 ==2, mydesign, na.rm=TRUE)#Total promedio
M_acc16Ent <- svyby(~acc_alim16 ==2,by=~ent,mydesign,svytotal, na.rm=TRUE) # Estatal promedio


ES_M_acc10 <- M_acc10[[1]]
ES_M_acc10Ent <- M_acc10Ent[[2]]

ES_M_acc11 <- M_acc11[[1]]
ES_M_acc11Ent <- M_acc11Ent[[2]]

ES_M_acc12 <- M_acc12[[1]]
ES_M_acc12Ent <- M_acc12Ent[[2]]

ES_M_acc13 <- M_acc13[[1]]
ES_M_acc13Ent <- M_acc13Ent[[2]]

ES_M_acc14 <- M_acc14[[1]]
ES_M_acc14Ent <- M_acc14Ent[[2]]

ES_M_acc15 <- M_acc15[[1]]
ES_M_acc15Ent <- M_acc15Ent[[2]]

ES_M_acc16 <- M_acc16[[1]]
ES_M_acc16Ent <- M_acc16Ent[[2]]

# Creamos la base a mostrar
c_ent_ES3 <- data.frame(c(ES_M_acc10 ,ES_M_acc10Ent), c(ES_M_acc11 ,ES_M_acc11Ent), c(ES_M_acc12 ,ES_M_acc12Ent), c(ES_M_acc13 ,ES_M_acc13Ent), c(ES_M_acc14 ,ES_M_acc14Ent), c(ES_M_acc15 ,ES_M_acc15Ent),c(ES_M_acc16 ,ES_M_acc16Ent))
# Agregamos nombres
colnames(c_ent_ES3) <- c("ALGÚN ADULTO SINTIÓ HAMBRE PERO NO COMIÓ", "ALGÚN ADULTO COMIÓ UNA VEZ AL DÍA O DEJÓ DE COMER TODO UN DÍA", "TUVIERON QUE HACER ALGO QUE HUBIERAN PREFERIDO NO HACER PARA CONSEGUIR COMIDA")
row.names(c_ent_ES3)<- Entidades
c_ent_ES3




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