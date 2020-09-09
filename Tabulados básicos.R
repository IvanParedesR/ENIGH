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
                  ifelse((vivienda$antiguedad >= 6 & vivienda$antiguedad <= 15),  2,+
                           ifelse((vivienda$antiguedad  >= 16 & vivienda$antiguedad  <= 25),  3,+
                                    ifelse((vivienda$antiguedad  >= 26 & vivienda$antiguedad <= 30), 4,+
                                             ifelse((vivienda$antiguedad >= 31 & vivienda$antiguedad <= 35), 5,+
                                                      ifelse((vivienda$antiguedad == "NA"), 7,+
                                                               ifelse((vivienda$antiguedad  >= 36), 6, 0)))))))


vivienda$antiguedad_1[is.na(vivienda$antiguedad_1)] <- 7 
vivienda$antiguedad_1 <- as.character(vivienda$antiguedad_1)

mydesign <- svydesign(id=~upm,strata=~est_dis,data=vivienda,weights=~factor)


### establecemos variables
M_antigue  <-svytotal(~antiguedad_1=="1", mydesign)#Total promedio
M_antigueEnt <- svyby(~antiguedad_1=="1", by=~ent,mydesign,svytotal, na.rm=FALSE) # Estatal promedio

M_antigue2  <-svytotal(~antiguedad_1=="2", mydesign)#Total promedio
M_antigueEnt2 <- svyby(~antiguedad_1=="2",by=~ent,mydesign,svytotal, na.rm=FALSE) # Estatal promedio

M_antigue3  <-svytotal(~antiguedad_1=="3", mydesign)#Total promedio
M_antigueEnt3 <- svyby(~antiguedad_1=="3",by=~ent,mydesign,svytotal, na.rm=FALSE) # Estatal promedio

M_antigue4  <-svytotal(~antiguedad_1=="4", mydesign)#Total promedio
M_antigueEnt4 <- svyby(~antiguedad_1=="4",by=~ent,mydesign,svytotal, na.rm=FALSE) # Estatal promedio

M_antigue5  <-svytotal(~antiguedad_1=="5", mydesign)#Total promedio
M_antigueEnt5 <- svyby(~antiguedad_1=="5",by=~ent,mydesign,svytotal, na.rm=FALSE) # Estatal promedio

M_antigue6  <-svytotal(~antiguedad_1=="6", mydesign)#Total promedio
M_antigueEnt6 <- svyby(~antiguedad_1=="6",by=~ent,mydesign,svytotal, na.rm=FALSE) # Estatal promedio


ES_M_antigue <- M_antigue[[2]]
ES_M_antigueEnt <- M_antigueEnt[[3]]

ES_M_antigue2 <- M_antigue2[[2]]
ES_M_antigueEnt2 <- M_antigueEnt2[[3]]

ES_M_antigue3 <- M_antigue3[[2]]
ES_M_antigueEnt3 <- M_antigueEnt3[[3]]

ES_M_antigue4 <- M_antigue4[[2]]
ES_M_antigueEnt4 <- M_antigueEnt4[[3]]

ES_M_antigue5 <- M_antigue5[[2]]
ES_M_antigueEnt5 <- M_antigueEnt5[[3]]

ES_M_antigue6 <- M_antigue6[[2]]
ES_M_antigueEnt6 <- M_antigueEnt6[[3]]

ES_M_antigue7 <- M_antigue7[[2]]
ES_M_antigueEnt7 <- M_antigueEnt7[[3]]

# Creamos la base a mostrar
c_ent_ES8 <- data.frame(c(ES_M_antigue, ES_M_antigueEnt), c(ES_M_antigue2, ES_M_antigueEnt2), c(ES_M_antigue3 ,ES_M_antigueEnt3), c(ES_M_antigue4 ,ES_M_antigueEnt4), c(ES_M_antigue5 ,ES_M_antigueEnt5), c(ES_M_antigue6 ,ES_M_antigueEnt6))
# Agregamos nombres
colnames(c_ent_ES8) <- c("DE 0 A 5 AÑOS",	"DE 6 A 15 AÑOS", "DE 16 A 25 AÑOS", "26 O MÁS AÑOS")

row.names(c_ent_ES8)<- Entidades
c_ent_ES8

##################################################################
#### 1.9 VIVIENDAS POR ENTIDAD FEDERATIVA, SEGÚN DISPONIBILIDAD DE CUARTO PARA COCINAR Y USO PARA DORMIR

### establecemos variables
M_mat_cocinaloc  <-svytotal(~cocina=="1", mydesign)#Total promedio
M_mat_cocinalocEnt <- svyby(~cocina=="1", by=~ent,mydesign,svytotal, na.rm=FALSE) # Estatal promedio

M_mat_cocinaloc1  <-svytotal(~cocina=="2", mydesign)#Total promedio
M_mat_cocinalocEnt1 <- svyby(~cocina=="2", by=~ent,mydesign,svytotal, na.rm=FALSE) # Estatal promedio

M_mat_cocina_dorloc2  <-svytotal(~cocina_dor=="1", mydesign, na.rm=TRUE)#Total promedio
M_mat_cocina_dorlocEnt2 <- svyby(~cocina_dor=="1",by=~ent,mydesign,svytotal, na.rm=TRUE) # Estatal promedio

M_mat_cocina_dorloc3  <-svytotal(~cocina_dor=="2", mydesign, na.rm=TRUE)#Total promedio
M_mat_cocina_dorlocEnt3 <- svyby(~cocina_dor=="2",by=~ent,mydesign,svytotal, na.rm=TRUE) # Estatal promedio


ES_M_mat_cocinaloc <- M_mat_cocinaloc[[2]]
ES_M_mat_cocinalocEnt <- M_mat_cocinalocEnt[[3]]

ES_M_mat_cocinaloc1 <- M_mat_cocinaloc1[[2]]
ES_M_mat_cocinalocEnt1 <- M_mat_cocinalocEnt1[[3]]

ES_M_mat_cocina_dorloc2 <- M_mat_cocina_dorloc2[[2]]
ES_M_mat_cocina_dorlocEnt2 <- M_mat_cocina_dorlocEnt2[[3]]

ES_M_mat_cocina_dorloc3 <- M_mat_cocina_dorloc3[[2]]
ES_M_mat_cocina_dorlocEnt3 <- M_mat_cocina_dorlocEnt3[[3]]

# Creamos la base a mostrar
c_ent_ES9 <- data.frame(c(ES_M_mat_cocinaloc ,ES_M_mat_cocinalocEnt), c(ES_M_mat_cocinaloc1 ,ES_M_mat_cocinalocEnt1), c(ES_M_mat_cocina_dorloc2 ,ES_M_mat_cocina_dorlocEnt2),  c(ES_M_mat_cocina_dorloc3 ,ES_M_mat_cocina_dorlocEnt3))
# Agregamos nombres
colnames(c_ent_ES9) <- c("VIVIENDAS CON CUARTO PARA COCINAR", "VIVIENDAS SIN CUARTO PARA COCINAR", "VIVIENDAS CON CUARTO PARA COCINAR Y DORMIR", "VIVIENDAS SIN CUARTO PARA COCINAR Y DORMIR")
row.names(c_ent_ES9)<- Entidades
c_ent_ES9
##################################################################
#### 1.10 PROMEDIO DE CUARTOS Y CUARTOS PARA DORMIR CON LOS QUE CUENTA 

### establecemos variables
M_mat_numcuartos <-svymean(~num_cuarto, mydesign, na.rm=FALSE) #promedio
M_mat_numcuartosEnt <- svyby(~num_cuarto, by=~ent,mydesign,svymean, na.rm=FALSE) # Estatal promedio

M_mat_numcuartos1 <-svymean(~cuart_dorm, mydesign, na.rm=FALSE) #promedio
M_mat_numcuartosEnt1 <- svyby(~cuart_dorm, by=~ent,mydesign,svymean, na.rm=FALSE) # Estatal promedio

ES_M_mat_numcuartos <- M_mat_numcuartos[[2]]
ES_M_mat_numcuartosEnt <- M_mat_numcuartosEnt[[2]]

ES_M_mat_numcuartos1 <- M_mat_numcuartos1[[2]]
ES_M_mat_numcuartosEnt1 <- M_mat_numcuartosEnt1[[2]]

# Creamos la base a mostrar
c_ent_ES10 <- data.frame(c(ES_M_mat_numcuartos ,ES_M_mat_numcuartosEnt), c(ES_M_mat_numcuartos1 ,ES_M_mat_numcuartosEnt1))
# Agregamos nombres
colnames(c_ent_ES10) <- c("CUARTOS POR VIVIENDA", "CUARTOS PARA DORMIR POR VIVIENDA")
row.names(c_ent_ES10)<- Entidades
c_ent_ES10

##################################################################

#### 1.11 VIVIENDAS POR ENTIDAD FEDERATIVA, SEGÚN DISPONIBILIDAD DE AGUA										

### establecemos variables
M_mat_dotac_agua  <-svytotal(~dotac_agua==1, mydesign, na.rm=TRUE)#Total promedio
M_mat_dotac_aguaEnt <- svyby(~dotac_agua==1, by=~ent,mydesign,svytotal, na.rm=TRUE) # Estatal promedio

M_mat_dotac_agualoc1  <-svytotal(~dotac_agua==2, mydesign, na.rm=TRUE)#Total promedio
M_mat_dotac_agualocEnt1 <- svyby(~dotac_agua==2, by=~ent,mydesign,svytotal, na.rm=TRUE) # Estatal promedio

M_mat_dotac_agualoc2  <-svytotal(~dotac_agua==3, mydesign, na.rm=TRUE)#Total promedio
M_mat_dotac_agualocEnt2 <- svyby(~dotac_agua==3, by=~ent,mydesign,svytotal, na.rm=TRUE) # Estatal promedio

M_mat_dotac_agualoc3  <-svytotal(~dotac_agua==4, mydesign, na.rm=TRUE)#Total promedio
M_mat_dotac_agualocEnt3 <- svyby(~dotac_agua==4, by=~ent,mydesign,svytotal, na.rm=TRUE) # Estatal promedio


ES_M_mat_dotac_agua <- M_mat_dotac_agua[[2]]
ES_M_mat_dotac_aguaEnt <- M_mat_dotac_aguaEnt[[3]]

ES_M_mat_dotac_agualoc1 <- M_mat_dotac_agualoc1[[2]]
ES_M_mat_dotac_agualocEnt1 <- M_mat_dotac_agualocEnt1[[3]]

ES_M_mat_dotac_agualoc2 <- M_mat_dotac_agualoc2[[2]]
ES_M_mat_dotac_agualocEnt2 <- M_mat_dotac_agualocEnt2[[3]]

ES_M_mat_dotac_agualoc3 <- M_mat_dotac_agualoc3[[2]]
ES_M_mat_dotac_agualocEnt3 <- M_mat_dotac_agualocEnt3[[3]]

# Creamos la base a mostrar
c_ent_ES12 <- data.frame(c(ES_M_mat_dotac_agua, ES_M_mat_dotac_aguaEnt), c(ES_M_mat_dotac_agualoc1 ,ES_M_mat_dotac_agualocEnt1), c(ES_M_mat_dotac_agualoc2 ,ES_M_mat_dotac_agualocEnt2),  c(ES_M_mat_dotac_agualoc3 ,ES_M_mat_dotac_agualocEnt3))
# Agregamos nombres
colnames(c_ent_ES12) <- c("Diario", "Cada tercer día", "Dos veces por semana", "Una vez por semana")
row.names(c_ent_ES12)<- Entidades
c_ent_ES12

##################################################################
#### 1.13 "VIVIENDAS POR ENTIDAD FEDERATIVA, SEGÚN DISPONIBILIDAD DE SERVICIO SANITARIO"							

### establecemos variables
M_mat_dotac_agua  <-svytotal(~excusado==1, mydesign, na.rm=TRUE)#Total promedio
M_mat_dotac_aguaEnt <- svyby(~excusado==1, by=~ent,mydesign,svytotal, na.rm=TRUE) # Estatal promedio

M_mat_dotac_agualoc1  <-svytotal(~excusado==2, mydesign, na.rm=TRUE)#Total promedio
M_mat_dotac_agualocEnt1 <- svyby(~excusado==2, by=~ent,mydesign,svytotal, na.rm=TRUE) # Estatal promedio

ES_M_mat_dotac_agua <- M_mat_dotac_agua[[2]]
ES_M_mat_dotac_aguaEnt <- M_mat_dotac_aguaEnt[[3]]

ES_M_mat_dotac_agualoc1 <- M_mat_dotac_agualoc1[[2]]
ES_M_mat_dotac_agualocEnt1 <- M_mat_dotac_agualocEnt1[[3]]

# Creamos la base a mostrar
c_ent_ES13 <- data.frame(c(ES_M_mat_dotac_agua, ES_M_mat_dotac_aguaEnt), c(ES_M_mat_dotac_agualoc1 ,ES_M_mat_dotac_agualocEnt1))

# Agregamos nombres
colnames(c_ent_ES13) <- c("Sí", "No")
row.names(c_ent_ES13)<- Entidades
c_ent_ES13

##################################################################

#### 1.14 ""VIVIENDAS CON SERVICIO SANITARIO POR ENTIDAD FEDERATIVA, SEGÚN USO EXCLUSIVO""							

### establecemos variables
M_mat_sanitarioex  <-svytotal(~uso_compar==1, mydesign, na.rm=TRUE)#Total promedio
M_mat_sanitarioexEnt <- svyby(~uso_compar==1, by=~ent,mydesign,svytotal, na.rm=TRUE) # Estatal promedio

M_mat_sanitarioexloc1  <-svytotal(~uso_compar==2, mydesign, na.rm=TRUE)#Total promedio
M_mat_sanitarioexlocEnt1 <- svyby(~ uso_compar==2, by=~ent,mydesign,svytotal, na.rm=TRUE) # Estatal promedio

ES_M_mat_sanitarioex <- M_mat_sanitarioex[[2]]
ES_M_mat_sanitarioexEnt <- M_mat_sanitarioexEnt[[3]]

ES_M_mat_sanitarioexloc1 <- M_mat_sanitarioexloc1[[2]]
ES_M_mat_sanitarioexlocEnt1 <- M_mat_sanitarioexlocEnt1[[3]]

# Creamos la base a mostrar
c_ent_ES14 <- data.frame(c(ES_M_mat_sanitarioex, ES_M_mat_sanitarioexEnt), c(ES_M_mat_sanitarioexloc1 ,ES_M_mat_sanitarioexlocEnt1))

# Agregamos nombres
colnames(c_ent_ES14) <- c("No", "Sí")
row.names(c_ent_ES14)<- Entidades
c_ent_ES14
##################################################################

#### 1.15 "VIVIENDAS CON SERVICIO SANITARIO POR ENTIDAD FEDERATIVA, SEGÚN ADMISIÓN DE AGUA																

### establecemos variables
M_mat_sanit_agua  <-svytotal(~sanit_agua==1, mydesign, na.rm=TRUE)#Total promedio
M_mat_sanit_aguaEnt <- svyby(~sanit_agua==1, by=~ent,mydesign,svytotal, na.rm=TRUE) # Estatal promedio

M_mat_sanit_agua1  <-svytotal(~sanit_agua==2, mydesign, na.rm=TRUE)#Total promedio
M_mat_sanit_aguaEnt1 <- svyby(~sanit_agua==2, by=~ent,mydesign,svytotal, na.rm=TRUE) # Estatal promedio

M_mat_sanit_agua2  <-svytotal(~sanit_agua==3, mydesign, na.rm=TRUE)#Total promedio
M_mat_sanit_aguaEnt2 <- svyby(~sanit_agua==3, by=~ent,mydesign,svytotal, na.rm=TRUE) # Estatal promedio

ES_M_mat_sanit_agua <- M_mat_sanit_agua[[2]]
ES_M_mat_sanit_aguaEnt <- M_mat_sanit_aguaEnt[[3]]

ES_M_mat_sanit_agua1 <- M_mat_sanit_agua1[[2]]
ES_M_mat_sanit_aguaEnt1 <- M_mat_sanit_aguaEnt1[[3]]

ES_M_mat_sanit_agua2 <- M_mat_sanit_agua2[[2]]
ES_M_mat_sanit_aguaEnt2 <- M_mat_sanit_aguaEnt2[[3]]

# Creamos la base a mostrar
c_ent_ES15 <- data.frame(c(ES_M_mat_sanit_agua, ES_M_mat_sanit_aguaEnt), c(ES_M_mat_sanit_agua1 ,ES_M_mat_sanit_aguaEnt1), c(ES_M_mat_sanit_agua2 ,ES_M_mat_sanit_aguaEnt2))

# Agregamos nombres
colnames(c_ent_ES15) <- c("TIENE DESCARGA DIRECTA DE AGUA", "LE ECHAN AGUA CON CUBETA", "NO SE LE PUEDE ECHAR AGUA")
row.names(c_ent_ES15)<- Entidades
c_ent_ES15

##################################################################
#### 1.16 "VIVIENDAS CON SERVICIO SANITARIO POR ENTIDAD FEDERATIVA, SEGÚN DISPONIBILIDAD DE BIODIGESTOR"													

### establecemos variables
M_mat_biodigest  <-svytotal(~biodigest==1, mydesign, na.rm=TRUE)#Total promedio
M_mat_biodigestEnt <- svyby(~biodigest==1, by=~ent,mydesign,svytotal, na.rm=TRUE) # Estatal promedio

M_mat_biodigest1  <-svytotal(~biodigest==2, mydesign, na.rm=TRUE)#Total promedio
M_mat_biodigestEnt1 <- svyby(~biodigest==2, by=~ent,mydesign,svytotal, na.rm=TRUE) # Estatal promedio


ES_M_mat_biodigest <- M_mat_biodigest[[2]]
ES_M_mat_biodigestEnt <- M_mat_biodigestEnt[[3]]

ES_M_mat_biodigest1 <- M_mat_biodigest1[[2]]
ES_M_mat_biodigestEnt1 <- M_mat_biodigestEnt1[[3]]

# Creamos la base a mostrar
c_ent_ES16 <- data.frame(c(ES_M_mat_biodigest, ES_M_mat_biodigestEnt), c(ES_M_mat_biodigest1 ,ES_M_mat_biodigestEnt1))

# Agregamos nombres
colnames(c_ent_ES16) <- c("Sí", "No")
row.names(c_ent_ES16)<- Entidades
c_ent_ES16

##################################################################

#### 1.17 "VIVIENDAS POR ENTIDAD FEDERATIVA, SEGÚN TIPO DE CONEXIÓN DE DRENAJE"								"													

### establecemos variables
M_mat_drenaje  <-svytotal(~drenaje==1, mydesign, na.rm=TRUE)#Total promedio
M_mat_drenajeEnt <- svyby(~drenaje==1, by=~ent,mydesign,svytotal, na.rm=TRUE) # Estatal promedio

M_mat_drenaje1  <-svytotal(~drenaje==2, mydesign, na.rm=TRUE)#Total promedio
M_mat_drenajeEnt1 <- svyby(~drenaje==2, by=~ent,mydesign,svytotal, na.rm=TRUE) # Estatal promedio

M_mat_drenaje2  <-svytotal(~drenaje==3, mydesign, na.rm=TRUE)#Total promedio
M_mat_drenajeEnt2 <- svyby(~drenaje==3, by=~ent,mydesign,svytotal, na.rm=TRUE) # Estatal promedio

M_mat_drenaje3  <-svytotal(~drenaje==4, mydesign, na.rm=TRUE)#Total promedio
M_mat_drenajeEnt3 <- svyby(~drenaje==4, by=~ent,mydesign,svytotal, na.rm=TRUE) # Estatal promedio

M_mat_drenaje4  <-svytotal(~drenaje==5, mydesign, na.rm=TRUE)#Total promedio
M_mat_drenajeEnt4 <- svyby(~drenaje==5, by=~ent,mydesign,svytotal, na.rm=TRUE) # Estatal promedio

ES_M_mat_drenaje <- M_mat_drenaje[[2]]
ES_M_mat_drenajeEnt <- M_mat_drenajeEnt[[3]]

ES_M_mat_drenaje1 <- M_mat_drenaje1[[2]]
ES_M_mat_drenajeEnt1 <- M_mat_drenajeEnt1[[3]]

ES_M_mat_drenaje2 <- M_mat_drenaje2[[2]]
ES_M_mat_drenajeEnt2 <- M_mat_drenajeEnt2[[3]]

ES_M_mat_drenaje3 <- M_mat_drenaje3[[2]]
ES_M_mat_drenajeEnt3 <- M_mat_drenajeEnt3[[3]]

ES_M_mat_drenaje4 <- M_mat_drenaje4[[2]]
ES_M_mat_drenajeEnt4 <- M_mat_drenajeEnt4[[3]]

# Creamos la base a mostrar
c_ent_ES17 <- data.frame(c(ES_M_mat_drenaje, ES_M_mat_drenajeEnt), c(ES_M_mat_drenaje1 ,ES_M_mat_drenajeEnt1), c(ES_M_mat_drenaje2 ,ES_M_mat_drenajeEnt2), c(ES_M_mat_drenaje3 ,ES_M_mat_drenajeEnt3), c(ES_M_mat_drenaje4 ,ES_M_mat_drenajeEnt4))

# Agregamos nombres
colnames(c_ent_ES17) <- c("Sí", "No")
row.names(c_ent_ES17)<- Entidades
c_ent_ES17

##################################################################

#### 1.18 ""VIVIENDAS POR ENTIDAD FEDERATIVA, SEGÚN FUENTE DE OBTENCIÓN DE ENERGIA ELÉCTRICA "													

### establecemos variables
M_mat_disp_elect  <-svytotal(~disp_elect==1, mydesign, na.rm=TRUE)#Total promedio
M_mat_disp_electEnt <- svyby(~disp_elect==1, by=~ent,mydesign,svytotal, na.rm=TRUE) # Estatal promedio

M_mat_disp_elect1  <-svytotal(~disp_elect==2, mydesign, na.rm=TRUE)#Total promedio
M_mat_disp_electEnt1 <- svyby(~disp_elect==2, by=~ent,mydesign,svytotal, na.rm=TRUE) # Estatal promedio

M_mat_disp_elect2  <-svytotal(~disp_elect==3, mydesign, na.rm=TRUE)#Total promedio
M_mat_disp_electEnt2 <- svyby(~disp_elect==3, by=~ent,mydesign,svytotal, na.rm=TRUE) # Estatal promedio

M_mat_disp_elect3  <-svytotal(~disp_elect==4, mydesign, na.rm=TRUE)#Total promedio
M_mat_disp_electEnt3 <- svyby(~disp_elect==4, by=~ent,mydesign,svytotal, na.rm=TRUE) # Estatal promedio

M_mat_disp_elect4  <-svytotal(~disp_elect==5, mydesign, na.rm=TRUE)#Total promedio
M_mat_disp_electEnt4 <- svyby(~disp_elect==5, by=~ent,mydesign,svytotal, na.rm=TRUE) # Estatal promedio

ES_M_mat_disp_elect <- M_mat_disp_elect[[2]]
ES_M_mat_disp_electEnt <- M_mat_disp_electEnt[[3]]

ES_M_mat_disp_elect1 <- M_mat_disp_elect1[[2]]
ES_M_mat_disp_electEnt1 <- M_mat_disp_electEnt1[[3]]

ES_M_mat_disp_elect2 <- M_mat_disp_elect2[[2]]
ES_M_mat_disp_electEnt2 <- M_mat_disp_electEnt2[[3]]

ES_M_mat_disp_elect3 <- M_mat_disp_elect3[[2]]
ES_M_mat_disp_electEnt3 <- M_mat_disp_electEnt3[[3]]

ES_M_mat_disp_elect4 <- M_mat_disp_elect4[[2]]
ES_M_mat_disp_electEnt4 <- M_mat_disp_electEnt4[[3]]

# Creamos la base a mostrar
c_ent_ES18 <- data.frame(c(ES_M_mat_disp_elect, ES_M_mat_disp_electEnt), c(ES_M_mat_disp_elect1 ,ES_M_mat_disp_electEnt1), c(ES_M_mat_disp_elect2 ,ES_M_mat_disp_electEnt2), c(ES_M_mat_disp_elect3 ,ES_M_mat_disp_electEnt3), c(ES_M_mat_disp_elect4 ,ES_M_mat_disp_electEnt4))

# Agregamos nombres
colnames(c_ent_ES18) <- c("Sí", "No")
row.names(c_ent_ES18)<- Entidades
c_ent_ES18

##################################################################

# 1.19 PROMEDIO DE FOCOS POR VIVIENDA POR ENTIDAD FEDERATIVA, SEGÚN TIPO DE FOCOS					


### establecemos variables
M_mat_focos_incas <-svymean(~focos_inca, mydesign, na.rm=TRUE) #promedio
M_mat_focos_incasEnt <- svyby(~focos_inca, by=~ent,mydesign,svymean, na.rm=TRUE) # Estatal promedio

M_mat_focos_ahora1 <-svymean(~focos_ahor, mydesign, na.rm=TRUE) #promedio
M_mat_focos_ahoraEnt1 <- svyby(~focos_ahor, by=~ent,mydesign,svymean, na.rm=TRUE) # Estatal promedio

ES_M_mat_focos_incas <- M_mat_focos_incas[[1]]
ES_M_mat_focos_incasEnt <- M_mat_focos_incasEnt[[2]]

ES_M_mat_focos_ahora1 <- M_mat_focos_ahora1[[1]]
ES_M_mat_focos_ahoraEnt1 <- M_mat_focos_ahoraEnt1[[2]]

# Creamos la base a mostrar
c_ent_ES19 <- data.frame(c(ES_M_mat_focos_incas ,ES_M_mat_focos_incasEnt), c(ES_M_mat_focos_ahora1 ,ES_M_mat_focos_ahoraEnt1))
# Agregamos nombres
colnames(c_ent_ES19) <- c("Incadescentes", "Ahorradores")
row.names(c_ent_ES19)<- Entidades
c_ent_ES19

##################################################################

#### 1.20 "VIVIENDAS POR ENTIDAD FEDERATIVA, SEGÚN COMBUSTIBLE MÁS UTILIZADO PARA COCINAR"													

M_mat_combustible  <-svytotal(~combustibl==1, mydesign, na.rm=TRUE) #Total promedio
M_mat_combustibleEnt <- svyby(~combustibl==1, by=~ent,mydesign,svytotal, na.rm=TRUE) # Estatal promedio

M_mat_combustible1  <-svytotal(~combustibl==2, mydesign, na.rm=TRUE)#Total promedio
M_mat_combustibleEnt1 <- svyby(~combustibl==2, by=~ent,mydesign,svytotal, na.rm=TRUE) # Estatal promedio

M_mat_combustible2  <-svytotal(~combustibl==3, mydesign, na.rm=TRUE)#Total promedio
M_mat_combustibleEnt2 <- svyby(~combustibl==3, by=~ent,mydesign,svytotal, na.rm=TRUE) # Estatal promedio

M_mat_combustible3  <-svytotal(~combustibl==4, mydesign, na.rm=TRUE)#Total promedio
M_mat_combustibleEnt3 <- svyby(~combustibl==4, by=~ent,mydesign,svytotal, na.rm=TRUE) # Estatal promedio

M_mat_combustible4  <-svytotal(~combustibl==5, mydesign, na.rm=TRUE)#Total promedio
M_mat_combustibleEnt4 <- svyby(~combustibl==5, by=~ent,mydesign,svytotal, na.rm=TRUE) # Estatal promedio

M_mat_combustible5  <-svytotal(~combustibl==6, mydesign, na.rm=TRUE)#Total promedio
M_mat_combustibleEnt5 <- svyby(~combustibl==6, by=~ent,mydesign,svytotal, na.rm=TRUE) # Estatal promedio

ES_M_mat_combustible <- M_mat_combustible[[2]]
ES_M_mat_combustibleEnt <- M_mat_combustibleEnt[[3]]

ES_M_mat_combustible1 <- M_mat_combustible1[[2]]
ES_M_mat_combustibleEnt1 <- M_mat_combustibleEnt1[[3]]

ES_M_mat_combustible2 <- M_mat_combustible2[[2]]
ES_M_mat_combustibleEnt2 <- M_mat_combustibleEnt2[[3]]

ES_M_mat_combustible3 <- M_mat_combustible3[[2]]
ES_M_mat_combustibleEnt3 <- M_mat_combustibleEnt3[[3]]

ES_M_mat_combustible4 <- M_mat_combustible4[[2]]
ES_M_mat_combustibleEnt4 <- M_mat_combustibleEnt4[[3]]

ES_M_mat_combustible5 <- M_mat_combustible5[[2]]
ES_M_mat_combustibleEnt5 <- M_mat_combustibleEnt5[[3]]

# Creamos la base a mostrar
c_ent_ES20 <- data.frame(c(ES_M_mat_combustible, ES_M_mat_combustibleEnt), c(ES_M_mat_combustible1 ,ES_M_mat_combustibleEnt1), c(ES_M_mat_combustible2 ,ES_M_mat_combustibleEnt2), c(ES_M_mat_combustible3 ,ES_M_mat_combustibleEnt3), c(ES_M_mat_combustible4 ,ES_M_mat_combustibleEnt4), c(ES_M_mat_combustible5, ES_M_mat_combustibleEnt5))

# Agregamos nombres
# Agregamos nombres
colnames(c_ent_ES20) <- c("Leña", "Carbón", "X","XX")
row.names(c_ent_ES20)<- Entidades
c_ent_ES20

##################################################################

#### 1.21 ""VIVIENDAS QUE UTILIZAN LEÑA O CARBÓN PARA COCINAR POR ENTIDAD FEDERATIVA, SEGÚN DISPONIBILIDAD DE CHIMENEA O ALGÚN DUCTO PARA SACAR EL HUMO"							
												

M_mat_estufa_chie  <-svytotal(~estufa_chi==1, mydesign, na.rm=TRUE) #Total promedio
M_mat_estufa_chieEnt <- svyby(~estufa_chi==1, by=~ent,mydesign,svytotal, na.rm=TRUE) # Estatal promedio

M_mat_estufa_chie1  <-svytotal(~estufa_chi==2, mydesign, na.rm=TRUE)#Total promedio
M_mat_estufa_chieEnt1 <- svyby(~estufa_chi==2, by=~ent,mydesign,svytotal, na.rm=TRUE) # Estatal promedio


ES_M_mat_estufa_chie <- M_mat_estufa_chie[[2]]
ES_M_mat_estufa_chieEnt <- M_mat_estufa_chieEnt[[3]]

ES_M_mat_estufa_chie1 <- M_mat_estufa_chie1[[2]]
ES_M_mat_estufa_chieEnt1 <- M_mat_estufa_chieEnt1[[3]]


# Creamos la base a mostrar
c_ent_ES21 <- data.frame(c(ES_M_mat_estufa_chie, ES_M_mat_estufa_chieEnt), c(ES_M_mat_estufa_chie1 ,ES_M_mat_estufa_chieEnt1))

# Agregamos nombres
# Agregamos nombres
colnames(c_ent_ES21) <- c("Sí", "No")
row.names(c_ent_ES21)<- Entidades
c_ent_ES21

##################################################################

#### 1.22 "TOTAL DE VIVIENDAS POR ENTIDAD FEDERATIVA, SEGÚN FORMA EN LA QUE ELIMINA LA BASURA"

### establecemos variables
M_mat_eli_basura  <-svytotal(~eli_basura==1, mydesign, na.rm=TRUE)#Total promedio
M_mat_eli_basuraEnt <- svyby(~eli_basura==1, by=~ent,mydesign,svytotal, na.rm=TRUE) # Estatal promedio

M_mat_eli_basura1  <-svytotal(~eli_basura==2, mydesign, na.rm=TRUE)#Total promedio
M_mat_eli_basuraEnt1 <- svyby(~eli_basura==2, by=~ent,mydesign,svytotal, na.rm=TRUE) # Estatal promedio

M_mat_eli_basura2  <-svytotal(~eli_basura==3, mydesign, na.rm=TRUE)#Total promedio
M_mat_eli_basuraEnt2 <- svyby(~eli_basura==3, by=~ent,mydesign,svytotal, na.rm=TRUE) # Estatal promedio

M_mat_eli_basura3  <-svytotal(~eli_basura==4, mydesign, na.rm=TRUE)#Total promedio
M_mat_eli_basuraEnt3 <- svyby(~eli_basura==4, by=~ent,mydesign,svytotal, na.rm=TRUE) # Estatal promedio

M_mat_eli_basura4  <-svytotal(~eli_basura==5, mydesign, na.rm=TRUE)#Total promedio
M_mat_eli_basuraEnt4 <- svyby(~eli_basura==5, by=~ent,mydesign,svytotal, na.rm=TRUE) # Estatal promedio

M_mat_eli_basura5  <-svytotal(~eli_basura==6, mydesign, na.rm=TRUE)#Total promedio
M_mat_eli_basuraEnt5 <- svyby(~eli_basura==6, by=~ent,mydesign,svytotal, na.rm=TRUE) # Estatal promedio

M_mat_eli_basura6  <-svytotal(~eli_basura==7, mydesign, na.rm=TRUE)#Total promedio
M_mat_eli_basuraEnt6 <- svyby(~eli_basura==7, by=~ent,mydesign,svytotal, na.rm=TRUE) # Estatal promedio

ES_M_mat_eli_basura <- M_mat_eli_basura[[2]]
ES_M_mat_eli_basuraEnt <- M_mat_eli_basuraEnt[[3]]

ES_M_mat_eli_basura1 <- M_mat_eli_basura1[[2]]
ES_M_mat_eli_basuraEnt1 <- M_mat_eli_basuraEnt1[[3]]

ES_M_mat_eli_basura2 <- M_mat_eli_basura2[[2]]
ES_M_mat_eli_basuraEnt2 <- M_mat_eli_basuraEnt2[[3]]

ES_M_mat_eli_basura3 <- M_mat_eli_basura3[[2]]
ES_M_mat_eli_basuraEnt3 <- M_mat_eli_basuraEnt3[[3]]

ES_M_mat_eli_basura4 <- M_mat_eli_basura4[[2]]
ES_M_mat_eli_basuraEnt4 <- M_mat_eli_basuraEnt4[[3]]

ES_M_mat_eli_basura5 <- M_mat_eli_basura5[[2]]
ES_M_mat_eli_basuraEnt5 <- M_mat_eli_basuraEnt5[[3]]

ES_M_mat_eli_basura6 <- M_mat_eli_basura6[[2]]
ES_M_mat_eli_basuraEnt6 <- M_mat_eli_basuraEnt6[[3]]

# Creamos la base a mostrar
c_ent_ES22 <- data.frame(c(ES_M_mat_eli_basura, ES_M_mat_eli_basuraEnt), c(ES_M_mat_eli_basura1 ,ES_M_mat_eli_basuraEnt1), c(ES_M_mat_eli_basura2 ,ES_M_mat_eli_basuraEnt2), c(ES_M_mat_eli_basura3 ,ES_M_mat_eli_basuraEnt3), c(ES_M_mat_eli_basura4 ,ES_M_mat_eli_basuraEnt4), c(ES_M_mat_eli_basura5 ,ES_M_mat_eli_basuraEnt5), c(ES_M_mat_eli_basura6 ,ES_M_mat_eli_basuraEnt6))

# Agregamos nombres
colnames(c_ent_ES22) <- c("Sí", "No")
row.names(c_ent_ES22)<- Entidades
c_ent_ES22
##################################################################
#### 1.23 "VIVIENDAS POR ENTIDAD FEDERATIVA, SEGÚN TIPO DE TENENCIA DE LA VIVIENDA"													

### establecemos variables
M_mat_tenencia  <-svytotal(~tenencia==1, mydesign, na.rm=TRUE)#Total promedio
M_mat_tenenciaEnt <- svyby(~tenencia==1, by=~ent,mydesign,svytotal, na.rm=TRUE) # Estatal promedio

M_mat_tenencia1  <-svytotal(~tenencia==2, mydesign, na.rm=TRUE)#Total promedio
M_mat_tenenciaEnt1 <- svyby(~tenencia==2, by=~ent,mydesign,svytotal, na.rm=TRUE) # Estatal promedio

M_mat_tenencia2  <-svytotal(~tenencia==3, mydesign, na.rm=TRUE)#Total promedio
M_mat_tenenciaEnt2 <- svyby(~tenencia==3, by=~ent,mydesign,svytotal, na.rm=TRUE) # Estatal promedio

M_mat_tenencia3  <-svytotal(~tenencia==4, mydesign, na.rm=TRUE)#Total promedio
M_mat_tenenciaEnt3 <- svyby(~tenencia==4, by=~ent,mydesign,svytotal, na.rm=TRUE) # Estatal promedio

M_mat_tenencia4  <-svytotal(~tenencia==5, mydesign, na.rm=TRUE)#Total promedio
M_mat_tenenciaEnt4 <- svyby(~tenencia==5, by=~ent,mydesign,svytotal, na.rm=TRUE) # Estatal promedio

M_mat_tenencia5  <-svytotal(~tenencia==6, mydesign, na.rm=TRUE)#Total promedio
M_mat_tenenciaEnt5 <- svyby(~tenencia==6, by=~ent,mydesign,svytotal, na.rm=TRUE) # Estatal promedio


ES_M_mat_tenencia <- M_mat_tenencia[[2]]
ES_M_mat_tenenciaEnt <- M_mat_tenenciaEnt[[3]]

ES_M_mat_tenencia1 <- M_mat_tenencia1[[2]]
ES_M_mat_tenenciaEnt1 <- M_mat_tenenciaEnt1[[3]]

ES_M_mat_tenencia2 <- M_mat_tenencia2[[2]]
ES_M_mat_tenenciaEnt2 <- M_mat_tenenciaEnt2[[3]]

ES_M_mat_tenencia3 <- M_mat_tenencia3[[2]]
ES_M_mat_tenenciaEnt3 <- M_mat_tenenciaEnt3[[3]]

ES_M_mat_tenencia4 <- M_mat_tenencia4[[2]]
ES_M_mat_tenenciaEnt4 <- M_mat_tenenciaEnt4[[3]]

ES_M_mat_tenencia5 <- M_mat_tenencia5[[2]]
ES_M_mat_tenenciaEnt5 <- M_mat_tenenciaEnt5[[3]]


# Creamos la base a mostrar
c_ent_ES23 <- data.frame(c(ES_M_mat_tenencia, ES_M_mat_tenenciaEnt), c(ES_M_mat_tenencia1 ,ES_M_mat_tenenciaEnt1), c(ES_M_mat_tenencia2 ,ES_M_mat_tenenciaEnt2), c(ES_M_mat_tenencia3 ,ES_M_mat_tenenciaEnt3), c(ES_M_mat_tenencia4 ,ES_M_mat_tenenciaEnt4), c(ES_M_mat_tenencia5 ,ES_M_mat_tenenciaEnt5))

# Agregamos nombres
colnames(c_ent_ES23) <- c("Sí", "No")
row.names(c_ent_ES23)<- Entidades
c_ent_ES23

##################################################################

#### 1.24 VIVIENDAS PROPIAS POR ENTIDAD FEDERATIVA, SEGÚN FORMA DE ADQUISICIÓN													

### establecemos variables
M_mat_tipo_adqui  <-svytotal(~tipo_adqui==1, mydesign, na.rm=TRUE)#Total promedio
M_mat_tipo_adquiEnt <- svyby(~tipo_adqui==1, by=~ent,mydesign,svytotal, na.rm=TRUE) # Estatal promedio

M_mat_tipo_adqui1  <-svytotal(~tipo_adqui==2, mydesign, na.rm=TRUE)#Total promedio
M_mat_tipo_adquiEnt1 <- svyby(~tipo_adqui==2, by=~ent,mydesign,svytotal, na.rm=TRUE) # Estatal promedio

M_mat_tipo_adqui2  <-svytotal(~tipo_adqui==3, mydesign, na.rm=TRUE)#Total promedio
M_mat_tipo_adquiEnt2 <- svyby(~tipo_adqui==3, by=~ent,mydesign,svytotal, na.rm=TRUE) # Estatal promedio

M_mat_tipo_adqui3  <-svytotal(~tipo_adqui==4, mydesign, na.rm=TRUE)#Total promedio
M_mat_tipo_adquiEnt3 <- svyby(~tipo_adqui==4, by=~ent,mydesign,svytotal, na.rm=TRUE) # Estatal promedio


ES_M_mat_tipo_adqui <- M_mat_tipo_adqui[[2]]
ES_M_mat_tipo_adquiEnt <- M_mat_tipo_adquiEnt[[3]]

ES_M_mat_tipo_adqui1 <- M_mat_tipo_adqui1[[2]]
ES_M_mat_tipo_adquiEnt1 <- M_mat_tipo_adquiEnt1[[3]]

ES_M_mat_tipo_adqui2 <- M_mat_tipo_adqui2[[2]]
ES_M_mat_tipo_adquiEnt2 <- M_mat_tipo_adquiEnt2[[3]]

ES_M_mat_tipo_adqui3 <- M_mat_tipo_adqui3[[2]]
ES_M_mat_tipo_adquiEnt3 <- M_mat_tipo_adquiEnt3[[3]]

# Creamos la base a mostrar
c_ent_ES24 <- data.frame(c(ES_M_mat_tipo_adqui, ES_M_mat_tipo_adquiEnt), c(ES_M_mat_tipo_adqui1 ,ES_M_mat_tipo_adquiEnt1), c(ES_M_mat_tipo_adqui2 ,ES_M_mat_tipo_adquiEnt2), c(ES_M_mat_tipo_adqui3 ,ES_M_mat_tipo_adquiEnt3))

# Agregamos nombres
colnames(c_ent_ES24) <- c("Sí", "No")
row.names(c_ent_ES24)<- Entidades
c_ent_ES24
##################################################################
#### 1.25 "VIVIENDAS PROPIAS COMPRADAS YA HECHAS POR ENTIDAD FEDERATIVA,  SEGÚN CONDICIÓN DE SER USADA"																			

M_mat_viv_usadae  <-svytotal(~viv_usada==1, mydesign, na.rm=TRUE) #Total promedio
M_mat_viv_usadaeEnt <- svyby(~viv_usada==1, by=~ent,mydesign,svytotal, na.rm=TRUE) # Estatal promedio

M_mat_viv_usadae1  <-svytotal(~viv_usada==2, mydesign, na.rm=TRUE)#Total promedio
M_mat_viv_usadaeEnt1 <- svyby(~viv_usada==2, by=~ent,mydesign,svytotal, na.rm=TRUE) # Estatal promedio


ES_M_mat_viv_usadae <- M_mat_viv_usadae[[2]]
ES_M_mat_viv_usadaeEnt <- M_mat_viv_usadaeEnt[[3]]

ES_M_mat_viv_usadae1 <- M_mat_viv_usadae1[[2]]
ES_M_mat_viv_usadaeEnt1 <- M_mat_viv_usadaeEnt1[[3]]


# Creamos la base a mostrar
c_ent_ES25 <- data.frame(c(ES_M_mat_viv_usadae, ES_M_mat_viv_usadaeEnt), c(ES_M_mat_viv_usadae1 ,ES_M_mat_viv_usadaeEnt1))

# Agregamos nombres
# Agregamos nombres
colnames(c_ent_ES25) <- c("Sí", "No")
row.names(c_ent_ES25)<- Entidades
c_ent_ES25


#######
# 2.1 HOGARES QUE EN LOS ÚLTIMOS TRES MESES EXPERIMENTARON DIFICULTADES PARA SATISFACER SUS NECESIDADES ALIMENTARIAS, 
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