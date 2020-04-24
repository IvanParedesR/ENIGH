# Cuadro1
# Ingreso corriente total promedio trimestral por hogar en deciles de hogares y su coeficiente de GINI
# 2018
# limpia la pantalla de tablas o basura de un ejercicio anterior
rm(list = ls())
# carga lista de librerías que necesitaremos
library(foreign) # librería que nos ayuda a leer las tablas en diferentes formatos
library(doBy) # librería que nos permite ordenar los datos de la tabla según el ingreso
library(reldist) # librería que incluye la función para el cálculo del GINI
# establece el directorio donde se encuentran nuestras bases de datos

# abrimos la tabla concentradohogar
Conc<- read.dbf("~/ENIGH/concentradohogar.dbf",as.is = T)
# selección de las variables de interés
Conc <- Conc [ c("folioviv", "foliohog", "ing_cor", "ingtrab", "trabajo", "negocio", "otros_trab", "rentas", "utilidad",
                 "arrenda", "transfer", "jubilacion", "becas", "donativos", "remesas", "bene_gob", "transf_hog", "trans_inst",
                 "estim_alqu", "otros_ing","factor","upm","est_dis")]
# se crea una variable para agregar la entidad federativa
Conc$entidad <- substr(Conc$folioviv,1,2)
# se define la columna con el nombre de las entidades federativas
Numdec<-c("Total", "I", "II", "III","IV", "V", "VI", "VII", "VIII", "IX","X")
# se crea una bandera para numerar a los hogares
Conc$Nhog <- 1
##################### DECILES DE INGRESO ###################
# deja activa la tabla Conc
attach(Conc)
# ordena Conc de acuerdo a ing_cor, folioviv, foliohog.
Conc<- orderBy (~+ing_cor+folioviv+foliohog, data=Conc)

# suma todos los factores y guarda el valor en el vector tot_hogares.
tot_hogares <- sum(factor,to.data.frame=TRUE)

# se divide la suma de factores entre diez para sacar el tamaño del decil
# se debe de truncar el resultado quitando los decimales.
tam_dec<-trunc(tot_hogares/10)
# muestra la suma del factor en variable hog.
Conc$tam_dec=tam_dec

############### CREACION DE DECILES DE INGRESO #######################
# se renombra la tabla concentrado a BD1.
BD1 <- Conc

# dentro de la tabla BD1 se crea la variable MAXT y se le asigna los valores que tienen el ing_cor.
BD1$MAXT<-BD1$ing_cor
# se ordena de menor a mayor según la variable MAXT.
BD1<-BD1[with(BD1, order(rank(MAXT))),]
# se aplica la función cumsum, suma acumulada a la variable factor.
BD1$ACUMULA<-cumsum(BD1$factor)
# entra a un ciclo donde iremos generando los deciles 1 a 10.
for(i in 1:9)
{
  a1<-BD1[dim(BD1[BD1$ACUMULA<tam_dec*i,])[1]+1,]$factor
  BD1<-rbind(BD1[1:(dim(BD1[BD1$ACUMULA<tam_dec*i,])[1]+1),],
             BD1[(dim(BD1[BD1$ACUMULA<tam_dec*i,])[1]+1):dim(BD1[1])[1],])
  b1<-tam_dec*i-BD1[dim(BD1[BD1$ACUMULA<tam_dec*i,])[1],]$ACUMULA
  BD1[(dim(BD1[BD1$ACUMULA<tam_dec*i,])[1]+1),]$factor<-b1
  BD1[(dim(BD1[BD1$ACUMULA<tam_dec*i,])[1]+2),]$factor<-(a1-b1)
}
BD1$ACUMULA2<-cumsum(BD1$factor)
BD1$DECIL<-0
BD1[(BD1$ACUMULA2<=tam_dec),]$DECIL<-1
for(i in 1:9)
{
  BD1[((BD1$ACUMULA2>tam_dec*i)&(BD1$ACUMULA2<=tam_dec*(i+1))),]$DECIL<-(i+1)
}
BD1[BD1$DECIL%in%"0",]$DECIL<-10
##################################################################
# TOTAL HOGARES
x<-tapply(BD1$factor,BD1$Nhog,sum)

# DECILES
y<-tapply(BD1$factor,BD1$DECIL,sum)
# se calcula el promedio (ingreso entre los hogares) tanto para el total como para cada uno de los deciles
ing_cormed_t<-tapply(BD1$factor*BD1$ing_cor,BD1$Nhog,sum)/x
ing_cormed_d<-tapply(BD1$factor*BD1$ing_cor,BD1$DECIL,sum)/y
########################## C U A D R O S #################################
# guardamos los resultados en un data frame
prom_rub <- data.frame (c(ing_cormed_t,ing_cormed_d))
# agregamos el nombre a las filas
row.names(prom_rub)<-Numdec

############### Cálculo del GINI #############
# GINI Nacional (sobre los 10 deciles) por hogar usando el promedio del ingreso corriente (ingcor)
deciles_hog_ingcor <- data.frame(hogaresxdecil=c(x,x,x,x,x,x,x,x,x,x),
                                 ingreso=c(ing_cormed_d[1],ing_cormed_d[2],ing_cormed_d[3],
                                           ing_cormed_d[4],ing_cormed_d[5],ing_cormed_d[6],
                                           ing_cormed_d[7],ing_cormed_d[8],ing_cormed_d[9],
                                           ing_cormed_d[10]))
# se efectua la función Gini y se guarda en nuestro vector a.
a<-gini(deciles_hog_ingcor$ingreso,weights=deciles_hog_ingcor$hogares)
# se renombran las variables (columnas)
names(prom_rub)=c("INGRESO CORRIENTE")
names(a)="GINI"
##### Mostramos el resultado en pantalla #####
round(prom_rub)
round(a,3)