install.packages("survival")
install.packages("KMsurv")
install.packages("survMisc")
install.packages("survminer")
install.packages("flexsurv")
install.packages("actuar")
install.packages("dplyr")

library(survival)
library(KMsurv)
library(survMisc)
library(survminer)
library(ggfortify)
library(flexsurv)
library(actuar)
library(dplyr)
library(tidyverse)
library(skimr)

load("F:/Entretención/PUC/Diplomado PUC/PROYECTO/NOVIEMBRE/PROYECTO FINAL/Titulados.Rdata")

#CONSEGUIR LA VARIABLE CENSURA----
##CENSURA: CAMBIO DE ESTADO
0: INDIVIDUOS QUE NO SE GRADUARON OPORTUNAMENTE/EFECTIVA
1: INDIVIDUOS QUE SE GRADUARON OPORTUNAMENTE (HASTA 1 AÑO PASADOS DE LA DURACION REAL)


table(tit0720$TIPO_TITULACION)
#NUESTRA TRANSFORMACIÓN SERA SOBRE LA VARIABLE TIPO DE TITULACIÓN
#1° ELIMINAMOS LOS "SIN INFORMACIÓN"

filter(tit0720, TIPO_TITULACION=="SIN INFORMACION")
TIT0720v.2 <- filter(tit0720, !TIPO_TITULACION=="SIN INFORMACION")

table(TIT0720v.2$TIPO_TITULACION)

#2° Creo la variable censura con mutate, y luego a factor pasandolo por numeric


TIT0720v.3 <- mutate(TIT0720v.2, CENSURA = TIPO_TITULACION)

TIT0720v.3$CENSURA <- factor(TIT0720v.3$CENSURA,
                              levels=c("EFECTIVA","EXACTA","OPORTUNA"),
                              labels = c("0","1","1"))

table(TIT0720v.3$CENSURA) #REVISO SI SE CUMPLIO LA RECODIFICAION
str(TIT0720v.3$CENSURA) #VEO QUE SIGUE COMO FACTOR
TIT0720v.3$CENSURA <- as.numeric(TIT0720v.3$CENSURA) #LO TRANSFORMO A NUMERIC
#LA WEA NO SE PORQUE EN VEZ DE 0,1 ME LO DEJO EN 1,2
TIT0720v.3$CENSURA[TIT0720v.3$CENSURA == 1] <- 0  #LO VUELVO A 0 EL 1
TIT0720v.3$CENSURA[TIT0720v.3$CENSURA == 2] <- 1  #LO VUELVO A 2 EL 1

table(TIT0720v.3$CENSURA)  #REVISO
str(TIT0720v.3$CENSURA) #REVISO SI SIGUE NUMERIC Y SI BB

#3° CREO LA FUNCION DE SUPERVIVENCIA EN FUNCION DEL TIEMPO (TIEMPO DE OBTENCION TITULO) Y LA CENSURA


# Guardando el Objeto Surv
tongue.surv <- Surv(TIT0720v.3$TIEMPO_OBT_TIT, TIT0720v.3$CENSURA)  #Creando objeto tipo Surv
tongue.km <- survfit(tongue.surv ~ 1, data = TIT0720v.3, type = "kaplan-meier")  #Estimación Kaplan Meier

# Sin Guardar el Objeto Surv
tongue.km <- survfit(Surv(TIEMPO_OBT_TIT, CENSURA) ~ 1, data = TIT0720v.3, type = "kaplan-meier")
#TABLA DE FUNCION DE SUPERVIVENCIA
summary(tongue.km)
S#RESUMEN ESTADISTICO DONDE EL TIEMPO NO PUEDE SER 0
#skim(TIT0720v.3$TIEMPO_OBT_TIT)

#ggsurvplot(fit = tongue.km, data = TIT0720v.3, conf.int = T, title = "Curva de Supervivencia", 
           xlab = "Tiempo", ylab = "Probabilidad de supervivencia", legend.title = "Estimación", 
           legend.labs = "Kaplan-Meier")


#CONCLUSIONES:
1) ELIMINAR EL TIEMPO MENOR A 2 AÑOS, ES ILOGICO. Y MAYOR A 10 AÑOS DONDE YA LA FUNCION SE VUELVE CONSTANTE


table(TIT0720v.3$TIEMPO_OBT_TIT)

#cambiar el tiempo

TIT0720v.4 <- TIT0720v.3 %>%  filter(TIEMPO_OBT_TIT>=1 & TIEMPO_OBT_TIT<=8) 

table(TIT0720v.4$CENSURA)





#TODO DE NUEVO


#3° CREO LA FUNCION DE SUPERVIVENCIA EN FUNCION DEL TIEMPO (TIEMPO DE OBTENCION TITULO) Y LA CENSURA


# Guardando el Objeto Surv
tongue.surv <- Surv(TIT0720v.4$TIEMPO_OBT_TIT, TIT0720v.4$CENSURA)  #Creando objeto tipo Surv
tongue.km <- survfit(tongue.surv ~ 1, data = TIT0720v.4, type = "kaplan-meier")  #Estimación Kaplan Meier

# Sin Guardar el Objeto Surv
tongue.km <- survfit(Surv(TIEMPO_OBT_TIT, CENSURA) ~ 1, data = TIT0720v.4, type = "kaplan-meier")
#TABLA DE FUNCION DE SUPERVIVENCIA
summary(tongue.km)


ggsurvplot(fit = tongue.km, data = TIT0720v.4, conf.int = T, title = "Curva de Supervivencia", 
           xlab = "Tiempo", ylab = "Probabilidad de supervivencia", legend.title = "Estimación", 
           legend.labs = "Kaplan-Meier")







#4° ahora filtro por edad, sexo, etc----



summary(TIT0720v.4)
#borrar todas las incoherencias posibles
TIT0720v.4 <- filter(TIT0720v.4, !AÑO_ING_PRI_AÑO >2021)  #borrar todos los años de ingreso mayor a 2021
TIT0720v.5 <- filter(TIT0720v.4, !AÑO_ING_CARR >2021)  #borrar todos los años de ingreso mayor a 2021

table(TIT0720v.5$AÑO_ING_CARR)

summary(TIT0720v.5)
table(TIT0720v.5$MODALIDAD)
TIT0720v.6 <- filter(TIT0720v.5, MODALIDAD =="PRESENCIAL")  #borrar Y DEJAR SOLO PRESENCIAL
TIT0720v.7 <- filter(TIT0720v.6, TIPO_PLAN_CARR =="PLAN REGULAR")  #borrar Y DEJAR SOLO PLAN REGULAR
TIT0720v.8 <- filter(TIT0720v.7, JORNADA =="DIURNO")  #borrar Y DEJAR SOLO DIURNO
TIT0720v.9 <- filter(TIT0720v.8, !NIVEL_CARRERA_2 =="DOCTORADO")  #borrar DOCTORADO
TIT0720v.10 <- filter(TIT0720v.9, !NIVEL_CARRERA_2 =="MAGISTER")  #borrar MAGISTER
TIT0720v.11 <- filter(TIT0720v.10, !NIVEL_CARRERA_2 =="POSTÍTULO")  #borrar POSTITULO
TIT0720v.11 <- filter(TIT0720v.11, !RANGO_EDAD =="SIN INFORMACIÓN")  #borrar POSTITULO
TIT0720v.11 <- filter(TIT0720v.11, !RANGO_EDAD =="15 A 19 AÑOS")  #borrar POSTITULO
TIT0720v.11 <- filter(TIT0720v.11, !TIPO_INST_3 =="CENTROS DE FORMACIÓN TÉCNICA ESTATAL")  #borrar POSTITULO

table(TIT0720v.11$NIVEL_GLOBAL) #ESTAMOS OK 
table(TIT0720v.11$NIVEL_CARRERA_2) #ESTAMOS OK 

#SEXO
TIT0720v.11$GEN_ALU <- factor(ifelse(TIT0720v.11$GEN_ALU ==1, "Hombre", "Mujer"))

km_trt_fit <- survfit(Surv(TIEMPO_OBT_TIT, CENSURA) ~ GEN_ALU, data=TIT0720v.11)
summary(km_trt_fit)
autoplot(km_trt_fit, xlab = "Tiempo", ylab = "Probabilidad de supervivencia",
         main = "Curva de Supervivencia en el sexo") +
  theme(panel.border = element_rect(fill = "transparent"))

#RANGO EDAD
RANGO_EDAD <- survfit(Surv(TIEMPO_OBT_TIT, CENSURA) ~ RANGO_EDAD, data=TIT0720v.11)
summary(RANGO_EDAD)
autoplot(RANGO_EDAD, xlab = "Tiempo", ylab = "Probabilidad de supervivencia",
         main = "Curva de Supervivencia en el edad") +
  theme(panel.border = element_rect(fill = "transparent"))
  

#TIPO INSTITUCIÓN
RANGO_INSTITUCION <- survfit(Surv(TIEMPO_OBT_TIT, CENSURA) ~ TIPO_INST_3, data=TIT0720v.11)
summary(RANGO_INSTITUCION)
autoplot(RANGO_INSTITUCION, xlab = "Tiempo", ylab = "Probabilidad de supervivencia",
         main = "Curva de Supervivencia institución superior") +
  theme(panel.border = element_rect(fill = "transparent"))

#TIPO NIVEL GLOBAL
RANGO_GLOBAL <- survfit(Surv(TIEMPO_OBT_TIT, CENSURA) ~ NIVEL_CARRERA_2, data=TIT0720v.11)
summary(RANGO_GLOBAL)
autoplot(RANGO_GLOBAL,  xlab = "Tiempo", ylab = "Probabilidad de supervivencia",
         main = "Curva de Supervivencia nivel de carrera") +
  theme(panel.border = element_rect(fill = "transparent"))

#TIPO NIVEL OECD
RANGO_OECD <- survfit(Surv(TIEMPO_OBT_TIT, CENSURA) ~ OECD_AREA, data=TIT0720v.11)
summary(RANGO_OECD)
autoplot(RANGO_OECD, xlab = "Tiempo", ylab = "Probabilidad de supervivencia",
         main = "Curva de Supervivencia áreas OECD") +
  theme(panel.border = element_rect(fill = "transparent"))

##--------------------------
  
TIT0720v.12 <- filter(TIT0720v.11, OECD_AREA =="AGRICULTURA")  #borrar POSTITULO
table(TIT0720v.12$NOMB_CARRERA)



