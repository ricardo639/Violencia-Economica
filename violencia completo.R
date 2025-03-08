library(tidyverse)
library(lme4)
library(lmerTest)
library(readxl)
library(jtools)
library(sjPlot)
library(broom)
library(readstata13)
library(haven)
library(lmtest)
library(readstata13)
library(merTools)
library(jtools)   
library(lmerTest) 
library(dplyr)
library(ggplot2)
library(readxl)
library(arm)
library(lavaan)  
library(readr)
VIOELENCIA_ECONOMICA <- read_csv("UNIVERSIDAD/SEMESTRES/SEPTIMO SEMESTRE/ECONOMETRIA/PROYECTO FINAL/VIOELENCIA ECONOMICA.csv")
View(VIOELENCIA_ECONOMICA)
violencia <- as_tibble(VIOELENCIA_ECONOMICA )

colnames(violencia)
modelo<-'violencia=~GPEM+ ORDM + IGM'     ## La variable aleatoria se calculan con =~
fit <- sem(modelo,data=violencia,estimator="ML")        ## La funcion sem ajusta el modelo 

summary(fit,fit.measures=T)           ## Resumen del ajuste


standardizedSolution(fit)             ## Cargas estandarizadas

##  MODELOS ##

# Modelo nulo con interceptos aleatorios 

M0 <- lmer(VIOLENCIAE  ~ 1 + (1|REGIONES), REML = F, data = violencia)
summary(M0)
summ(M0)


# Modelo completo  con interceptos aleatorios 

M1<- lmer(VIOLENCIAE  ~ 1 + IGM + GPEM + ORDM + (1|REGIONES), REML = F, data = violencia)
summary(M1)
summ(M1)

M2<- lmer(VIOLENCIAE  ~ 1 + IGM + IGH + (1+ORDH|REGIONES), REML = F, data = violencia)
summary(M2)
summ(M2)
 
#Modelo con educacion promedio de mujeres 

M3<- lmer(VIOLENCIAE  ~ 1 +  GPEM + (1|REGIONES), REML = F, data = violencia)
summary(M3)
summ(M3)


#Modelos con ingreso de mujeres 

M4<- lmer(VIOLENCIAE  ~ 1 + IGM  + (1|REGIONES), REML = F, data = violencia)
summary(M4)
summ(M4)


#Modelo con Opinion sobre los roles en mujeres 

M5<- lmer(VIOLENCIAE  ~ 1 + ORDM  + (1|REGIONES), REML = F, data = violencia)
summary(M5)
summ(M5)

# Primer Comparacion del modelo 
tab_model(M0, M1, M2, dv.labels = c("Modelo Nulo", "Modelo completo", "Modelo Ajustado"))

#Segunda comparacion del modelo 
tab_model(M2, M3,M4, dv.labels = c("Modelo  Educacion Promedio", "Modelo con Ingresos", "Modelo con Opinion de roles"))



## Ranking de paises por intercepto ##


## Estimadores de efectos aleatorios 
ranef(M2) #random effects
Estimadores <- ranef(M1) #Se guardan en forma de lista

Interceptos <- Estimadores[["REGIONES"]][["(Intercept)"]] # Acceden al vector de interceptos
x <- data.frame(Interceptos = Interceptos, 
                Paises = c("Zona NOROESTE", "Zona NORESTE", "Zona OCCIDENTE", "Zona ORIENTE",
                           "Zona CENTRONORTE", "Zona CENTROSUR", "Zona SUROESTE", "Zona SURESTE"))


# Error estandar 
SE <- se.ranef(M1) #extraer los errores estandar del modelo
SE <- as.data.frame(SE[["REGIONES"]]) #forma de lista para poder acceder a ese elemento


#agregamos los errores estandar a la base 
names(SE) <- c("SE_intercepto")
x$SE_intercepto <- SE$SE_intercepto
x





#Necesitamos calcular ahora intervalos de confianza 
x <- x %>% 
  mutate(int_low_95 = Interceptos - (1.96*SE_intercepto), 
         int_sup_95 = Interceptos + (1.96*SE_intercepto))

# Ordenar los datos por el valor de Interceptos
ranking <- x %>% 
  arrange(Interceptos) %>% 
  ggplot(aes(x = reorder(Paises, Interceptos), y = Interceptos)) +  # Cambié 'REGIONES' por 'Paises'
  geom_point(col = "steelblue", size = 2) +
  geom_errorbar(aes(ymin = int_low_95, ymax = int_sup_95)) +
  theme_minimal(base_size = 12) +
  labs(title = "VIOLENCIA ECONOMICA Y PATRIMONIAL POR REGION", 
       x = "REGIONES", 
       y = "Interceptos")

# Mostrar el gráfico
ranking

