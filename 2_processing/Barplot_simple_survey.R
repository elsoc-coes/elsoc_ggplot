library(ggplot2)
library(survey)
library(dplyr)
library(reshape2)
library(magrittr)
library(ggthemes)

######Abrir BBDD######
# BBDD LONG
load("~/Desktop/ELSOC/dataverse_files/Datasets/ELSOC_Long_2016_2019_labelled_v2.00.RData")

#BBDD WIDE
load("~/Desktop/ELSOC/dataverse_files/Datasets/ELSOC_Wide_2016_2019_v1.00_R.RData")

elsoc_long_2016_2019[elsoc_long_2016_2019==-999 | elsoc_long_2016_2019==-888] <- NA

elsoc_wide_2016_2019[elsoc_wide_2016_2019 == -888 | elsoc_wide_2016_2019 == -999] <- NA

elsoc_panel <- elsoc_long_2016_2019 %>% filter(tipo_atricion == 1 | tipo_atricion == 10)
elsoc_panel_m1 <- dplyr::filter(elsoc_long_2016_2019, muestra == 1 & tipo_atricion == 1)


####### Recode Variables que se utilizarán ###########
#ola
elsoc_panel_m1$ola <- factor(elsoc_panel_m1$ola,labels = c('2016', '2017', '2018', '2019'))
#sexo
elsoc_panel_m1$m0_sexo <- factor(elsoc_panel_m1$m0_sexo,labels = c('Hombre', 'Mujer'))
#edad
elsoc_panel_m1$edad <- factor(car::recode(elsoc_panel_m1$m0_edad, "18:29=1;30:49=2;50:64=3;65:150=4"),
                                    labels = c('18-29', '30-49', '50-64', '65 o más'))
#zona
elsoc_panel_m1$zona  <- car::recode(elsoc_panel_m1$region,"c('Tarapaca','Antofagasta','Atacama','Coquimbo','Arica')= 1;c('Valparaiso','Lib. Gral. B. Ohiggins','B. Ohiggins', 'Maule','Bio Bio')= 2;c('Araucania','Los Lagos','Aysen','Magallanes','Los Rios')=3 ;'Metropolitana'= 4")
elsoc_panel_m1$zona  <- factor(elsoc_panel_m1$zona,levels=c("1","2","3","4"), labels = c("Norte","Centro","Sur","Metropolitana"))

#estatus
elsoc_panel_m1$estatus<- factor(car::recode(elsoc_panel_m1$d01_01, "0:4=1;5=2;6:10=3"),
                            labels = c('Bajo','Medio','Alto'))                          
attr(elsoc_panel_m1$estatus, which = 'label') <- 'Estatus Social Subjetivo'

#empleo
elsoc_panel_m1$empleo <- car::recode(elsoc_panel_m1$m02,"c(1,2,3) = 1; 7 = 2; 6 = 3; 5 = 4; c(4, 8, 9) = 5")
elsoc_panel_m1$empleo <- factor(elsoc_panel_m1$empleo, 
                            labels = c("Trabajo remunerado", "Trabajo doméstico no remunerado", "Desempleado/a", "Jubilado/a o pensionado/a", "Otras categorías"))

#perception de merito
elsoc_panel_m1$d05_01 <- factor(elsoc_panel_m1$d05_01,labels = c('Nada Importante', 'Poco Importante', 'Algo Importante', 'Bastante Importante','Muy Importante' ))
elsoc_panel_m1$d05_02 <- factor(elsoc_panel_m1$d05_02,labels = c('Nada Importante', 'Poco Importante', 'Algo Importante', 'Bastante Importante','Muy Importante' ))
elsoc_panel_m1$d05_03 <- factor(elsoc_panel_m1$d05_03,labels = c('Nada Importante', 'Poco Importante', 'Algo Importante', 'Bastante Importante','Muy Importante' ))
elsoc_panel_m1$d05_04 <- factor(elsoc_panel_m1$d05_04,labels = c('Nada Importante', 'Poco Importante', 'Algo Importante', 'Bastante Importante','Muy Importante' ))


#### Preparamos el diseño de la encuesta longitudinal ####
d_els <- svydesign(ids = ~idencuesta, strata = ~estrato, weights = ~ponderador02, nest = TRUE,
                     data = elsoc_panel_m1)
##revisar ids

p_04 <- svytable(~d05_01 + ola + m0_sexo + edad + empleo + zona + estatus, d_els, round = T)
#Hacemos una tabla ponderada para la variables de interés
p_d05 <- data.frame(prop.table((svytable(~d05_01 + ola + m0_sexo + edad + empleo + zona + estatus, d_els, round = T))))

#Estimación en subpoblaciones
#Función svyby: Por ejemplo, si queremos estimar [d05_01] [d05_02] [d05_03] [d05_04] para cada ola

d05table <- svyby(~d05_01 + d05_02 + d05_03 + d05_04, ~ola, d_els, na.rm = T, svymean)
#ojo que ahora tengo porcentual, pero si cambiamos a svytotal , entrega frecuencia absoluta.

#Ahora tenemos dos instrumentos para armar gráficos.
#p_d05 es una tabla donde sólo usa UNA variable y ordenamos la frecuencia en MUCHAS variables de categoría
#d05table es una tabla donde se usan MUCHAS variables y ordenamos frecuencia en UNA variables de categoría


p <- ggplot(p_d05, aes(d05_01, weight = Freq, fill = edad))
p + geom_bar() 
p + aes(fill = ola) + geom_bar(position = "dodge")
p + aes(fill = m0_sexo) + geom_bar(position = "fill")

q <- ggplot(p_d05, aes(edad, weight = Freq, fill = d05_01))
q + geom_bar() + scale_y_continuous(labels = scales::percent, limits = c(0, 1))

 

plot(svytable(~d05_02 + m0_sexo, d_els), main = "mosaic plot de s03 y género")

svyboxplot(m13 ~ d05_04, d_els, main = "d05_04 según ingreso")


