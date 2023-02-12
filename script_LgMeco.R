##### Aves LgMeco####
#### diresctorio de trabajo####
getwd()
#setwd("C:/Users/alejandro.aparicio/Documents/Curso_IntroR/Dia5_practica/pr_LgMeco")

#### instalar y cargar librerias####
# install.packages("dplyr")
library(dplyr)
# install.packages("tidyverse")
library(tidyverse)
# library(utils)

####importar datos####

#leemos profundidad y cambiamos sus clases a date y numerico
prof<-read.csv("C:/Users/alejandro.aparicio/Documents/Curso_IntroR/Dia5_practica/02_dat/prof.csv",
         dec = ",",
         header = TRUE,
         sep = ";")

prof$Fecha <- as.Date(prof$Fecha, format = "%d/%m/%Y")#cambiamos el formato de fecha al correcto
str(prof)


#leemos aves, asi no salen las x delante de las fechas


aves<-read_delim("C:/Users/alejandro.aparicio/Documents/Curso_IntroR/Dia5_practica/02_dat/Aves_d.csv",
                 delim = ";")


#####explorar datos####

head(prof)
class(prof)
str(prof)
dim(prof)
ncol(prof)
nrow (prof)


head(aves)
class(aves)
str(aves)
dim(aves)
ncol(aves)
nrow (aves)


####ordenar datos tidy####
glimpse(aves)

aves <- aves %>% 
  mutate(
    across(2:ncol(aves), function(x) as.numeric(x))
  ) #cambiamos todas las colunmas, exceto la primera, a numerico

glimpse(aves)

#ponemos en tidy
aves_l <- aves %>% 
  pivot_longer(cols = c(2:ncol(aves)),
               names_to = "fecha",
               values_to = "conteo")
# cambiamos NA por 0
aves_l_cero <- aves_l %>%
  mutate(
    conteo = replace_na(conteo, 0)
  )

aves_l_cero$fecha <- as.Date(aves_l_cero$fecha, format = "%d/%m/%Y")#cambiamos el formato de fecha al correcto


table(aves_l$conteo, aves_l$Nombre)
view(aves_l_cero)

#seleccionamos una sp
Fulica_atra <- aves_l_cero%>%
  filter(Nombre=="Fulica atra")

#generamos otra coolumna
Fulica_atra_mes <- Fulica_atra %>%
  mutate(
    mes=months(fecha),
    year=format.Date(fecha, "%Y")
  )

labels(Fulica_atra_mes$mes)
unique(Fulica_atra_mes$mes) #se enumeran las niveles del factor

#lo convierto en factor y ordeno
Fulica_atra_mes$mes <- factor(Fulica_atra_mes$mes,levels = c("enero", "febrero", "marzo", "abril",  "mayo", "junio", "julio","agosto", "septiembre", "octubre", "noviembre", "diciembre"))

#represntacion grafica
ggplot(Fulica_atra_mes)+
  geom_smooth(aes(x=fecha, y=conteo,group=year,color=year),se = FALSE) +
  geom_point(aes(x=fecha, y=conteo,color=year), alpha=0.3) +
  facet_wrap(.~year, scales = "free")+
  ggtitle("Censos mensueales de fochas (Fulica atra) en la Laguna de Meco")+
  theme_gray()
  

ggplot(Fulica_atra_mes,
            aes (x=mes,y=conteo))+
  geom_boxplot()+
  geom_smooth(aes(group=year,color=year))+
  coord_cartesian(ylim = c(0, 200))

#seleccionamos una sp2
unique(aves_l_cero$Nombre)
Chlidonias_hybrida <- aves_l_cero%>%
  filter(Nombre=="Chlidonias hybrida")

#generamos otra coolumna
Chlidonias_hybrida_mes <- Chlidonias_hybrida %>%
  mutate(
    mes=months(fecha),
    year=format.Date(fecha, "%Y")
  )

ggplot(Chlidonias_hybrida_mes,
       aes (x=mes,y=conteo))+
  geom_boxplot()

graf_fumareles<-ggplot(Chlidonias_hybrida_mes)+
  geom_smooth(aes(x=fecha, y=conteo,group=year,color=conteo),se = FALSE) +
  geom_point(aes(x=fecha, y=conteo,color=conteo), alpha=0.3) +
  facet_wrap(.~year, scales = "free")+
  ggtitle("Censos anuales de Chlidonias_hybrida en la Laguna de Meco")+
  theme_light()+
  ggsave("graf_fumareles2.pdf")

####calcular biodiversidad mes año...####



#### relacionar profundidad con riqueza####



####representacion grafica####


