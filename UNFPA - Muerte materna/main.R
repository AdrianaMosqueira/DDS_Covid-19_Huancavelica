# install.packages("tidyr")
# install.packages("viridis")
# install.packages("hrbrthemes")
# install.packages("scales")
# install.packages("tidyverse")
# install.packages('ggthemes')
# install.packages("leaflet")
# install.packages('geojsonio')
# install.packages('plotrix')
# install.packages("leaflegend")

library(tidyr)
library(viridis)
library(plotly)
library(ggplot2)
#library(hrbrthemes)
library(scales)

library(tidyverse)
library("ggthemes")
library(gridExtra)
library(plotrix)
library(leaflet)
library(sf)
library(geojsonio)
library(RColorBrewer)
library(shiny)
library(forcats)

source("utils.R")


# Data reading
# Provided by Luis
f10 <- read.csv("data/intermediate/figura10.csv", dec = ",", header = TRUE, check.names = FALSE)
f11 <- read.csv("data/intermediate/figura11.csv", dec = ",", header = TRUE, check.names = FALSE)
f13 <- read.csv("data/intermediate/figura13.csv", dec = ",", header = TRUE, check.names = FALSE)
f14 <- read.csv("data/intermediate/figura14.csv", dec = ",", header = TRUE, check.names = FALSE)
f15 <- read.csv("data/intermediate/figura15.csv", dec = ",", header = TRUE, check.names = FALSE)

f12a <- read.csv("data/intermediate/figura12-beneficios.csv", dec = ",", header = TRUE, check.names = TRUE, na.strings = c("", "NA"))
f12b <- read.csv("data/intermediate/figura12-costos.csv", dec = ",", header = TRUE, check.names = TRUE, na.strings = c("", "NA"))
f16a <- read.csv("data/intermediate/figura16-beneficios.csv", dec = ",", header = TRUE, check.names = TRUE)
f16b <- read.csv("data/intermediate/figura16-costos.csv", dec = ",", header = TRUE, check.names = TRUE)

f17 <- read.csv("data/intermediate/figura17_typed.csv", dec = ",", header = TRUE, check.names = TRUE)
f17 <- f17 %>% mutate(valor = as.numeric(valor))


# Provided by Carla
f1 <- read.csv("data/intermediate/figura1.csv", dec = ",", header = TRUE, check.names = TRUE)
f4 <- read.csv("data/intermediate/figura4.csv", dec = ",", header = TRUE, check.names = FALSE)
f5 <- read.csv("data/intermediate/figura5.csv", dec = ",", header = TRUE, check.names = FALSE)
f7 <- read.csv("data/intermediate/figura7.csv", dec = ",", header = TRUE, check.names = FALSE)

# Maps Fig 3
#peruRegion <- geojson_sf("https://raw.githubusercontent.com/juaneladio/peru-geojson/master/peru_departamental_simple.geojson")
tab3 <- read.csv('data/intermediate/TablasF3.csv', header = TRUE, check.names = FALSE)
peruRegion <- geojson_sf('data/intermediate/peru_departamental_simple.geojson')

# Provided by Luis
f2 <- read.csv("data/intermediate/figura2.csv", dec = ",", header = TRUE, check.names = TRUE)
f6 <- read.csv("data/intermediate/figura6.csv", dec = ",", header = TRUE, check.names = FALSE)
f8 <- read.csv("data/intermediate/figura8.csv", dec = ",", header = TRUE, check.names = FALSE)
f9 <- read.csv("data/intermediate/figura9.csv", dec = ",", header = TRUE, check.names = FALSE)


# Formatting
data_f10 <- formattingStack(f10)
data_f11 <- formattingStack(f11)
data_f13 <- formattingStack(f13)
data_f14 <- formattingStack(f14)
data_f15 <- formattingStack(f15) 

data_f12a <- formattingStack2(f12a)
data_f12b <- formattingStack2(f12b)
data_f12a$tipo <- "Beneficio"
data_f12b$tipo <- "Costo"
data_f12 <- rbind(data_f12a, data_f12b)

data_f16a <- formattingStack3(f16a)
data_f16b <- formattingStack3(f16b)
data_f16a$tipo <- "Beneficio"
data_f16b$tipo <- "Costo"
data_f16 <- rbind(data_f16a, data_f16b)

data_f10_ <- data_f10 %>% mutate(newVar = recode(Variable, "Suplementación de hierro durante el embarazo" = "hierro",
                                                "Manejo de pre-eclampsia con sulfato de magnesio" = "magnesio",
                                                "Administración parenteral de anticonvulsivantes" = "anticonvulsionantes",
                                                "Retiro de productos de la concepción retenidos" = "concepción",
                                                "Transfusión de sangre" = "sangre",
                                                "Manejo de caso de trastorno hipertensivo" = "hipertensivo",
                                                "Retiro manual de placenta" = "placenta",
                                                "Administración parenteral de uterotónicos" = "uterotónicos",
                                                "Parto por cesárea" = "cesárea")) %>% select(Year, Value, newVar) %>% 
  pivot_wider(names_from = newVar, values_from = Value)

data_f11_ <- data_f11 %>% mutate(newVar = recode(Variable, "Suplementación de hierro durante el embarazo" = "hierro_",
                                                 "Manejo de pre-eclampsia con sulfato de magnesio" = "magnesio_",
                                                 "Administración parenteral de anticonvulsivantes" = "anticonvulsionantes_",
                                                 "Retiro de productos de la concepción retenidos" = "concepción_",
                                                 "Transfusión de sangre" = "sangre_",
                                                 "Manejo de caso de trastorno hipertensivo" = "hipertensivo_",
                                                 "Retiro manual de placenta" = "placenta_",
                                                 "Administración parenteral de uterotónicos" = "uterotónicos_",
                                                 "Parto por cesárea" = "cesárea_")) %>% select(Year, Value, newVar) %>% 
  pivot_wider(names_from = newVar, values_from = Value) %>% inner_join(data_f10_, by = "Year")


data_f13_ <- data_f13 %>% select(Year, Value, Variable) %>% pivot_wider(names_from = Variable, values_from = Value) 
data_f14_ <- data_f14 %>% select(Year, Value, Variable) %>% pivot_wider(names_from = Variable, values_from = Value) 
data_f15_ <- data_f15 %>% select(Year, Value, Variable) %>% pivot_wider(names_from = Variable, values_from = Value) 

data_f131415 <- data_f13_ %>% inner_join(data_f14_,  by = "Year") %>% inner_join(data_f15_,  by = "Year")

data_f1 <- f1
#data_f4 <- formattingMM(f4)
data_f4 <- formattingMML(f4)
data_f5 <- formattingMM5(f5)
data_f7 <- formattingAC(f7)

data_f3 <- formattingGeo(peruRegion, tab3)

data_f2 <- formattingL(f2)
data_f6 <- formattingL(f6)

data_f8 <- formattingMAC(f8)
data_f8$ejex <- seq(1,21,5)
data_f8$ejex[data_f8$ejex==21] <- 20

data_f9 <- formattingLL(f9)


# Plotting: nbreaks number of horizontal lines
# Cada línea se deben colocar en la parte del documento que corresponda
# Fig 10
#customPl <- c(color1, color2, color3, color4, color5, color6, color7, color8, color9)
#customTh <- customTheme(fontf = "Arial", lgpos = "bottom", lgtxtsize=9, lgbox="horizontal")
#plotStack(data =  data_f10, baseTheme=theme_minimal(), customTh, customPalette=customPl, yLimSup = 30000000)

# Fig 11
#plotStack(data =  data_f11, baseTheme=theme_minimal(), customTh, customPalette=customPl, yLimSup = 30000000)


# Fig 13
#plotStack(data =  data_f13, nbreaks =  6, fontf = "Arial")

# Fig 14
#plotStack(data =  data_f14, nbreaks =  6, fontf = "Arial")

# Fig 15
#plotStack(data =  data_f15, nbreaks =  3, fontf = "Arial")

# Fig 12
customPl <- c(color1, color2, color3, color4, color5, color6, color7, color8, color9, color10, color11)
customTh <- customTheme(fontf = "Arial", txsize=10, lgpos = "bottom", lgtxtsize=8, lgbox="horizontal",
                        titlesize = 11)
plotStack2(data =  data_f12, baseTheme=theme_minimal(), customTh, customPalette=customPl)

# Fig 16
customPl <- c(color1, color2, color3, color4, color5, color6, color7, color8, color9, color10, color11, color12)
plotStack2(data = data_f16, baseTheme=theme_minimal(), customTh, customPalette=customPl)

# Fig 1
customTh <- customTheme(fontf = "Arial", lgpos="none")
fig1 <- plotggLine(data = data_f1, colorLine=color1, linesize=0.7, baseTheme=theme_minimal(), 
           customTheme=customTh, colord1=color9, colord2=color2, colord3=color10, 
           colorseg1 = color2, colorseg2 = color12)
plotlyLine(fig1, asize = 13, fontf = "Arial", color9, color2, color10, 
           colorLine= color1, colorseg1=color2, colorseg2=color12)

# Fig 4 static(plotPyramid) and dynamic(plotPyramidB) versions
#plotPyramid(data = data_f4, title = "Distribución de muertes maternas")
#plotPyramidBB(data = data_f4, baseTheme=theme_minimal(), customTheme=customTh, 
#              colorB =c("plum1", "darkmagenta"))
customTh <- customTheme(fontf = "Arial", lgpos = "bottom")
plotPyramidBBL(data = data_f4, fontf="Arial", fsize=11, pal =c(color4, color1))

# Fig 5 Pyramid and Stack versions por revisar
# plotPyramidB(data = data_f5, font="Arial", 
#              titleP="Distribución de muertes maternas según causa de fallecimiento", 
#              variable = "Causas", labelbreaks = seq(0,  60, by = 10))
customTh <- customThemeMM(fontf = "Arial", txsize=10, lgpos = "bottom")
plotStackMM(data = data_f5, baseTh=theme_minimal(), customTh=customTh, colorB =c("plum1", "darkmagenta"))

# Fig 7 Multiline and Stack versions add bar x: pais, y methodos
#plotLineMult(data = data_f7, nbreaks =  3, fontf = "Arial")
customTh <- customThemeMM(fontf = "Arial", txsize=10, lgpos = "bottom")
plotStackAC(data = data_f7, baseTh=theme_minimal(), customTh=customTh, colorB =c("darkmagenta", "plum1"), linecolor='blue')


# Fig 2 Multiple lines
customPl <- c(color1, color2, color3, color4, color5, color6, color7, color8, color9, color10)
plotMultLine(data_f2, pal = customPl)

# Fig 6 Multiple lines
customPl <- c(color1, color2, color3, color4, color5, color6, color7, color8, color9, color10)
plotMultLine(data_f6, pal = customPl)

# Fig 8
customPl <- c(color1, color2, color3, color4, color5, color6, color7, color8, color9, color10)
plotMultLineB(data_f8, pal = customPl)

# Fig 9
customPl <- c(color1, color2)
plotMultLineF(data_f9, pal = customPl)


# Maps Fig 3
plotMapPeru(data_f3, fontf = "Arial", paletteC='YlOrRd')

