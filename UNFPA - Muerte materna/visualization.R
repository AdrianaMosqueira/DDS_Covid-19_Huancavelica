# Stacked Plots

# install.packages("tidyr")
# install.packages("viridis")
# install.packages("hrbrthemes")
# install.packages("scales")
# install.packages("tidyverse")
# install.packages('ggthemes')
# install.packages("leaflet")
# install.packages('geojsonio')
# install.packages('plotrix')

install.packages("leaflegend")


library(tidyr)
library(viridis)
library(plotly)
library(ggplot2)
library(hrbrthemes)
library(scales)

library(tidyverse)
library("ggthemes")

library(leaflet)
library(sf)
library(geojsonio)

source("utils.R")


# Data reading
f10 <- read.csv("data/intermediate/figura10.csv", dec = ",", header = TRUE, check.names = FALSE)
f11 <- read.csv("data/intermediate/figura11.csv", dec = ",", header = TRUE, check.names = FALSE)

# Pre processing and formatting 
data_f10 <- f10[f10['Clasificación'] !='Total',] %>% gather(Year, Value, "2021":"2030")

data_f10 <- data_f10 %>%
              mutate(Value = as.character(Value)) %>%
              mutate(Val = as.numeric(gsub(",", "", Value)))


# Custom functions
ylabel <- function (x) { number_format(accuracy = 1,
                                   scale = 1/1000000,
                                   suffix = " MM",
                                   big.mark = ",")(x) }


# Stacked Plots
fig10 <- ggplot(data_f10, aes(fill=Clasificación, y=Val, x=Year)) +
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  scale_y_continuous(labels = ylabel) + 
  xlab("") +
  labs( x = '', y = '') +
  theme_minimal() +
  theme(legend.position = 'bottom')
  

plotly::ggplotly(fig10) %>%
  layout(legend = list(
    orientation = "h", x = 0.1, y = -0.06))


# Alterantive plots
fig10 <- ggplot(data_f10, aes(fill=Clasificación, y=Val, x=Year)) +
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  scale_y_continuous(n.breaks = 6, labels = ylabel) + 
  labs(x = '', y = '') + 
  theme_economist() + 
  theme(
    text = element_text(family = "TradeGothic LT"), 
    plot.title = element_text(size = 12, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, margin = margin(t = 40)),
    plot.caption = element_text(size = 10, hjust = 1),
    legend.text = element_text(size = 10),
    legend.box = "vertical",
    legend.margin=margin(), 
    panel.grid = element_line(color = "gray80", size=0.15),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank() ,
    axis.title = element_text(size = 11),
    axis.text = element_text(size=10, color = "black"))

plt <- ggplotly(fig10) %>%
  layout(legend = list(
    orientation = "h", x = 0.02, y = -0.05),
    annotations = 
      list(x = 1, y = -0.22, text = "Fuente: Lavado y Luna, 2020", 
           showarrow = F, xref='paper', yref='paper', 
           xanchor='auto', yanchor='bottom', xshift=0, yshift=0,
           font=list(size=12)))

plt


##### facet wrap
f12a <- read.csv("data/intermediate/figura12-beneficios.csv", dec = ",", header = TRUE, check.names = TRUE)
f12b <- read.csv("data/intermediate/figura12-costos.csv", dec = ",", header = TRUE, check.names = TRUE)

# Pre processing and formatting 
formattingAlt <- function(data) {
  tmp <- data %>% 
    rename("Escenario 1" = Escenario.1) %>% 
    rename("Escenario 2" = Escenario.2) %>% 
    gather(Escenario, Val, "Escenario 1":"Escenario 2")
  
  tmp <- tmp %>%
    mutate(Val = as.character(Val)) %>%
    mutate(Value = as.numeric(gsub(",", "", Val))) %>% 
    rename(Variable = colnames(data)[1])
  
  return(tmp)
}


data_f12a <- formattingAlt(f12a)
data_f12b <- formattingAlt(f12b)

data_f12a$tipo <- "Beneficio"
data_f12b$tipo <- "Costo"

data_f12 <- rbind(data_f12a, data_f12b)


# Visualization

fig12 <- ggplot(data_f12, aes(fill=Variable, y=Value, x=tipo)) +
  geom_bar(position="stack", stat="identity", width = 0.5) +
  scale_fill_viridis(discrete = T, option="magma") +
  scale_y_continuous(n.breaks = 4, labels = ylabelMM) + 
  facet_wrap(~Escenario, dir="v") +
  labs(x = '', y = '') + 
  theme_economist() + 
  theme(
    text = element_text(family = "TradeGothic LT"), 
    plot.title = element_text(size = 12, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, margin = margin(t = 40)),
    plot.caption = element_text(size = 10, hjust = 1),
    legend.text = element_text(size = 10),
    legend.box = "vertical",
    legend.position = "bottom", 
    panel.grid = element_line(color = "gray80", size=0.15),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank() ,
    axis.title = element_text(size = 11),
    axis.text = element_text(size=10, color = "black"))

plt <- ggplotly(fig12) %>%
  layout(legend = list(
    orientation = "h", x = 0.02, y = -0.05),
    annotations = 
      list(x = 1, y = -0.4, text = "Fuente: Lavado y Luna, 2020", 
           showarrow = F, xref='paper', yref='paper', 
           xanchor='auto', yanchor='bottom', xshift=0, yshift=0,
           font=list(size=12)))

plt


# PLots with data provided by Carla
ylabel <- function (x) { number_format(accuracy = 1,
                                       scale = 1,
                                       suffix = " ?",
                                       big.mark = ",")(x) }

##Palettes
#Primary
color1 <- "#f7941e" #color principal naranja
color2 <- "#56a1d3" #color principal azul
#Secondary
color3 <- "#fdb721" #amarillo
color4 <- "#79bde8" #celeste
color5 <- "#e6e5d6" #beige
color6 <- "#5a3228" #marrón
#Accent
color7 <- "#71bf44" #verde
color8 <- "#ff26ff" #magenta
color9 <- "#ff3f19" #rojo
color10 <- "#98d900" #verde limón
#paleta creada en el otro theme 
color_pal <- c(color1, color2, color3, color4, color5, color6)

linesize <- 0.7


f1 <- read.csv("data/intermediate/figura1.csv", dec = ",", header = TRUE, check.names = TRUE)
f4 <- read.csv("data/intermediate/figura4.csv", dec = ",", header = TRUE, check.names = FALSE)
f5 <- read.csv("data/intermediate/figura5.csv", dec = ",", header = TRUE, check.names = FALSE)
f7 <- read.csv("data/intermediate/figura7.csv", dec = ",", header = TRUE, check.names = FALSE)


fig1 <- ggplot(f1, aes(y=RMM, x=año, text=RMM)) + 
  #geom_point() + 
  geom_line(color = color_pal[1], size = linesize) + 
  geom_hline(aes(yintercept = 70, linetype='Meta al 2030 según los ODS'), #'Meta al 2030 según los ODS', 
                 color = color_pal[3], size = 0.2) + 
  geom_hline(aes(yintercept = 33, linetype='Meta al 2030 según la Política Nacional Multisectorial de Salud al 2030'), #'Meta al 2030 según la Política Nacional Multisectorial de Salud al 2030', 
                 color = color_pal[4], size = 0.2) + 
  geom_hline(aes(yintercept = 35.8, linetype='Meta al 2030 según Consenso de Montevideo'), #'Meta al 203 según Consenso de Montevideo', 
                 color = color_pal[5], size = 0.2) + 
  scale_linetype_manual(name ="", values = c('dashed','dashed','dashed'), 
                        guide = guide_legend(override.aes = list(color = c(color_pal[3], color_pal[4],color_pal[5])))) + 
   
  #annotate("text", x=2004, y=25, label="-- Meta al 2030 según los ODS", color=color_pal[3], hjust = 0) + 
  #annotate("text", x=2004, y=20, label="-- Meta al 2030 según la Política Nacional Multisectorial de Salud al 2030", color=color_pal[4], hjust = 0) + 
  #annotate("text", x=2004, y=15, label="-- Meta al 2030 según Consenso de Montevideo", color=color_pal[5], hjust = 0) + 
  #scale_fill_viridis(discrete = T, option="A") + 
  scale_y_continuous(labels = ylabel) + 
  scale_x_continuous(breaks = seq(2002,2019,1), limits = c(2002,2019)) + 
  labs(x = '', y = '') + 
  theme_minimal() + 
  expand_limits(y=c(0, 100)) + 
  
  theme(
    text = element_text(family = "Arial"), 
    plot.title = element_text(size = 12, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, margin = margin(t = 40)),
    plot.caption = element_text(size = 10, hjust = 1),
    #legend.text = element_text(size = 10, colour="blue"),
    #legend.position = c(0.35,0.18), 
    #legend.box = "vertical",
    legend.position = "none", 
    #panel.grid = element_line(color = "gray80", size=0.15),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #panel.grid.minor.x = element_blank(),
    #panel.grid.major.x = element_blank(),
    #panel.grid.minor.y = element_blank(),
    #panel.grid.major.y = element_blank(),
    axis.line.x = element_line(colour = "black"),
    axis.ticks.x = element_line(colour = "black"),
    axis.title = element_text(size = 11),
    axis.text = element_text(size=10, color = "black"))

plt <- ggplotly(fig1, tooltip = c("año","text")) %>% 
  layout(hoverlabel=list(bgcolor="white"),
         annotations = list(
    list(x = 0.05, y = 0.15, text = "-- Meta al 2030 según los ODS",  
         xref='paper', yref='paper', 
         xanchor='auto', yanchor='bottom', xshift=0, yshift=0,
         font=list(size=12, color=color_pal[3]),
         showarrow = FALSE),
    list(x = 0.05, y = 0.1,  #x = 0.5, y = 1,  
         text = "-- Meta al 2030 según la Política Nacional Multisectorial de Salud al 2030",  
         xref = "paper", yref = "paper",  
         xanchor = "auto", yanchor = "bottom", 
         font=list(size=12, color=color_pal[4]),
         showarrow = FALSE),  
    list(x = 0.05, y = 0.05,  
         text = "-- Meta al 2030 según Consenso de Montevideo",  
         xref = "paper", yref = "paper",  
         xanchor = "auto", yanchor = "bottom",  
         font=list(size=12, color=color_pal[5]),
         showarrow = FALSE)))

plt 


# F4
formattingC <- function(data) {
  tmp <- data %>% 
    rename("Año" = colnames(data)[2]) 
  
  tmp <- tmp %>%
    mutate(Año = as.factor(Año)) %>% 
    rename(Value = colnames(data)[3]) %>%
    rename(Variable = colnames(data)[1]) %>% 
    mutate(Variable = as.factor(Variable)) %>% 
  
  return(tmp)
}

data_f4 <- formattingC(f4)


# Static version
fig4 <- pyramid.plot(data_f4[data_f4$Año == 2010,]$Value, data_f4[data_f4$Año == 2019,]$Value,
             laxlab=seq(0,  25, by = 5),
             raxlab=seq(0,  25, by = 5),
             top.labels=c("2010","Grupo Etario","2019"), labels=unique(data_f4$Variable),
             main = "Distribución de muertes maternas", 
             gap  =5, labelcex = .8, unit="", lxcol="#edf8e9", rxcol="#f2f0f7",
             show.values = TRUE)


# Dynamic version
fig4a <- ggplot(data_f4[data_f4$Año == 2010,], aes(fill=Variable , y=Value, x=Variable)) +
  geom_bar(stat="identity", position='identity', width = 1) + 
  coord_flip () + 
  scale_fill_viridis(discrete = T, option="F") +
  labs(x = '', y = '') + 
  # scale_x_discrete(name = "", position = "top") + 
  # scale_y_continuous(name = "",
  #                    breaks = seq(0, -25, by = -5),  # y axis values (before coord_flip) 
  #                    labels = seq(0,  25, by = 5)) + 
  scale_y_continuous(name = "", labels = seq(0,  25, by = 5), 
                     trans = 'reverse') + 
  theme_economist() + 
  theme(
    text = element_text(family = "Arial"), 
    plot.title = element_text(size = 12, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, margin = margin(t = 40)),
    plot.caption = element_text(size = 10, hjust = 1),
    #plot.margin=unit(c(0.4,0.1,0.4,-0.1),"cm"),
    legend.position = "none",
    panel.grid = element_line(color = "gray80", size=0.15),
   
    axis.title = element_text(size = 11),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(), 
    axis.text = element_text(size=10, color = "black")) + 
  ggtitle(label="", subtitle="2010")

plt4a <- ggplotly(fig4a)

fig4b <- ggplot(data_f4[data_f4$Año == 2019,], aes(fill=Variable , y=Value, x=Variable)) +
  # geom_bar(position="stack", stat="identity", width = 0.5) +
  geom_bar(stat="identity", position='identity', width = 1) + 
  coord_flip () + 
  scale_fill_viridis(discrete = T, option="F") +
  labs(x = '', y = '') + 
  #scale_y_discrete(labels=NULL) + 
  theme_economist() + 
  theme(
    text = element_text(family = "TradeGothic LT"), 
    plot.title = element_text(size = 12, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, margin = margin(t = 40)),
    plot.caption = element_text(size = 10, hjust = 1),
    #plot.margin=unit(c(0.4,0.2,0.4,-.1),"cm"),
    legend.text = element_blank(),
    legend.position="none",
    panel.grid = element_line(color = "gray80", size=0.15),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(), 
    axis.text.y = theme_bw()$axis.text.y,
    
    axis.title = element_text(size = 11),
    axis.text = element_text(size=10, color = "black")) + 
  ggtitle(label="", subtitle="2019")

plt4b <- ggplotly(fig4b)

#fig4 <- grid.arrange(fig4a, fig4b, widths=c(0.4,0.6), ncol=2)

fig <- subplot(plt4a, plt4b, 
               margin = 0.055) %>% 
  layout(title = list(x = 0.55, y = -0.3, text = "Grupo Etario", font=list(size=15)),
         annotations = list(
           list(x = 1, y = -0.08, text = "Fuente: Centro Nacional de Epidemiología, Prevención y Control de Enfermedades (2010-2019)", 
                showarrow = F, xref='paper', yref='paper', 
                xanchor='auto', yanchor='bottom', xshift=0, yshift=0,
                font=list(size=12)),
           list(x = 0.2, y = 1,  #x = 0.5, y = 1,  
             text = "2010",  
             xref = "paper", yref = "paper",  
             xanchor = "center", yanchor = "top",  
             showarrow = FALSE),  
           list(x = 0.8, y = 1,  
             text = "2019",  
             xref = "paper", yref = "paper",  
             xanchor = "center", yanchor = "top",  
             showarrow = FALSE)))
fig

# Previos version stacked and lines
fig4 <- ggplot(data_f4[data_f4$Año == 2019,], aes(fill=Variable , y=Variable, x=Value)) +
  # geom_bar(position="stack", stat="identity", width = 0.5) +
  geom_bar(stat="identity", position="identity", width = 1) +
  scale_fill_viridis(discrete = T, option="F") +
  labs(x = '', y = '') + 
  theme_economist() + 
  theme(
    text = element_text(family = "TradeGothic LT"), 
    plot.title = element_text(size = 12, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, margin = margin(t = 40)),
    plot.caption = element_text(size = 10, hjust = 1),
    legend.text = element_text(size = 10),
    legend.box = "vertical",
    legend.position = "bottom", 
    panel.grid = element_line(color = "gray80", size=0.15),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank() ,
    axis.title = element_text(size = 11),
    axis.text = element_text(size=10, color = "black"))

# fig4 <- ggplot(data_f4, aes(group=Año, colour=Año, y=Value, x=Variable)) + 
#   geom_point() + 
#   geom_line() +
#   scale_fill_viridis(discrete = T, option="magma") +
#   labs(x = '', y = '') + 
#   theme_economist() + 
#   theme(
#     text = element_text(family = "TradeGothic LT"), 
#     plot.title = element_text(size = 12, hjust = 0.5),
#     plot.subtitle = element_text(size = 11, hjust = 0.5, margin = margin(t = 40)),
#     plot.caption = element_text(size = 10, hjust = 1),
#     legend.text = element_text(size = 10),
#     legend.box = "vertical",
#     legend.position = "top", 
#     panel.grid = element_line(color = "gray80", size=0.15),
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.x = element_blank() ,
#     axis.title = element_text(size = 11),
#     axis.text = element_text(size=10, color = "black"),
#     axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5))
# 
# plt <- ggplotly(fig4) %>%
#   layout(legend = list(
#     orientation = "h", x = 0.02, y = -0.2),
#     annotations = 
#       list(x = 1, y = -0.3, text = "Centro Nacional de Epidemiología, Prevención y Control de Enfermedades (2010-2019)", 
#            showarrow = F, xref='paper', yref='paper', 
#            xanchor='auto', yanchor='bottom', xshift=0, yshift=0,
#            font=list(size=12)))

plt <- ggplotly(fig4) %>%
  layout(legend = list(
    orientation = "h", x = 0.02, y = -0.04),
    annotations = 
      list(x = 1, y = -0.22, text = "Fuente: Centro Nacional de Epidemiología, Prevención y Control de Enfermedades (2010-2019)", 
           showarrow = F, xref='paper', yref='paper', 
           xanchor='auto', yanchor='bottom', xshift=0, yshift=0,
           font=list(size=12)))

plt


# F5
data_f5 <- formattingC(f5)

fig5 <- ggplot(data_f5, aes(fill=Variable , y=Value, x=Año)) +
  geom_bar(position="stack", stat="identity", width = 0.5) +
  scale_fill_viridis(discrete = T, option="F") +
  labs(x = '', y = '') + 
  theme_economist() + 
  theme(
    text = element_text(family = "TradeGothic LT"), 
    plot.title = element_text(size = 12, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, margin = margin(t = 40)),
    plot.caption = element_text(size = 10, hjust = 1),
    legend.text = element_text(size = 10),
    legend.box = "vertical",
    legend.position = "bottom", 
    panel.grid = element_line(color = "gray80", size=0.15),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank() ,
    axis.title = element_text(size = 11),
    axis.text = element_text(size=10, color = "black"))

# fig4 <- ggplot(data_f4, aes(group=Año, colour=Año, y=Value, x=Variable)) + 
#   geom_point() + 
#   geom_line() +
#   scale_fill_viridis(discrete = T, option="magma") +
#   labs(x = '', y = '') + 
#   theme_economist() + 
#   theme(
#     text = element_text(family = "TradeGothic LT"), 
#     plot.title = element_text(size = 12, hjust = 0.5),
#     plot.subtitle = element_text(size = 11, hjust = 0.5, margin = margin(t = 40)),
#     plot.caption = element_text(size = 10, hjust = 1),
#     legend.text = element_text(size = 10),
#     legend.box = "vertical",
#     legend.position = "top", 
#     panel.grid = element_line(color = "gray80", size=0.15),
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.x = element_blank() ,
#     axis.title = element_text(size = 11),
#     axis.text = element_text(size=10, color = "black"),
#     axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5))
# 
# plt <- ggplotly(fig4) %>%
#   layout(legend = list(
#     orientation = "h", x = 0.02, y = -0.2),
#     annotations = 
#       list(x = 1, y = -0.3, text = "Centro Nacional de Epidemiología, Prevención y Control de Enfermedades (2010-2019)", 
#            showarrow = F, xref='paper', yref='paper', 
#            xanchor='auto', yanchor='bottom', xshift=0, yshift=0,
#            font=list(size=12)))

plt <- ggplotly(fig5) %>%
  layout(legend = list(
    orientation = "h", x = 0.02, y = -0.04),
    annotations = 
      list(x = 1, y = -0.15, text = "Fuente: Centro Nacional de Epidemiología, Prevención y Control de Enfermedades (2010-2019)", 
           showarrow = F, xref='paper', yref='paper', 
           xanchor='auto', yanchor='bottom', xshift=0, yshift=0,
           font=list(size=12)))

plt

# F7
formattingC <- function(data) {
  tmp <- data %>% 
    rename(País = colnames(data)[2]) 
  
  tmp <- tmp %>%
    mutate(País = as.factor(País)) %>% 
    rename(Value = colnames(data)[3]) %>% 
    mutate(Value = as.numeric(sub("%","",Value))) %>% 
    rename(Variable = colnames(data)[1]) %>% 
    mutate(Variable = as.factor(Variable)) %>% 
    
    return(tmp)
}

ylabelPer <- function (x) { number_format(accuracy = 1,
                                       scale = 1,
                                       suffix = "%",
                                       big.mark = ",")(x) }

data_f7 <- formattingC(f7)



fig7 <- ggplot(data_f7, aes(group=Variable, colour=Variable, y=Value, x=País)) + 
  geom_point() + 
  geom_line() + 
  #geom_smooth(method = "lm") + 
  annotate("text", x=3, y=62, label="Uso de métodos anticonceptivos en Perú y LAC (2019)") + 
  scale_fill_viridis(discrete = T, option="A") + 
  scale_y_continuous(n.breaks = 4, labels = ylabelPer) + 
  labs(x = '', y = '') + 
  theme_economist() + 
  scale_colour_economist() + 
  theme(
    text = element_text(family = "TradeGothic LT"), 
    plot.title = element_text(size = 12, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, margin = margin(t = 40)),
    plot.caption = element_text(size = 10, hjust = 1),
    legend.text = element_text(size = 10),
    legend.box = "vertical",
    legend.position = "bottom", 
    panel.grid = element_line(color = "gray80", size=0.15),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank() ,
    axis.title = element_text(size = 11),
    axis.text = element_text(size=10, color = "black"))

plt <- ggplotly(fig7) %>%
  layout(legend = list(
    orientation = "h", x = 0.02, y = -0.05),
    annotations = 
      list(x = 1, y = -0.15, text = "Fuente: Banco Mundial - World Bank Open Data (2019)", 
           showarrow = F, xref='paper', yref='paper', 
           xanchor='auto', yanchor='bottom', xshift=0, yshift=0,
           font=list(size=12)))

plt



# Fig 3
m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=174.768, lat=-36.852, popup="The birthplace of R")
m  # Print the map

# install.packages('ggmap')
# install.packages('maps')
# install.packages('maptools')
# install.packages('rgeos')
# install.packages('shiny')
# install.packages('sf')
# install.packages('rnaturalearth')

install.packages('geojsonio')

install.packages("devtools") # I guess you also need this
devtools::install_github("ropensci/rnaturalearthhires")

library(shiny)
library(leaflet)
library(sf)
library(rnaturalearth)

world <- ne_countries(returnclass = "sf")
class(world)
#> [1] "sf"    "data.frame"
plot_ly(world, color = I("gray90"), stroke = I("black"), span = I(1))

world %>%
  select(name) %>%
  print(n = 4)

canada <- ne_states(country = "Peru", returnclass = "sf")
plot_ly(canada, split = ~name, color = ~provnum_ne)


ui <- fluidPage(
  leafletOutput(outputId = "mymap"),
  tableOutput(outputId = "myDf_output")
)

server <- function(input, output){
  
  ## use reactive values to store the data you generate from observing the shape click
  rv <- reactiveValues()
  rv$myDf <- NULL
  
  cities <- read.csv(textConnection("
City,Lat,Long,Pop
Boston,42.3601,-71.0589,645966
Hartford,41.7627,-72.6743,125017
New York City,40.7127,-74.0059,8406000
Philadelphia,39.9500,-75.1667,1553000
Pittsburgh,40.4397,-79.9764,305841
Providence,41.8236,-71.4222,177994
"))
  cities$id <- 1:nrow(cities)  ## I'm adding an 'id' value to each shape
  
  output$mymap <- renderLeaflet({
    leaflet(cities) %>% addTiles() %>%
      addCircles(lng = ~Long, lat = ~Lat, weight = 1,
                 radius = ~sqrt(Pop) * 30, popup = ~City, layerId = ~id)
  })
  
  observeEvent(input$mymap_shape_click, {
    
    print("shape clicked")
    event <- input$mymap_shape_click
    print(str(event))
    
    ## update the reactive value with your data of interest
    rv$myDf <- data.frame(lat = event$lat, lon = event$lng)
    
    print(rv$myDf)
    
  })
  
  ## you can now 'output' your generated data however you want
  output$myDf_output <- renderTable({
    rv$myDf
  })
  
}

shinyApp(ui, server)




# One last chance

########## Generating the shp file for Peruvian regions ##########
# Source of raw shp file: https://data.humdata.org/dataset/limites-de-peru

# states <- read_sf('PeruAllInfo.shp')
# 
# # Selects only name of region and its geometry
# tmp <- states %>% select(8, 16)
# # testing
# x <- tmp %>% filter(tmp$ADM1_ES == 'Lima')
# plot(st_union(x))
# 
# # Merged geometries of subregions into one geometry representing the region
# crime_rate <- tmp %>% 
#   st_union(tmp) %>%
#   summarize(Region = unique(ADM1_ES))
# 
# # Save the geometry of regions 
# st_write(crime_rate, "Peru.shp")
########### End of generating shp file ###############

formattingTab <- function(data) {
  tmp <- data %>% 
    rename(Region = colnames(data)[2])
  return(tmp)
}

formattingShp <- function(data) {
  tmp <- data %>%
    rename(Region = ADM1_ES)
  return(tmp)
}

  
tab3 <- read.csv('data/intermediate/TablasF3.csv', header = TRUE, check.names = FALSE)
Peru <- read_sf('data/intermediate/Peru.shp')  # to the level of provinces

# Source 
peruRegion <- geojson_sf("https://raw.githubusercontent.com/juaneladio/peru-geojson/master/peru_departamental_simple.geojson")

tab3 <- formattingTab(tab3)
Peru <- formattingShp(Peru)

states <- formattingShp(Peru)

peruRegion <- peruRegion %>% 
  add_column(Region = tab3$Region)

# Merge of data with polygons 
# Peru <- merge(states, tab3, by = 'Region', all.x = F)
Peru <- merge(peruRegion, tab3, by = 'Region', all.x = F)


paletteNum1 <- colorNumeric('magma', domain = Peru$`RMM_quinq_02-06`, )
paletteNum2 <- colorNumeric('magma', domain = Peru$`RMM_quinq_07-11`, )
paletteNum3 <- colorNumeric('magma', domain = Peru$`RMM_quinq_12-16`, )

# bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
# pal <- colorBin("YlOrRd", domain = Peru$Region, bins = bins)
# x <- states %>% filter(states$ADM1_ES == 'Lima')
# plot(st_union(x))

stateLabels1 <- sprintf('<b>%s</b><br/>%g RMM',
                       Peru$Region, Peru$`RMM_quinq_02-06`) %>%
  lapply(htmltools::HTML)

stateLabels2 <- sprintf('<b>%s</b><br/>%g RMM',
                        Peru$Region, Peru$`RMM_quinq_07-11`) %>%
  lapply(htmltools::HTML)

stateLabels3 <- sprintf('<b>%s</b><br/>%g RMM',
                        Peru$Region, Peru$`RMM_quinq_12-16`) %>%
  lapply(htmltools::HTML)


leaflet() %>%
  setView(lng = -74.99, lat = -9.19, zoom = 5.1) %>% 
  addTiles() %>% 
  #addProviderTiles(providers$CartoDB.PositronNoLabels)  %>%
  addPolygons(data = Peru,
              # state border stroke color
              color = 'white', 
              weight = 0.1, 
              # set opacity of polygons
              fillOpacity = .95, 
              fillColor = ~paletteNum1(Peru$`RMM_quinq_02-06`), 
              smoothFactor = 1,
              label = stateLabels1,
              labelOptions = labelOptions(
                style = list(color = 'gray30'),
                textsize = '12px'),
              highlightOptions = highlightOptions(
                weight = 3,
                color = 'white'),
              group = "Quinq 02-06") %>% 
  
  addPolygons(data = Peru,
              # state border stroke color
              color = 'white', 
              weight = 0.1, 
              # set opacity of polygons
              fillOpacity = .95, 
              fillColor = ~paletteNum2(Peru$`RMM_quinq_07-11`), 
              smoothFactor = 1,
              label = stateLabels2,
              labelOptions = labelOptions(
                style = list(color = 'gray30'),
                textsize = '12px'),
              highlightOptions = highlightOptions(
                weight = 3,
                color = 'white'),
              group = "Quinq 07-11") %>% 
  
  addPolygons(data = Peru,
              # state border stroke color
              color = 'white', 
              weight = 0.1, 
              # set opacity of polygons
              fillOpacity = .95, 
              fillColor = ~paletteNum3(Peru$`RMM_quinq_12-16`), 
              smoothFactor = 1,
              label = stateLabels3,
              labelOptions = labelOptions(
                style = list(color = 'gray30'),
                textsize = '12px'),
              highlightOptions = highlightOptions(
                weight = 3,
                color = 'white'),
              group = "Quinq 12-16") %>% 
  addLegend(pal = paletteNum1, values = Peru$`RMM_quinq_02-06`, 
            title = '<small>Razón de mortalidad materna <br> quinquenal (2002-2016)</small>', 
            position = 'bottomleft',
            group = "Quinq 02-06") %>% 
  addLayersControl(
    baseGroups = c("Quinq 02-06", "Quinq 07-11", "Quinq 12-16"),
    options = layersControlOptions( collapsed = FALSE ),
    position = "topright") 


# leaflet() %>%
#   addTiles(group = "OpenStreetMap") %>%
#   addProviderTiles("Stamen.Toner", group = "Toner by Stamen") %>%
#   addMarkers(runif(20, -75, -74), runif(20, 41, 42), group = "Markers") %>%
#   addLayersControl(
#     baseGroups = c("OpenStreetMap", "Toner by Stamen"),
#     overlayGroups = c("Markers")
#   )
