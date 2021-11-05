
##Palettes
#Primary
color1 <- "#f7941e" #color principal naranja
color2 <- "#56a0d3" #color principal azul
#Secondary
color3 <- "#fdb721" #amarillo
color4 <- "#79bde8" #celeste
color5 <- "#e6e5d6" #beige
color6 <- "#5a3228" #marrón
#Accent
color7 <- "#ffc322" #lightning yellow
color8 <- "#0590c7" #rich electric blue
color9 <- "#6ec4d7" #northern lights blue
color10 <- "#71bf44" #verde
color11 <- "#ff26ff" #magenta
color12 <- "#ff3f19" #rojo
color13 <- "#7ed0e0" #light cornflower blue
color14 <- "#98d900" #verde limón
#Adicionales
color15 <- "darkgreen"
color16 <- "darkblue"

#paleta creada en el otro theme 
color_pal <- c(color1, color2, color3, color4, color5, color6)


# Function to prepare data for proper visualization
formattingStack <- function(data) {
  tmp <- data[data['Intervención'] !='Total',] %>% gather(Year, Val, "2021":"2030")
  tmp2 <- data[data['Intervención'] =='Total',] %>% gather(Year, ValueY, "2021":"2030")
  tmp <- merge(tmp, tmp2[c('Year','ValueY')], by = 'Year', all.x = TRUE)
  
  tmp <- tmp %>%
    mutate(Val = as.character(Val)) %>%
    mutate(Value = as.numeric(gsub(",", "", Val))) %>% 
    rename(Variable = Intervención) %>% 
    mutate(Variable = str_trim(Variable)) %>% 
    mutate(ValueY = as.numeric(gsub(",", "", ValueY)))
  
  return(tmp)
}

# Function that converts to MM format
ylabelMM <- function (x) { number_format(accuracy = 1,
                                         scale = 1/1000000,
                                         suffix = " MM",
                                         big.mark = ",")(x) }

ylabelMM_ <- function (x) { number_format(accuracy = 1,
                                         scale = 1/1000000,
                                         suffix = "",
                                         big.mark = ",")(x) }

formattingStack2 <- function(data) {
  tmp <- data %>% 
    na.omit() %>% 
    rename("Escenario 1" = Escenario.1) %>% 
    rename("Escenario 2" = Escenario.2) %>% 
    gather(Escenario, Val, "Escenario 1":"Escenario 2")
  
  tmp <- tmp %>%
    mutate(Val = as.character(Val)) %>%
    mutate(Value = as.numeric(gsub(",", "", Val))) %>% 
    rename(Variable = colnames(data)[1])
  
  return(tmp)
}

formattingStack3 <- function(data) {
  tmp <- data %>% 
    rename("Escenario 1" = Escenario.1) %>% 
    rename("Escenario 2" = Escenario.2) %>% 
    rename("Escenario 3" = Escenario.3) %>% 
    gather(Escenario, Val, "Escenario 1":"Escenario 3")
  
  tmp <- tmp %>%
    mutate(Val = as.character(Val)) %>%
    mutate(Value = as.numeric(gsub(",", "", Val))) %>% 
    rename(Variable = colnames(data)[1])
  
  return(tmp)
}

formattingMM <- function(data) {
  tmp <- data %>% 
    rename("Año" = colnames(data)[2]) 
  
  tmp <- tmp %>%
    mutate(Año = as.factor(Año)) %>% 
    rename(Value = colnames(data)[3]) %>%
    rename(Variable = colnames(data)[1]) %>% 
    mutate(Variable = as.factor(Variable)) %>% 
    
    return(tmp)
}

formattingMML <- function(data) {
  tmp <- data %>% 
    rename("Año" = colnames(data)[2]) 
  
  tmp <- tmp %>%
    mutate(Año = as.factor(Año)) %>% 
    rename(Value = colnames(data)[3]) %>%
    rename(Variable = colnames(data)[1]) %>% 
    mutate(Variable = as.factor(Variable)) %>%
    spread(Año, Value)
  
  tmp <- tmp %>% 
    rename(Value2010 = colnames(tmp)[2]) %>% 
    rename(Value2019 = colnames(tmp)[3])
    
    return(tmp)
}

formattingMM5 <- function(data) {
  tmp <- data %>% 
    rename("Año" = colnames(data)[3]) 
  
  tmp <- tmp %>%
    mutate(Año = as.factor(Año)) %>% 
    rename(Tipo = colnames(data)[1]) %>% 
    rename(Value = colnames(data)[4]) %>%
    rename(Variable = colnames(data)[2]) %>% 
    mutate(Variable = as.factor(Variable)) %>% 
    mutate(Value = as.numeric(sub("%","",Value)))
    
    return(tmp)
}

formattingAC <- function(data) {
  tmp <- data %>% 
    rename(País = colnames(data)[2]) 
  
  tmp <- tmp %>%
    mutate(País = as.factor(País)) %>% 
    rename(Value = colnames(data)[3]) %>% 
    mutate(Value = as.numeric(sub("%","",Value))) %>% 
    rename(Variable = colnames(data)[1]) %>% 
    mutate(Variable = as.factor(Variable)) %>% 
    mutate(Tipo = if_else(Variable == "Cualquier método", "cualquiera", "modernos")) 
    
    return(tmp)
}


formattingL <- function(data) {
  tmp <- data %>% 
    mutate_if(is.character,as.numeric) %>% 
    mutate(year = as.character(year))
  return(tmp)
}


formattingMAC <- function(data) {
  tmp <- data %>% 
    mutate(across(where(~any(str_detect(.,"%"))), parse_number)/100) %>% 
    rename(Year = colnames(data)[1]) %>% 
    mutate(Year = as.character(Year))
  
  return(tmp)
}

formattingLL <- function(data) {
  tmp <- data %>% 
    mutate_if(is.character, as.numeric) %>% 
    rename(Year = colnames(data)[1]) %>% 
    mutate(Year = as.character(Year))
  return(tmp)
}

# Function that converts to % format
ylabelPer <- function (x) { number_format(accuracy = 1,
                                          scale = 1,
                                          suffix = "%",
                                          big.mark = ",")(x) }

ylabelFlat <- function (x) { number_format(accuracy = 1,
                                          scale = 1,
                                          suffix = "",
                                          big.mark = ",")(x) }

formatValue <- function(x, sufx="K"){
  scal <- 1000
  if(sufx == 'K'){scal <- 1000}
  if(sufx == 'MM'){scal <- 1000000}
  nx <- number_format(accuracy = 1,
                scale = 1/scal,
                suffix = sufx,
                big.mark = ",")(x)
  return(nx)
}
sumValue <- function(x, var) {
  print(var)
  return(sum(x["Value"][x["Year"] == var,]))
}

# Plotting functions
# single stack
plotStack <- function(data, baseTheme, customTheme, customPalette, yLimSup){
  fig <- ggplot(data, aes(fill=Variable, y=Value, x=Year, group = Variable, 
                          text = paste(Year,'<br>',
                                       Variable,': ',formatValue(Value),
                                       '<br>Todas las intervenciones: ', formatValue(ValueY)))) +
    geom_bar(position="stack", stat="identity") +
    #geom_area(aes(fill=Variable), position='stack') + 
    scale_fill_manual(values=customPalette) +
    scale_y_continuous(labels = ylabelMM, limits = c(0,yLimSup)) + 
    labs(x = '', y = '') + 
    baseTheme + 
    customTheme+ theme( legend.title = element_blank())
    
  
  plt <- ggplotly(fig, tooltip = "text") %>%
    layout(legend = list(
      orientation = "v", x = 100, y = 0.5)
      )
  
  return(plt)
}

# Dropdow of stacks

# ui <-shinyUI(fluidPage(selectInput("selectPlot", "Choose desired plot", choices=paste0("p", 1:2)), plotlyOutput("plot")))
# 
# server <- shinyServer(function(input,output){      
#   output$plot <- renderPlotly({
#     return(get(input$selectPlot)) # get("p1") from the workspace returns the object p1, which is a plotly object
#   })
# })
# 
# shinyApp(ui,server)



# Double stacks
plotStack2 <- function(data, baseTheme, customTheme, customPalette){
  fig <- ggplot(data, aes(fill=Variable, y=Value, x=tipo, 
                          text = paste(tipo,
                                       '<br>',Variable,
                                       '<br>Costo: ',formatValue(Value,sufx = " MM")))) +
    geom_bar(position="stack", stat="identity", width = 0.5) +
    #scale_fill_viridis(discrete = T, option="magma") + # Better colors with this package
    scale_fill_manual(values=customPalette) +
    scale_y_continuous(labels = ylabelMM) + 
    facet_wrap(~Escenario, dir="h") +
    labs(x = '', y = '') + 
    baseTheme + 
    customTheme
    
  
  plt <- ggplotly(fig, tooltip = "text") %>%
    layout(legend = list(
      orientation = "h", x = 0.02, y = -0.09),
      hoverlabel=list(bgcolor="white", font=list(family=customTheme$text$family)))
  
  return(plt)
}


# Dashed lines
customTheme <- function(fontf, txsize=10, lgpos="none", lgtxtsize=10, lgbox="vertical", titlesize=10){
  t <- theme(
    text = element_text(family = fontf), 
    plot.title = element_text(size = 12, hjust = 0.5),
    plot.subtitle = element_text(size = 11),
    plot.caption = element_text(size = 10, hjust = 1),
    legend.text = element_text(size = 10),
    legend.box = "vertical",
    legend.position = lgpos, 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(colour = "black"),
    axis.ticks.x = element_line(colour = "black"),
    axis.title = element_text(size = 11),
    axis.text = element_text(size=txsize, color = "black"),
    strip.text.x = element_text(size = titlesize, colour = "black"))
  return(t)
}

plotggLine <- function(data, colorLine, linesize, baseTheme, customTheme, colord1, colord2, colord3, colorseg1, colorseg2){
  fig <- ggplot(data, aes(y=RMM, x=año, text=RMM)) + 
    #geom_line(color = colorLine, size = linesize) + 
    geom_line(data = data %>% filter(año <= 1996), aes(y=RMM, x=año), color = colorseg1, size = linesize) + 
    geom_line(data = data %>% filter(año >= 1996 & año <= 1997), aes(y=RMM, x=año), color = "gray", linetype = "dashed") + 
    geom_line(data = data %>% filter(año >= 1997 & año <= 2001), aes(y=RMM, x=año), color = colorseg2, size = linesize) + 
    geom_line(data = data %>% filter(año >= 2001 & año <= 2002), aes(y=RMM, x=año), color = "gray", linetype = "dashed") + 
    geom_line(data = data %>% filter(año >= 2002), aes(y=RMM, x=año), color = colorLine, size = linesize) + 
    geom_hline(aes(yintercept = 70, linetype='M1'), 
               color = colord1, size = 0.2) + 
    geom_hline(aes(yintercept = 33, linetype='M2'), 
               color = colord2, size = 0.2) + 
    geom_hline(aes(yintercept = 41.3, linetype='M3'), 
               color = colord3, size = 0.2) + 
    scale_linetype_manual(name ="", values = c('dashed','dashed','dashed'), 
                          guide = guide_legend(override.aes = list(color = c(colord1, colord2, colord3)))) + 
    scale_y_continuous(labels = ylabelFlat) + 
    #scale_x_continuous(breaks = seq(2002,2019,1), limits = c(2002,2019)) + 
    expand_limits(y=c(0, 310)) + 
    labs(x = '', y = '') + 
    baseTheme + 
    customTheme 
  
  return(fig)
}

plotlyLine <- function(fig, fontf, asize, colord1, colord2, colord3, colorLine, colorseg1, colorseg2){
  plt <- ggplotly(fig, tooltip = c("año","text")) %>% 
    #hide_legend() %>% 
    layout(hoverlabel=list(bgcolor="white", font=list(family=fontf)),
           legend = list(orientation = "v", x = 1, y = 0.55, font = list(color = "white")),
           annotations = list(
             list(x = 0.05, y = 0.25, #x = 0.05, y = 0.15, 
                  text = "Meta mundial ODS",  
                  xref='paper', yref='paper', 
                  xanchor='auto', yanchor='bottom', xshift=0, yshift=0,
                  font=list(size=asize, color=colord1),
                  showarrow = FALSE),
             list(x = 0.05, y = 0.08, #x = 0.05, y = 0.1,  #x = 0.5, y = 1,  
                  text = "Meta de Política Nacional Multisectorial de Salud al 2030",  
                  xref = "paper", yref = "paper",  
                  xanchor = "auto", yanchor = "bottom", 
                  font=list(size=asize, color=colord2),
                  showarrow = FALSE),  
             list(x = 0.05, y = 0.16, #x = 0.05, y = 0.05,  
                  text = "Meta según Organización Panamericana de la Salud",  
                  xref = "paper", yref = "paper",  
                  xanchor = "auto", yanchor = "bottom",  
                  font=list(size=asize, color=colord3),
                  showarrow = FALSE),
             
             # Right legend
             list(x = 2018, y = 0.60, #x = 2021.5, y = 0.60, 
                  text = "- RMM",  bgcolor="white", 
                  xref='x', yref='paper', 
                  xanchor='left', yanchor='bottom',
                  font=list(size=asize, color=colorLine),
                  showarrow = FALSE),
             list(x = 2018, y = 0.53, #x = 0.05, y = 0.1,  #x = 0.5, y = 1,  
                  text = "- RMM*",  bgcolor="white",
                  xref = "x", yref = "paper",  
                  xanchor = "left", yanchor = "bottom", 
                  font=list(size=asize, color=colorseg2),
                  showarrow = FALSE)#,  
           #  list(x = 2018, y = 0.46, #x = 0.05, y = 0.05,  
           #       text = "- RMM**",  bgcolor="white",
           #        xref = "x", yref = "paper",  
           #        xanchor = "left", yanchor = "bottom",  
           #        font=list(size=asize, color=colorseg1),
           #       showarrow = FALSE)
)
)
  
  return(plt) 
}

# Pyramid plot F4
plotPyramid <- function(data, title="Distribución de muertes maternas"){
  pyramid.plot(data[data$Año == 2010,]$Value, data[data$Año == 2019,]$Value,
                     laxlab=seq(0,  25, by = 5),
                     raxlab=seq(0,  25, by = 5),
                     top.labels=c("2010","Grupo Etario","2019"), labels=unique(data$Variable),
                     main = title, 
                     gap  =5, labelcex = .8, unit="", lxcol="#edf8e9", rxcol="#f2f0f7",
                     show.values = TRUE)
}

# Pyramid; Dynamic version
# mirrored
plotPyramidB <- function(data, font="Arial", titleP="Distribución de muertes maternas", 
                         variable="Grupo Etario", labelbreaks=seq(0,  25, by = 5)){
  fig4a <- ggplot(data[data$Año == 2010,], aes(fill=Variable , y=Value, x=Variable)) +
    geom_bar(stat="identity", position='identity', width = 1) + 
    coord_flip () + 
    scale_fill_viridis(discrete = T, option="F") +
    labs(x = '', y = '') +  
    scale_y_continuous(name = "", breaks = labelbreaks, labels = labelbreaks, 
                       trans = 'reverse') + 
    theme_economist() + 
    theme(
      text = element_text(family = font), 
      plot.title = element_text(size = 12, hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5, margin = margin(t = 40)),
      plot.caption = element_text(size = 10, hjust = 1),
      legend.position = "none",
      panel.grid = element_line(color = "gray80", size=0.15),
      axis.title = element_text(size = 11),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(), 
      axis.text = element_text(size=10, color = "black")) + 
    ggtitle(label="", subtitle="2010")
  
  plt4a <- ggplotly(fig4a)
  
  fig4b <- ggplot(data[data$Año == 2019,], aes(fill=Variable , y=Value, x=Variable)) + 
    geom_bar(stat="identity", position='identity', width = 1) + 
    coord_flip () + 
    scale_fill_viridis(discrete = T, option="F") +
    labs(x = '', y = '') + 
    theme_economist() + 
    theme(
      text = element_text(family = font), 
      plot.title = element_text(size = 12, hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5, margin = margin(t = 40)),
      plot.caption = element_text(size = 10, hjust = 1), 
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
  
  
  fig <- subplot(plt4a, plt4b, 
                 margin = 0.055) %>% 
    layout(title = list(x = 0.55, y = -0.3, text = titleP, font=list(size=18)),
           annotations = list(
             list(x = 1, y = -0.08, text = "Fuente: Centro Nacional de Epidemiología, Prevención y Control de Enfermedades (2010-2019)", 
                  showarrow = F, xref='paper', yref='paper', 
                  xanchor='auto', yanchor='bottom', xshift=0, yshift=0,
                  font=list(size=12)),
             list(x = 0.5, y = 1,  
                  text = variable,  
                  xref = "paper", yref = "paper",  
                  xanchor = "center", yanchor = "top",  
                  showarrow = FALSE),
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
  return(fig)
}

# Same side (overlapped)
plotPyramidBB <- function(data, baseTheme, customTheme, colorB){
  
  fig <- ggplot(data, aes(fill=Año, y=Value, x=Variable, alpha=Año, #group=Variable, 
                          text = paste('Las muertes maternas de mujeres de ', Variable, 
                                       ' representaron el: <br>',Value,
                                       '% en ', Año))) +
    geom_bar(stat="identity", position=position_dodge(0.4), width = 1) + 
    scale_fill_manual(values=colorB) + 
    scale_alpha_manual(values=c(.9, .9)) + 
    coord_flip() + 
    scale_y_continuous(labels = ylabelPer) + 
    labs(x = '', y = '') +
    baseTheme +
    customTheme

  plt <- ggplotly(fig, tooltip = "text") %>% 
    layout(hovermode = "y unified",
           hoverlabel=list(bgcolor='white',
                           font=list(color='black')),
           hoverdistance = 0,
           legend = list(
             orientation = "v", x = 100, y = 0.5)
           )
  return(plt)
}

# Same side (overlapped) ******* Plan B :)
# El tag <i> y </i> da el formato de cursiva, si no se quire, solo se debe borrar
plotPyramidBBL <- function(data, fontf="Arial", fsize=11, pal){
  plt <- plot_ly(data, y = ~Variable) %>% 
    add_bars(x= ~Value2010/100, type = 'bar', width = 0.65, orientation = 'h', name = "2010", 
             offset = -0.5, opacity=0.9, marker = list(color = pal[2]), hoverinfo='text', 
             text = ~paste("<i>Las muertes maternas de mujeres de ",Variable,
                           "representaron el: </i><br>",Value2010,
                           "% en 2010")) %>% 
    add_bars(x= ~Value2019/100, type = 'bar', width = 0.65, orientation = 'h', name = "2019", 
             opacity=0.9, marker = list(color = pal[1]), hoverinfo='text', 
             text = ~paste("<i>Las muertes maternas de mujeres de ",Variable,
                           "representaron el: </i><br>",Value2019,
                           "% en 2019")) %>% 
    # plot_ly(data, x=~Value2019/100, y = ~Variable, type = 'bar', orientation = 'h', 
    #         name = "2019", hoverinfo='text', 
    #         text = ~paste("<i>Las muertes maternas de mujeres de ",Variable,
    #                       "representaron el: </i><br>", Value2019,
    #                       "% en 2019")) %>% 
    # add_bars(x= ~Value2019/100, width = 0.6, name = "2019", orientation = 'h', 
    #          opacity=0.9, marker = list(color = pal[1]), showlegend = TRUE, text = "2019", 
    #          hovertemplate = paste('<i>Las muertes maternas de mujeres de </i> %{y} <i>representaron el:</i>',
    #                                '<br>%{x:.1%} en %{text}',
    #                                "<extra></extra>")) %>% 
    # add_bars(x= ~Value2010/100, width = 0.6, name = "2010", orientation = 'h', text = "2010", 
    #          offset = -0.4, opacity=0.9, marker = list(color = pal[2]), showlegend = TRUE, 
    #          hovertemplate = paste('<br>%{x:.1%} en %{text}',
    #                                "<extra></extra>")) %>% 
    
    layout(modebar = list(remove=c("toggleSpikelines")),
           hovermode = "y unified",
           hoverlabel=list(bgcolor='white',
                           font=list(color='black')),
           font = list(family = fontf, size = fsize), 
           xaxis = list(title = '', showgrid = F, showline = T, zeroline = FALSE, tickformat = "%", range=c(0,0.3), tickcolor = "black"),
           yaxis = list(title = '', showgrid = F, showline = F, zeroline = FALSE, showspikes=F, ticklen = 20,  tickcolor = "white"),
           legend = list(
             orientation = "v", x = 100, y = 0.5)
    ) 
  return(plt)
}


# Stacked single for MM (F5)
customThemeMM <- function(fontf, txsize=10, lgpos="none"){
  t <- theme(
    text = element_text(family = fontf), 
    plot.title = element_text(size = 12, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, margin = margin(t = 40)),
    plot.caption = element_text(size = 10, hjust = 1),
    legend.text = element_text(size = 10),
    legend.box = "vertical",
    legend.position = lgpos, 
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.spacing = unit(1.7, "lines"),
    axis.title = element_text(size = txsize),
    axis.line.x = element_line(),
    axis.text = element_text(size=8, color = "black"))
  
  return(t)
}

plotStackMM <- function(data, baseTh, customTh, colorB){
  data$Variable <- factor(data$Variable, levels = unique(data$Variable)) #Make persistent the order in the data
  
  fig <- ggplot(data, aes(fill=Año, y=Value, x=Variable, 
                          text=paste(Variable, '<br>','en:', Año, '<br>',Value,'%'))) +
    geom_bar(position="dodge", stat="identity", width = 0.8) + 
    #scale_colour_manual(values=colorB) + 
    scale_fill_manual(values=colorB) + 
    geom_text(aes(label = paste(Value,"%")), size = 3, position = position_dodge(.9), vjust=-0.05) + 
    facet_wrap(~Tipo, dir="v", scales = "free_x") + 
    scale_y_continuous(labels = ylabelPer, expand = c(0, 0)) + 
    labs(x = '', y = '') + 
    scale_x_discrete(labels = function(x) str_wrap(x, width = 13)) + 
    baseTh + 
    customTh + theme( legend.title = element_blank() )
    
  
  plt <- ggplotly(fig, tooltip = "text") %>%
    layout(margin = list(b = 60),
           legend = list(orientation = "v", x = 100, y = 0.5)) %>%
    style(textposition = "top", cliponaxis = FALSE)
  
  return(plt)
}

# Multiple Lines (F7)
plotLineMult <- function(data, nbreaks=4, fontf="Arial"){
  fig7 <- ggplot(data_f7, aes(group=Variable, colour=Variable, y=Value, x=País)) + 
    geom_point() + 
    geom_line() + 
    annotate("text", x=3, y=62, label="Uso de métodos anticonceptivos en Perú y LAC (2019)") + 
    scale_fill_viridis(discrete = T, option="A") + 
    scale_y_continuous(n.breaks = nbreaks, labels = ylabelPer) + 
    labs(x = '', y = '') + 
    theme_economist() + 
    scale_colour_economist() + 
    theme(
      text = element_text(family = fontf), 
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
  
  return(plt)
}



# Stacked single for Countries (F7)
plotStackAC <- function(data, baseTh, customTh, colorB, linecolor='magenta'){
  tmp <- data[data$Tipo == 'modernos',]
  tmp <- tmp[order(tmp$Value),]
  nlevels <- c(as.character(tmp$País))
  
  fig <- ggplot(data, aes(fill=Variable, y=Value, x=factor(País, level=nlevels), alpha=Variable, text = paste0(País, ': ', Value,'% usa: ', Variable))) +
    geom_bar(position=position_dodge(0.5), stat="identity", width = 0.9) + 
    geom_hline(aes(yintercept = 56), linetype = "dashed", 
               color = linecolor, size = 0.2) + 
    #scale_colour_manual(values=colorB) + 
    scale_fill_manual(values=colorB) +
    scale_alpha_manual(values=c(.95, .95)) +
    #scale_fill_viridis(discrete = T, option="F") +
    scale_y_continuous(labels = ylabelFlat, expand = c(0, 0)) + 
    labs(x = '', y = '') + 
    baseTh + 
    customTh + theme( legend.title = element_blank() )
  
  plt <- ggplotly(fig, tooltip = "text") %>%
    layout(legend = list(
      orientation = "v", x = 100, y = 0.5))
  
  return(plt)
}


# Functions for preprocesing geographic data
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

formattingGeo <- function(geodata, infodata){
  tmp <- infodata %>% 
    rename(Region = colnames(infodata)[2])
  
  geodata <- geodata %>% 
    add_column(Region = tmp$Region)
  
  # Merge of info data with polygons (geo data)
  peru <- merge(geodata, tmp, by = 'Region', all.x = F)
}


# Multilines Fig 2 & 6
plotMultLine <- function(data, fontf="Arial", fsize=11, pal){
  plt <- plot_ly(data, x = ~year) %>% 
    add_lines(y = ~urbano, name = 'Urbano', mode='lines', color = I(pal[1])) %>%
    add_lines(y = ~rural, name = 'Rural', mode='lines', color = I(pal[2])) %>%
    add_lines(y = ~total, name = 'Nacional', mode='lines', color = I(pal[3]), line = list(dash = "dash")) %>% 
    
    add_lines(y = ~lima_metropolitana, name = 'Lima Metropolitana', mode='lines', color = I(pal[4]), visible = F) %>%
    add_lines(y = ~selva, name = 'Selva', mode='lines', color = I(pal[5]), visible = F) %>%
    add_lines(y = ~resto_del_pais, name = 'Resto del País', mode='lines', color = I(pal[6]), visible = F) %>% 
    
    add_lines(y = ~sin_educacion, name = 'Sin educación', mode='lines', color = I(pal[7]), visible = F) %>%
    add_lines(y = ~primaria, name = 'Primaria', mode='lines', color = I(pal[8]), visible = F) %>%
    add_lines(y = ~secundaria, name = 'Secundaria', mode='lines', color = I(pal[9]), visible = F) %>% 
    add_lines(y = ~superior, name = 'Superior', mode='lines', color = I(pal[10]), visible = F) %>% 
    
    layout(
      hovermode = "x unified",
      hoverlabel = list(bgcolor='white',
                        font=list(color='black'),
                        namelength = -1),
      font = list(family = fontf, size = fsize), 
      xaxis = list(title = '', showgrid = F, showline = T),
      yaxis = list(title = '', showgrid = F, ticklen = 20, tickcolor = "transparent", tickformat = "%"),
      legend = list(orientation="v",
                    yanchor="center",
                    y=0.5,
                    xanchor="right",
                    x=100
                    ),
      updatemenus = list(
        list(
          x = 0.55,
          y = 1.1,
          label = 'Category',
          buttons = list(
            list(method = "restyle",
                 args = list('visible', c(T, T, T, F, F, F, F, F, F, F)),
                 label = "Ámbito"),
            list(method = "restyle",
                 args = list('visible', c(F, F, F, T, T, T, F, F, F, F)),
                 label = "Región"),
            list(method = "restyle",
                 args = list('visible', c(F, F, F, F, F, F, T, T, T, T)),
                 label = "Nivel Educativo")
          )
        ))
    )
  return(plt)
}



sortData <- function(data, byrow='2019'){
  valtmp <- data[data['Year']==byrow]
  idx <- order(valtmp[3:length(valtmp)-1]) + 1
  
  return(data[, c(1, c(idx), 11)])
}

# Multilines fig 8
plotMultLineB <- function(data, fontf="Arial", fsize=11, pal){
  data <- sortData(data)
  
  plt <- plot_ly(data, x = ~ejex) %>% 
    add_lines(y = ~DIU, name = 'DIU', mode='lines', color = I(pal[2])) %>% 
    add_lines(y = ~`Otros modernos`, name = 'Otros modernos', mode='lines', color = I(pal[6]), visible = T) %>% 
    add_lines(y = ~Píldora, name = 'Píldora', mode='lines', color = I(pal[1])) %>% 
    add_lines(y = ~Otros, name = 'Otros', mode='lines', color = I(pal[8]), visible = T) %>% 
    add_lines(y = ~Esterilización, name = 'Esterilización', mode='lines', color = I(pal[5]), visible = T) %>%
    add_lines(y = ~`Abstinencia periódica`, name = 'Abstinencia periódica', mode='lines', color = I(pal[7]), visible = T) %>% 
    
    add_lines(y = ~`Condón masculino`, name = 'Condón masculino', mode='lines', color = I(pal[4]), visible = T) %>%
    add_lines(y = ~Inyección, name = 'Inyección', mode='lines', color = I(pal[3])) %>%
    
    add_lines(y = ~`Total MAC modernos`, name = 'Total MAC modernos', mode='lines', color = I(pal[9]), visible = T) %>% 
    
    layout(
      hovermode = "x unified",
      hoverlabel = list(bgcolor='white',
                        font=list(color='black')),
      font = list(family = fontf, size = fsize), 
      xaxis = list(title = '', showgrid = F, showline = T, 
                   ticktext = list("2000", "2004/6", "2010", "2015", "2019"), 
                   tickvals = list(1, 6, 11, 16, 20),
                   tickcolor = "black"),
      yaxis = list(title = '', showgrid = F, showline = F, range=c(0,0.58), ticklen = 20, tickcolor = "transparent", tickformat = "%"),
      legend = list(orientation="v",
                    traceorder='reversed',
                    yanchor="center",
                    y=0.5,
                    xanchor="center",
                    x=100)
    )
  return(plt)
}

# # Multilines fig 9
plotMultLineF <- function(data, fontf="Arial", fsize=11, pal){
  plt <- plot_ly(data, x = ~Year) %>%
    add_lines(y = ~`TEFA-10-14`, name = 'TEFA-10-14', mode='lines', color = I(pal[1])) %>%
    add_lines(y = ~`TEFA-15-19`, name = 'TEFA-15-19', mode='lines', color = I(pal[2])) %>%
    add_text(x = ~Year, y = ~`TEFA-10-14`, text=~paste(`TEFA-10-14`), showlegend=F, hoverinfo='skip') %>%
    add_text(x = ~Year, y = ~`TEFA-15-19`, text=~paste(round(`TEFA-15-19`)), showlegend=F, hoverinfo='skip') %>%

    layout(
      hovermode = "x unified",
      hoverlabel = list(bgcolor='white',
                        font=list(color='black')),
      font = list(family = fontf, size = fsize),
      xaxis = list(title = '', showgrid = F, showline = T),
      yaxis = list(title = '', showgrid = F, range=c(0,70), showline = F,
                   ticklen = 20, tickcolor = "transparent"),#
      legend = list(orientation="v",
                    yanchor="center",
                    y=0.5,
                    xanchor="center",
                    x=100)
    ) %>%
    style(textposition = "top center", cliponaxis = FALSE)
  return(plt)
}

# plotMultLineF <- function(data, fontf="Arial", fsize=11, pal){
#   plt <- plot_ly(data, x = ~Year) %>% 
#     add_lines(y = ~`TEFA-10-14`/100, name = 'TEFA-10-14', mode='lines', color = I(pal[1])) %>% 
#     add_lines(y = ~`TEFA-15-19`/100, name = 'TEFA-15-19', mode='lines', color = I(pal[2])) %>% 
#     add_text(x = ~Year, y = ~`TEFA-10-14`/100 + 0.005, text=~paste(`TEFA-10-14`,"%"), showlegend=F, hoverinfo='skip') %>% 
#     add_text(x = ~Year, y = ~`TEFA-15-19`/100 + 0.005, text=~paste(`TEFA-15-19`,"%"), showlegend=F, hoverinfo='skip') %>% 
#     
#     layout(
#       hovermode = "x unified",
#       hoverlabel = list(bgcolor='white',
#                         font=list(color='black')),
#       font = list(family = fontf, size = fsize), 
#       xaxis = list(title = '', showgrid = F, showline = T),
#       yaxis = list(title = '', showgrid = F, range=c(0,0.7), showline = F, 
#                    ticklen = 20, tickcolor = "transparent", tickformat = "%"),
#       legend = list(orientation="v",
#                     yanchor="center",
#                     y=0.5,
#                     xanchor="center",
#                     x=100)
#     ) %>% 
#     style(textposition = "top center", cliponaxis = FALSE)
#   return(plt)
# }

# Leaflet plot (F3)
plotMapPeru <- function(geodata, fontf="Arial", paletteC='YlOrRd'){
  x <- geodata$`RMM_quinq_02-06`
  # color definition for each period
  paletteNum1 <- colorNumeric(paletteC, domain = geodata$`RMM_quinq_02-06`)
  paletteNum2 <- colorNumeric(paletteC, domain = geodata$`RMM_quinq_07-11`)
  paletteNum3 <- colorNumeric(paletteC, domain = geodata$`RMM_quinq_12-16`)
  
  palettelab <- colorNumeric(paletteC, domain = x, reverse = TRUE)
  
  
  # Labels for each region when selected
  stateLabels1 <- sprintf('<b>%s</b><br/>%s %g<br/>%s %g<br/>%s %g',
                          geodata$Region, 
                          "2002-2006:", geodata$`RMM_quinq_02-06`,
                          "2007-2011:", geodata$`RMM_quinq_07-11`,
                          "2012-2016:", geodata$`RMM_quinq_12-16`) %>%
    lapply(htmltools::HTML)
  stateLabels2 <- sprintf('<b>%s</b><br/>%s %g<br/>%s %g<br/>%s %g',
                          geodata$Region, 
                          "2002-2006:", geodata$`RMM_quinq_02-06`,
                          "2007-2011:", geodata$`RMM_quinq_07-11`,
                          "2012-2016:", geodata$`RMM_quinq_12-16`) %>%
    lapply(htmltools::HTML)
  stateLabels3 <- sprintf('<b>%s</b><br/>%s %g<br/>%s %g<br/>%s %g',
                          geodata$Region, 
                          "2002-2006:", geodata$`RMM_quinq_02-06`,
                          "2007-2011:", geodata$`RMM_quinq_07-11`,
                          "2012-2016:", geodata$`RMM_quinq_12-16`) %>%
    lapply(htmltools::HTML)
  
  
  map <- leaflet(width = "100%", options = leafletOptions(minZoom = 5, maxZoom = 5)) %>%
    setView(lng = -74.99, lat = -9.19, zoom = 5.1) %>% 
    setMaxBounds(lng1 = -74, lng2 = -76,
                 lat1 = 1 , lat2 = -20)%>%
    #addTiles() %>% 
    #addProviderTiles(providers$CartoDB.PositronNoLabels)  %>%
    addPolygons(data = geodata,
                # state border stroke color
                color = 'black', 
                weight = 1, 
                # set opacity of polygons
                fillOpacity = .95, 
                fillColor = ~paletteNum1(geodata$`RMM_quinq_02-06`), 
                smoothFactor = 1,
                label = stateLabels1,
                labelOptions = labelOptions(
                  style = list(color = 'gray30',
                               "font-family" = fontf),
                  textsize = '12px'),
                highlightOptions = highlightOptions(
                  weight = 4,
                  color = 'white'),
                group = "2002-2006") %>% 
    
    addPolygons(data = geodata,
                # state border stroke color
                color = 'black', 
                weight = 1, 
                # set opacity of polygons
                fillOpacity = .95, 
                fillColor = ~paletteNum2(geodata$`RMM_quinq_07-11`), 
                smoothFactor = 1,
                label = stateLabels2,
                labelOptions = labelOptions(
                  style = list(color = 'gray30',
                               "font-family" = fontf),
                  textsize = '12px'),
                highlightOptions = highlightOptions(
                  weight = 4,
                  color = 'white'),
                group = "2007-2011") %>% 
    
    addPolygons(data = geodata,
                # state border stroke color
                color = 'black', 
                weight = 1, 
                # set opacity of polygons
                fillOpacity = .95, 
                fillColor = ~paletteNum3(geodata$`RMM_quinq_12-16`), 
                smoothFactor = 1,
                label = stateLabels3,
                labelOptions = labelOptions(
                  style = list(color = 'gray30',
                               "font-family" = fontf),
                  textsize = '12px'),
                highlightOptions = highlightOptions(
                  weight = 4,
                  color = 'white'),
                group = "2012-2016") %>% 
    addLegend(pal = palettelab, values = x, 
              title = '<small>Razón de mortalidad materna <br> quinquenal (2002-2016)</small>', 
              position = 'bottomleft', opacity = 0.95,
              labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) %>% 
    addLayersControl(
      baseGroups = c("2002-2006", "2007-2011", "2012-2016"),
      options = layersControlOptions( collapsed = FALSE ),
      position = "topright") 
  
  return(map)
}