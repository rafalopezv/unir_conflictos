library(highcharter)
library(tidyverse)
library(magrittr)
library(lubridate)

source("codigo/funciones.R")
Sys.setlocale(locale = "es_ES.UTF-8")

df <- rio::import("input/base_limpia.xlsx")

colores <- c("#264653","#2a9d8f","#e9c46a","#f4a261","#e76f51", "#e63946","#d90429", "#50514f", "#293241")
colores_1 <- c("#222438","#262842","#3D405B","#CCF0F9","#870845", "#753855", "#E07A5F", "#F2CC8F", "#5BBF8F", "#BBEAD3")
colores_2  <- c("#262842", "#5BBF8F", "#BBEAD3", "#870845", "#222438", "#E07A5F", "#3D405B", "#F2CC8F", "#CCF0F9", "#753855")

# razones conflicto
df %>% 
  select(id, tipo) %>% 
  unique() %>% 
  count(tipo) %>% 
  mutate(
    x_100 = round(n/10, 0),
    prop = prop.table(n)*100,
    prop = round(prop, 2)
  ) %>% 
  arrange(desc(x_100)) %>% 
  hchart(
    "item", 
    hcaes(name = tipo, y = x_100),
    marker = list(symbol = "square"),
    showInLegend = TRUE
  ) %>% 
  hc_colors(colors = colores) %>% 
  hc_chart(style = list(fontFamily = "Open Sans")) %>% 
  hc_tooltip(enabled = T, valueDecimals = 2, borderWidth = 0.01, 
             style = list(fontFamily = "Open Sans"), backgroundColor =  "white",
             pointFormat=paste("<b>{point.tipo}</b><br>
                               <b>{point.n}</b> conflictos<br>
                               <b>{point.prop} %</b> sobre el total<br>"),
             headerFormat = "") %>% 
    hc_credits(
      enabled = TRUE,
      text = "cada cuadrado representa a 10 conflictos"
    ) -> razones_conflicto
 
# año conflicto
año_2020 <- df %>% 
  arrange(desc(fecha)) %>% 
  mutate(año = lubridate::yday(fecha)) %>% 
  slice(1) %>% 
  pull(año)

df %>% 
  mutate(
    año = lubridate::year(fecha)
  ) %>% 
  count(año) %>% 
  filter(año > 2009) %>% 
  mutate(
    dia = case_when(
      año == 2020 ~ n/año_2020, 
      T ~ n/365
    ), 
    dia = case_when(
      año == 2020 ~ round(dia, 1), 
      T ~ round(dia, 0)
    ),  
    num = 1,
    etiqueta = paste0("Año: ", año)
  ) %>% 
  ggplot(aes(as.character(num), dia)) + 
  geom_text(aes(x = 1, y = 5.5, label = dia), family = "Open Sans Bold",
            size = 17, color = "#222438") +
  facet_wrap(~etiqueta) + 
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA),
    strip.background = element_rect(fill = "lightgray", colour = NA),
    strip.text =  element_text(color = "#222438", family = "Open Sans Light", size = 18),
    axis.ticks = element_blank(),
    legend.position = "none",
    panel.grid.major = element_line(colour = "#3D405B", size = 0.09),
    panel.grid.minor =  element_blank()
  ) -> calendario

# días sin conflicto
fechas <- df %>% 
  select(fecha) %>% 
  mutate(fecha = as.Date(fecha)) %>% 
  group_by(fecha) %>% 
  count()

fin <- df %>% arrange(desc(fecha)) %>% slice(1) %>% pull(fecha) %>% as.Date()

tibble(
  fecha = seq(as.Date("2010-01-01"), fin, 1),
) -> temp_fechas 

temp_fechas %>% 
  filter(!fecha %in% fechas$fecha) %>% 
  bind_rows(fechas, .) %>% 
  filter(fecha > "2009-12-31") %>% 
  mutate(
    año = lubridate::year(fecha),
    dia = lubridate::wday(fecha, label = TRUE),
    dia_literal = lubridate::wday(fecha, abbr = F),
    mes = lubridate::month(fecha, label = TRUE, abbr = F),
    semana = lubridate::week(fecha)
  ) %>% 
  ungroup() %>% 
  group_split(año, mes) %>% 
  map(., ~arrange(., fecha)) %>% 
  map(., ~mutate(., dia_mes = 1:nrow(.))) %>% 
  bind_rows() %>% 
  mutate(
    n = replace_na(n, 0), 
    eje_x = paste0(mes, " ", dia_mes),
    etiqueta = paste0(dia_mes, " de ", mes, " de ", año)
  ) -> fechas

fechas %>% 
  ungroup() %>% 
  group_split(año) %>% 
  map(., ~arrange(., fecha)) %>% 
  map(., ~mutate(., num = 1:nrow(.))) %>% 
  bind_rows() -> temp_fechas

# eje x con 366 días (bisiesto)
which(temp_fechas %>% 
        group_split(año) %>% 
        map(., "eje_x") %>% 
        map(., length) %>% 
        unlist() == 366) %>% 
  first()

eje_x <- temp_fechas %>% 
  group_split(año) %>% 
  map(., "eje_x")

eje_x <- eje_x[[3]]
eje_x <- eje_x[-365] 

hchart(
  temp_fechas, 
  "heatmap", 
  hcaes(
    x = num,
    y = año, 
    value = n
  )
)  %>% 
  hc_colorAxis(
    stops = color_stops(4, c("#222438", "#E07A5F", "#F2CC8F", "#BBEAD3"))
  ) %>% 
  hc_yAxis(mayorGridLineWidth = 0, gridLineColor = "white") %>% 
  hc_xAxis(minorGridLineWidth = 5, categories = eje_x, gridLineColor = "black", 
           title = list(text = NULL), ceiling = 365) %>% 
  hc_chart(backgroundColor="white", borderColor = "transparent", 
           style=list(fontFamily = "Open Sans", fontSize = 12)) %>% 
  hc_tooltip(enabled = T, valueDecimals = 2, borderWidth = 0.01, 
            backgroundColor = "white",  style = list(fontFamily = "Open Sans"),
             pointFormat=paste("<b>{point.etiqueta}</b><br>
                               <b>{point.n}</b> conflictos<br>"),
             headerFormat = "") %>% 
  hc_size(height = 800) -> años_juntos

#-----------------------------
# treemap
#-----------------------------

df %>% 
  count(sector_a, sub_sector_a) %>% 
  mutate(
    porcentaje = prop.table(n)*100,
    porcentaje = round(porcentaje, 3)
  ) %>% 
  mutate_if(is.character, replace_na, "Sin clasficación") %>% 
  rename(value = n) %>% 
  arrange(sector_a, desc(value))-> temp

colores <- temp %>% select(sector_a) %>% unique

colores %<>% 
  mutate(
    color = c(rep(c("#E01F52", "#C6A659", "#06D6A0", "#466B77", "#073B4C", 
                    "#D8B970", "#5FA1B7", "#118AB2", "#BAAB89", "#1E4D5C", 
                    "#2C6E49", "#E07A5F", "#3D405B", "#81B29A", "#63585F", 
                    "#B4B5BA", "#261F23", "#575D7C", "#9194C6", "#75184D"), 2), 
              "#FCBF49", "#F77F00", "#1478AA")
  )

temp %<>% merge(., colores, alll.x = T)


lvl_opts <-  list(
  list(
    level = 1,
    borderWidth = 0,
    borderColor = "transparent",
    dataLabels = list(
      enabled = TRUE,
      align = "left",
      verticalAlign = "top",
      style = list(fontSize = "12px", textOutline = FALSE, color = "white")
    )
  ),
  list(
    level = 2,
    borderWidth = 0,
    borderColor = "transparent",
    colorVariation = list(key = "brightness", to = 0.250),
    dataLabels = list(enabled = FALSE),
    style = list(fontSize = "8px", textOutline = FALSE, color = "white")
  )
)

cols <- temp %>% pull(color) %>% unique

hchart(
  data_to_hierarchical(temp, c(sector_a, sub_sector_a), porcentaje, colors = cols),
  type = "treemap",
  levelIsConstant = T,
  allowDrillToNode = T,
  drillUpButton = list(
    text = "< Volver"
  ),
  levels = lvl_opts,
  tooltip = list(valueDecimals = FALSE) 
) %>% 
  hc_chart(
    style = list(fontFamily = "Open Sans")
  ) %>% 
  hc_size(height = 700) %>% 
  hc_tooltip(backgroundColor =  "white", borderWidth =  0.001) -> tree_map_1



# treemap de quienes son los demandados
df %>% 
  count(sector_b, sub_sector_b) %>% 
  mutate(
    porcentaje = prop.table(n)*100,
    porcentaje = round(porcentaje, 3)
  ) %>% 
  mutate_if(is.character, replace_na, "Sin clasficación") %>% 
  rename(value = n) %>% 
  arrange(sector_b, desc(value))-> temp

colores <- temp %>% select(sector_b) %>% unique

colores %<>% 
  mutate(
    color = c(rep(c("#E01F52", "#C6A659", "#06D6A0", "#466B77", "#073B4C", 
                    "#D8B970", "#5FA1B7", "#118AB2", "#BAAB89", "#1E4D5C", 
                    "#2C6E49", "#E07A5F", "#3D405B", "#81B29A", "#63585F", 
                    "#B4B5BA", "#575D7C", "#9194C6", "#75184D"), 2), 
              "#FCBF49", "#F77F00", "#1478AA", "#4F000B")
  )

temp %<>% 
  merge(., colores, alll.x = T) %>% 
  mutate(porcentaje = round(porcentaje, 3))


lvl_opts <-  list(
  list(
    level = 1,
    borderWidth = 0,
    borderColor = "transparent",
    dataLabels = list(
      enabled = TRUE,
      align = "left",
      verticalAlign = "top",
      style = list(fontSize = "12px", textOutline = FALSE, color = "white")
    )
  ),
  list(
    level = 2,
    borderWidth = 0,
    borderColor = "transparent",
    colorVariation = list(key = "brightness", to = 0.250),
    dataLabels = list(enabled = FALSE),
    style = list(fontSize = "8px", textOutline = FALSE, color = "white")
  )
)

cols <- temp %>% pull(color) %>% unique

hchart(
  data_to_hierarchical(temp, c(sector_b, sub_sector_b), porcentaje, colors = cols),
  type = "treemap",
  levelIsConstant = T,
  allowDrillToNode = T,
  drillUpButton = list(
    text = "< Volver"
  ),
  levels = lvl_opts,
  tooltip = list(valueDecimals = FALSE)
) %>% 
  hc_chart(
    style = list(fontFamily = "Open Sans")
  ) %>% 
  hc_tooltip(backgroundColor =  "white", borderWidth =  0.001) %>% 
  hc_size(height = 700)  -> tree_map_3

#----------------------
# donde conflictos
#----------------------
df %>% 
  mutate(año = lubridate::year(fecha)) %>% 
  group_by(año, departamento, localidad_o_municipio, codigo) %>% 
  count() %>% 
  filter(año > 2009) %>% 
  ungroup() %>% 
  group_split(año) %>% 
  map(., ~mutate(., prop = prop.table(n)*100)) %>% 
  bind_rows() %>% 
  rename(CODIGO = codigo) %>%
  filter(!is.na(CODIGO)) %>% 
  group_split(año) -> temp

mapa <- sf::st_read("input/municipios.339.geojson")
mapas <- list()

for(i in 1:length(temp)) {
  merge(mapa, temp[[i]], all.x = T, by = "CODIGO") %>% 
    mutate(
      key = case_when(
        is.na(n) ~ "Sin conflicto", 
        T ~ "Al menos un conflicto"
      ), 
      key_1 = case_when(
        key == "Sin conflicto" ~ 0,
        T ~ 1
      )
    ) %>% 
    fill(año) -> mapas[[i]]
  
}

mapas %<>% 
  bind_rows(.) %>% 
  group_by(año) %>% 
  mutate(key_2 = sum(key_1)) %>% 
  mutate(etiqueta = paste0(año, "\n", key_2, " municipios con conflictos"))

ggplot(mapas) +
  geom_sf(aes(fill = key), color = "white", size = 0.009) + 
  scale_fill_manual(values = rev(c("#222438", "#E07A5F"))) +
  ggthemes::theme_map(base_family = "Open Sans") + 
  facet_wrap(~etiqueta) + 
  theme_map() -> mapa_donde

#-----
#Dependency wheel demandante vs demandado
#--------

df1 <- df %>% 
  mutate(year = lubridate::year(fecha))
df1 <-  merge((df1 %>% filter(year >= 2010) %>% select(id, sector_a) %>% distinct()),
              (df1 %>% filter(year >= 2010) %>% select(id, sector_b) %>% distinct())) %>% 
  group_by(sector_a, sector_b) %>% 
  summarise(cantidad = n()) %>% 
  ungroup() %>% 
  mutate(porcentaje = prop.table(cantidad)*100) %>% 
  arrange(desc(cantidad)) %>% select(from = sector_a, to = sector_b, weight = cantidad)


dependency <- highchart() %>%
  hc_chart(
    type = "dependencywheel",
    polar = FALSE,
    inverted = FALSE, 
    style = list(fontFamily = "Open Sans")
  ) %>% 
  hc_xAxis(
    categories = df1$from
  ) %>% 
  hc_yAxis(
    visible = TRUE
  ) %>% 
  hc_colors(c(rep(c("#E01F52", "#C6A659", "#06D6A0", "#466B77", "#073B4C", 
                    "#D8B970", "#5FA1B7", "#118AB2", "#BAAB89", "#1E4D5C", 
                    "#2C6E49", "#E07A5F", "#3D405B", "#81B29A", "#63585F", 
                    "#B4B5BA", "#261F23", "#575D7C", "#9194C6", "#75184D"), 2), 
              "#FCBF49", "#F77F00", "#1478AA")) %>% 
  hc_add_series(
    df1,
    name = "",
    showInLegend = FALSE
  ) %>% 
  hc_tooltip(
    outside = TRUE,
    style = list(fontFamily = "Open Sans", fontSize = 15),
    borderWidth = 0.01,
    backgroundColor =  "white"
  )



#-------
## Mapa Highcharter
#--------
mapa <- jsonlite::fromJSON("input/municipios.339.geojson", simplifyVector = F)
coord_mun <- sf::st_read("input/municipios.339.geojson")

temp <- df %>% mutate(year = lubridate::year(fecha)) %>% filter(!is.na(codigo), year >= 2010) %>% 
  select(id, codigo, year) %>% distinct() %>% 
  group_by(codigo) %>% 
  mutate(cantidad = n()) %>% 
  ungroup() %>% 
  group_by(codigo, year, cantidad) %>% 
  summarise(anual = n()) %>% select(codigo, year, anual, cantidad) %>% 
  mutate(year = paste0("a_",year)) %>% 
  spread(year, anual)  


temp <- left_join((coord_mun %>% 
                     mutate(municipio = MUNICIPIO) %>% 
                     select(municipio, codigo = CODIGO) %>% filter(!is.na(codigo))),
                  temp,
                  by = "codigo") 


temp[is.na(temp)] <- 0


datos <- temp %>% rename(CODIGO = codigo) %>% select(-municipio)
datos$geometry <- NULL

colores <- c("#CCDCE1", "#3BC0ED", "#07F9B8", "#C6A659")
secuencia <- as.numeric(quantile(unique(datos$cantidad)))
secuencia[1] <- 1
secuencia <- c(0,secuencia)

datos <- datos %>% rename(value = cantidad)


hcmap <- highchart(type = "map") %>%
  hc_add_series(mapData = mapa, showInLegend = F, data = datos, 
                value = "value", joinBy = "CODIGO",
                borderColor = "lightgray", borderWidth = 0.05) %>% 
  hc_colorAxis(stops = color_stops(10, c("#222438", "#E07A5F", "#F2CC8F"))) %>%
  
  #hc_colorAxis(dataClasses = color_classes(secuencia, colores)) %>% 
  hc_tooltip(enabled = T, valueDecimals = 0, borderWidth = 0.001, backgroundColor =  "white",
             pointFormat=paste("<br>Municipio: <b>{point.name}</b><br>
                               Conflictos: <b>{point.value}</b><br>
                               2010: <b>{point.a_2010}</b><br>
                               2011: <b>{point.a_2011}</b><br>
                               2012: <b>{point.a_2012}</b><br>
                               2013: <b>{point.a_2013}</b><br>
                               2014: <b>{point.a_2014}</b><br>
                               2015: <b>{point.a_2015}</b><br>
                               2016: <b>{point.a_2016}</b><br>
                               2017: <b>{point.a_2017}</b><br>
                               2018: <b>{point.a_2018}</b><br>
                               2019: <b>{point.a_2019}</b><br>
                               2020: <b>{point.a_2020}</b>"),
             headerFormat = "",
             fontFamily = "Open Sans",
             borderWidth = 0.8) %>% 
  hc_chart(style = list(fontFamily = "Open Sans"))
  


hcmap <- hc_size(hcmap, 800, 800)


#------
# Barras año vs nivel
#------

conflictos <- df %>% 
  mutate(year = lubridate::year(fecha))

temp <- conflictos %>% filter(nivel > 0, year >= 2010) %>% select(id, nivel, year) %>% distinct() %>% 
  group_by(year, nivel) %>% 
  summarise(cantidad = n()) %>% 
  group_by(year) %>% 
  mutate(total = sum(cantidad)) %>%
  ungroup() %>%
  mutate(nivel = paste0("Nivel: ",nivel)) %>% 
  spread(nivel, cantidad)


temp[is.na(temp)] <- 0

a_1 <- as.data.frame(temp) #%>% rename(Año = year)



categories_column <- "year"
measure_columns <- c(colnames(a_1[3:length(a_1)]))


hbr_yn <- highchart() %>%
  hc_xAxis(categories = a_1[, categories_column],
           title = categories_column)


invisible(lapply(measure_columns, function(column) {
  hbr_yn <<-
    hc_add_series(hc = hbr_yn, name = column,
                  data = a_1[, column])
}))



hbr_gestion_nivel <- hbr_yn %>%
  hc_chart(type = "column") %>%
  hc_plotOptions(series = list(stacking = "normal")) %>%
  hc_legend(reversed = TRUE) %>% 
  hc_colors(c("#06D6A0", 
              "#C6A659",
              "#82769D",
              "#E01F52",
              "#073B4C"
  )) %>%
  hc_tooltip(enabled = T, valueDecimals = 0, borderWidth = 0.01,
             crosshairs = TRUE, shared = TRUE, backgroundColor = "white",
             style = list(fontFamily = "Open Sans",
                          color = "black", fontSize = 14),
             headerFormat = "<br><b>{point.key}</b><br><br>Total: <b>{point.total}</b><br>") %>%
  hc_add_theme(hc_theme_smpl(
    yAxis = list(
      labels = list(style = list(fontSize = "15px"), useHTML = TRUE)
    ),
    xAxis = list(
      labels = list(style = list(fontSize = "15px"), useHTML = TRUE)
    )
  )) %>%
  hc_chart(backgroundColor="#FFFFFF", style = list(fontFamily = "Open Sans",
                                                   color = "black")) 




#------
# Barras sector vs nivel
#------

conflictos <- df %>% 
  mutate(year = lubridate::year(fecha))

temp <- merge((conflictos %>% select(id, sector_a) %>% distinct()),
              (conflictos %>% filter(nivel > 0) %>% select(id, nivel) %>% distinct())) %>% 
  group_by(sector_a, nivel) %>% 
  summarise(cantidad = n()) %>% 
  ungroup() %>% 
  mutate(porcentaje = prop.table(cantidad)*100) %>% 
  arrange(desc(cantidad)) %>% 
  group_by(sector_a) %>% 
  mutate(total = sum(cantidad)) %>% select(-porcentaje) %>% 
  ungroup() %>%
  mutate(nivel = paste0("Nivel: ",nivel)) %>% 
  spread(nivel, cantidad) %>% 
  arrange(desc(total))



temp[is.na(temp)] <- 0

a_1 <- as.data.frame(temp) %>% rename(Demandante = sector_a)



categories_column <- "Demandante"
measure_columns <- c(colnames(a_1[3:length(a_1)]))


hbr_sn <- highchart() %>%
  hc_xAxis(categories = a_1[, categories_column],
           title = categories_column)


invisible(lapply(measure_columns, function(column) {
  hbr_sn <<-
    hc_add_series(hc = hbr_sn, name = column,
                  data = a_1[, column])
}))



hbr_sector_nivel <- hbr_sn %>%
  hc_chart(type = "bar") %>%
  hc_plotOptions(series = list(stacking = "percent")) %>%
  hc_legend(reversed = TRUE) %>% 
  hc_colors(c("#06D6A0", 
              "#C6A659",
              "#82769D",
              "#E01F52",
              "#073B4C"
  )) %>%
  hc_tooltip(enabled = T, valueDecimals = 0, borderWidth = 0.01,
             crosshairs = TRUE, shared = TRUE, backgroundColor = "white",
             style = list(fontFamily = "Open Sans",
                          color = "black", fontSize = 14),
             headerFormat = "<br><b>{point.key}</b><br>
                              <br>Total: <b>{point.total}</b><br>") %>%
  hc_chart(backgroundColor="#FFFFFF", style = list(fontFamily = "Open Sans",
                                                   color = "black"))




#-----
# Tortas ALCANCE 
#------

conflictos <- df %>% 
  mutate(year = lubridate::year(fecha))

df1 <- conflictos %>% filter(year >= 2010) %>%  select(id, alcance) %>% distinct() %>% 
  group_by(alcance) %>% 
  summarise(frecuencia = n()) %>% 
  ungroup() %>% 
  mutate(porcentaje = round(prop.table(frecuencia)*100,2)) %>% 
  arrange(alcance)





pie_alcance_total <- df1 %>%
  hchart(
    "pie", hcaes(x = alcance, y = frecuencia),
    name = "Total Conflicots"
  ) %>% 
  hc_tooltip(
    valueDecimals = 2, borderWidth = 0.001,
    style = list(fontFamily = "Open Sans", fontSize = 15),
    pointFormat=paste("<br>Alcance: <b>{point.alcance}</b><br>
                      Cantidad: <b>{point.frecuencia}</b><br>
                      Porcentaje: <b>{point.porcentaje} % </b>"),
    headerFormat = "",
    fontFamily = "Open Sans",
    backgroundColor = "white") %>% 
  hc_colors(c("#E01F52", "#C6A659", "#06D6A0", "#466B77", "#073B4C", 
              "#FCBF49", "#F77F00")) 

# Facet pies

df2 <- conflictos %>% filter(year >= 2010) %>%  select(id, alcance, year) %>% distinct() %>% 
  group_by(alcance, year) %>% 
  summarise(frecuencia = n()) %>% 
  ungroup() %>% 
  spread(year, frecuencia) %>% 
  arrange(alcance)

df2[is.na(df2)] <- 0

create_hc <- function(t) {
  
  temp1 <- df2[c(1,t)]
  
  
  nombre <- colnames(temp1)[2]
  colnames(temp1)[2] <- "value"
  
  hc_pie <- temp1 %>%
    hchart(
      "pie", hcaes(x = alcance, y = value),
      name = "Total Conflicots"
    ) %>% 
    hc_tooltip(
      valueDecimals = 2, borderWidth = 0.001, backgroundColor = "white",
      style = list(fontFamily = "Open Sans", fontSize = 15),
      pointFormat=paste("<br>Alcance: <b>{point.alcance}</b><br>
                      Cantidad: <b>{point.value}</b>"),
      headerFormat = "") %>% 
    hc_colors(c("#E01F52", "#C6A659", "#06D6A0", "#466B77", "#073B4C", 
                "#FCBF49", "#F77F00")) %>% 
    hc_title(text = paste0("Año ", nombre)) %>% 
    hc_chart(style = list(fontFamily = "Open Sans"))
  #              style = list(useHTML = TRUE, 
  #                           fontSize = "18",
  #                           fontFamily = "Open Sans"))
  
  
  
  hc_pie
  
}


facet_alcance_pie <- c(2:12) %>%
  map(create_hc)


#-----
# Torta AMBITO
#------

conflictos <- df %>% 
  mutate(year = lubridate::year(fecha))

df1 <- conflictos %>% filter(year >= 2010) %>%  select(id, ambito) %>% distinct() %>% 
  group_by(ambito) %>% 
  summarise(frecuencia = n()) %>% 
  ungroup() %>% 
  mutate(porcentaje = round(prop.table(frecuencia)*100,2)) %>% 
  arrange(ambito)

pie_ambito_total <- df1 %>%
  hchart(
    "pie", hcaes(x = ambito, y = frecuencia),
    name = "Total Conflicots"
  ) %>% 
  hc_tooltip(
    valueDecimals = 2, borderWidth = 0.8,
    style = list(fontFamily = "Open Sans", fontSize = 15),
    pointFormat=paste("<br>Ámbito: <b>{point.ambitoe}</b><br>
                      Cantidad: <b>{point.frecuencia}</b><br>
                      Porcentaje: <b>{point.porcentaje} % </b>"),
    headerFormat = "",
    fontFamily = "Open Sans",
    borderWidth = 0.8) %>% 
  hc_colors(c("#81B29A", "#63585F", 
              "#B4B5BA", "#261F23", "#575D7C", "#9194C6", "#75184D"
  )) %>% 
  hc_chart(style = list(fontFamily = "Open Sans"))


# Facet pies


df2 <- conflictos %>% filter(year >= 2010) %>%  select(id, ambito, year) %>% distinct() %>% 
  group_by(ambito, year) %>% 
  summarise(frecuencia = n()) %>% 
  ungroup() %>% 
  spread(year, frecuencia) %>% 
  arrange(ambito)

df2[is.na(df2)] <- 0

create_hc <- function(t) {
  
  temp1 <- df2[c(1,t)]
  
  
  nombre <- colnames(temp1)[2]
  colnames(temp1)[2] <- "value"
  
  hc_pie <- temp1 %>%
    hchart(
      "pie", hcaes(x = ambito, y = value),
      name = "Total Conflicots"
    ) %>% 
    hc_tooltip(
      valueDecimals = 2, borderWidth = 0.01, backgroundColor = "white",
      style = list(fontFamily = "Open Sans", fontSize = 15),
      pointFormat=paste("<br>Ámbito: <b>{point.ambito}</b><br>
                      Cantidad: <b>{point.value}</b>"),
      headerFormat = "") %>% 
    hc_colors(c("#81B29A", "#63585F", 
                "#B4B5BA", "#261F23", "#575D7C", "#9194C6", "#75184D")) %>% 
    hc_title(text = paste0("Año ", nombre)) %>% 
    hc_chart(style = list(fontFamily = "Open Sans"))
  
  
  hc_pie
  
}


facet_ambito_pie <- c(2:12) %>%
  map(create_hc)



#----------
# Tree map demandante años
#----------

conflictos <- df %>% 
  mutate(year = lubridate::year(fecha))

conflictos %>% 
  count(sector_a, year) %>% 
  mutate(
    porcentaje = prop.table(n)*100,
    porcentaje = round(porcentaje, 3)
  ) %>% 
  mutate_if(is.character, replace_na, "Sin clasficación") %>% 
  rename(value = n) %>% 
  arrange(sector_a, desc(value))-> temp

colores <- temp %>% select(sector_a) %>% unique

colores %<>% 
  mutate(
    color = c(rep(c("#2C6E49", "#E07A5F", "#3D405B", "#81B29A", "#63585F",
                    "#E01F52", "#C6A659", "#06D6A0", "#466B77", "#073B4C", 
                    "#D8B970", "#5FA1B7", "#118AB2", "#BAAB89", "#1E4D5C", 
                    "#B4B5BA", "#261F23", "#575D7C", "#9194C6", "#75184D"), 2), 
              "#FCBF49", "#F77F00", "#1478AA")
  )

temp %<>% merge(., colores, alll.x = T)


lvl_opts <-  list(
  list(
    level = 1,
    borderWidth = 0,
    borderColor = "transparent",
    dataLabels = list(
      enabled = TRUE,
      align = "left",
      verticalAlign = "top",
      style = list(fontSize = "12px", textOutline = FALSE, color = "white")
    )
  ),
  list(
    level = 2,
    borderWidth = 0,
    borderColor = "transparent",
    colorVariation = list(key = "brightness", to = 0.250),
    dataLabels = list(enabled = FALSE),
    style = list(fontSize = "8px", textOutline = FALSE, color = "white")
  )
)

cols <- temp %>% pull(color) %>% unique

hchart(
  data_to_hierarchical(temp, c(sector_a, year), porcentaje, colors = cols),
  type = "treemap",
  levelIsConstant = T,
  allowDrillToNode = T,
  drillUpButton = list(
    text = "< Volver"
  ),
  levels = lvl_opts,
  tooltip = list(valueDecimals = FALSE)
) %>% 
  hc_chart(
    style = list(fontFamily = "Open Sans")
  ) %>% 
  hc_size(height = 700)  -> tree_map_demandante_year



