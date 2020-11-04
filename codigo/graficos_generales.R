library(highcharter)
library(tidyverse)
library(magrittr)
library(lubridate)

source("codigo/funciones.R")
Sys.setlocale(locale = "es_ES.UTF-8")
options(OutDec= ",") # decimales con comma

df <- rio::import("input/base_limpia.xlsx")

colores <- c("#264653","#2a9d8f","#e9c46a","#f4a261","#e76f51", "#e63946","#d90429", "#50514f", "#293241")
colores_1 <- c("#222438","#262842","#3D405B","#CCF0F9","#870845", "#753855", "#E07A5F", "#F2CC8F", "#5BBF8F", "#BBEAD3")
colores_2  <- c("#262842", "#5BBF8F", "#BBEAD3", "#870845", "#222438", "#E07A5F", "#3D405B", "#F2CC8F", "#CCF0F9", "#753855")

# razones conflicto
df %>% 
  select(id, tipo) %>% 
  unique() %>% 
  count(tipo) %>% 
  filter(tipo != "Otro") %>% 
  mutate(
    x_100 = round(n/10, 0),
    prop = prop.table(n)*100,
    prop = round(prop, 2),
    n = format(n, nsmall = 2, big.mark="."),
    prop = format(prop, nsmall = 2, big.mark=".")
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
      text = "cada cuadrado representa a 10 conflictos (enero 2010 - junio 2020)",
      style = list(fontFamily = "Open Sans", fontSize = 13)
    ) %>% 
  hc_legend(layout = "horizontal") -> razones_conflicto
 
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
  geom_text(aes(x = 1, y = 5.5, label = dia), family = "Open Sans ExtraBold",
            size = 17, color = "#222438") +
  facet_wrap(~etiqueta) + 
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA),
    strip.background = element_rect(fill = "lightgray", colour = NA),
    strip.text =  element_text(color = "#222438", family = "Open Sans Bold", size = 18),
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

# reducir sectores

df$sector_a %<>% gsub("Centrales obreras", "Central obrera", .)
df$sector_a %<>% gsub("Ejecutivo departamental", "Gobiernos departamentales", .)
df$sector_a %<>% gsub("Ejecutivo municipal", "Gobiernos municipales", .)
df$sector_a %<>% gsub("Legislativo departamental", "Gobiernos departamentales", .)
df$sector_a %<>% gsub("Legislativo municipal", "Gobiernos municipales", .)
df$sector_a %<>% gsub("Legislativo nacional", "Gobierno central", .)
df$sector_a %<>% gsub("Junta de Vecinos de Achachicala rechaza", "Vecinal/comunal", .)

temp <- which(df$sector_a == "Salud trabajadores sector privado")
df[temp, "sub_sector_a"] <- "Personal Médico y de Servicio sector privado"

temp <- which(df$sector_a == "Salud trabajadores sector público")
df[temp, "sub_sector_a"] <- "Personal Médico y de Servicio sector público"

df$sector_a %<>% gsub("Salud trabajadores sector privado", "Salud", .)
df$sector_a %<>% gsub("Salud trabajadores sector público", "Salud", .)

temp <- which(df$sector_a == "Grandes organizaciones empresariales")
df[temp, "sub_sector_a"] <- "Grandes organizaciones empresariales"
df$sector_a %<>% gsub("Grandes organizaciones empresariales", "Organizaciones empresariales", .)
	
temp <- which(df$sector_a == "Sector empresarial")
df[temp, "sub_sector_a"] <- "Sector empresarial"
df$sector_a %<>% gsub("Sector empresarial", "Organizaciones empresariales", .)

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

col <- rep(c("#E01F52", "#C6A659", "#06D6A0", "#466B77", "#073B4C", 
      "#D8B970", "#5FA1B7", "#118AB2", "#BAAB89", "#1E4D5C", 
      "#2C6E49", "#E07A5F", "#3D405B", "#81B29A", "#63585F", 
      "#B4B5BA", "#261F23", "#575D7C", "#9194C6", "#75184D"), 2)

col <- col[1: nrow(colores)]

colores %<>% 
  mutate(
    color = col
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
      style = list(fontSize = "13px", textOutline = FALSE, color = "black")
    )
  ),
  list(
    level = 2,
    borderWidth = 0,
    borderColor = "transparent",
    colorVariation = list(key = "brightness", to = 0.250),
    dataLabels = list(
      enabled = F,
      align = "left",
      verticalAlign = "top",
      style = list(fontSize = "13px", textOutline = FALSE, color = "white")
    ),
    style = list(fontSize = "9px", textOutline = FALSE, color = "white")
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
  tooltip = list(valueDecimals = 2) 
) %>% 
  hc_chart(
    style = list(fontFamily = "Open Sans")
  ) %>% 
  hc_size(height = 700) %>% 
  hc_tooltip(backgroundColor =  "white", borderWidth =  0.001, valueSuffix = "%") %>% 
  hc_credits(
    enabled = TRUE,
    text = "(enero 2010 - junio 2020)",
    style = list(fontFamily = "Open Sans", fontSize = 13)
  ) -> tree_map_1

# treemap de quienes son los demandados
# arreglo de categorías
df$sector_b %<>% gsub("Centrales obreras", "Central obrera", .)
df$sector_b %<>% gsub("Ejecutivo departamental", "Gobiernos departamentales", .)
df$sector_b %<>% gsub("Ejecutivo municipal", "Gobiernos municipales", .)
df$sector_b %<>% gsub("Legislativo departamental", "Gobiernos departamentales", .)
df$sector_b %<>% gsub("Legislativo municipal", "Gobiernos municipales", .)
df$sector_b %<>% gsub("Legislativo nacional", "Gobierno central", .)
df$sector_b %<>% gsub("Junta de Vecinos de Achachicala rechaza", "Vecinal/comunal", .)

temp <- which(df$sector_b == "Salud trabajadores sector privado")
df[temp, "sub_sector_b"] <- "Personal Médico y de Servicio sector privado"

temp <- which(df$sector_b == "Salud trabajadores sector público")
df[temp, "sub_sector_b"] <- "Personal Médico y de Servicio sector público"

df$sector_b %<>% gsub("Salud trabajadores sector privado", "Salud", .)
df$sector_b %<>% gsub("Salud trabajadores sector público", "Salud", .)

temp <- which(df$sector_b == "Grandes organizaciones empresariales")
df[temp, "sub_sector_b"] <- "Grandes organizaciones empresariales"
df$sector_b %<>% gsub("Grandes organizaciones empresariales", "Organizaciones empresariales", .)

temp <- which(df$sector_b == "Sector empresarial")
df[temp, "sub_sector_b"] <- "Sector empresarial"
df$sector_b %<>% gsub("Sector empresarial", "Organizaciones empresariales", .)


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
col <- c(rep(c("#E01F52", "#C6A659", "#06D6A0", "#466B77", "#073B4C", 
               "#D8B970", "#5FA1B7", "#118AB2", "#BAAB89", "#1E4D5C", 
               "#2C6E49", "#E07A5F", "#3D405B", "#81B29A", "#63585F", 
               "#B4B5BA", "#575D7C", "#9194C6", "#75184D"), 2), 
         "#FCBF49", "#F77F00", "#1478AA", "#4F000B")

col <- col[1:nrow(colores)]

colores %<>% 
  mutate(
    color = col
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
      style = list(fontSize = "13px", textOutline = FALSE, color = "black")
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
  tooltip = list(valueDecimals = 2)
) %>% 
  hc_chart(
    style = list(fontFamily = "Open Sans")
  ) %>% 
  hc_tooltip(backgroundColor =  "white", borderWidth =  0.001, valueSuffix = "%") %>% 
  hc_size(height = 700) %>% 
  hc_credits(
    enabled = TRUE,
    text = "(enero 2010 - junio 2020)",
    style = list(fontFamily = "Open Sans", fontSize = 13)
  ) -> tree_map_3

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

#-----------------
# donde: motion
#-----------------
lat_lon <- read_csv("input/lat_lon_339_cent.csv") %>% select(CODIGO, name = MUNICIPIO, lat, lon)
df %>% 
  mutate(año = lubridate::year(fecha)) %>% 
  group_by(año, departamento, localidad_o_municipio, codigo) %>% 
  count() %>% 
  filter(año > 2009) %>% 
  ungroup() %>% 
  rename(CODIGO = codigo) %>%
  filter(!is.na(CODIGO)) %>% 
  group_by(CODIGO, año) %>% 
  summarise(z = sum(n)) %>% 
  spread(año, z) %>% 
  gather(año, value, -CODIGO) %>% 
  select(-año) %>% 
  group_by(CODIGO) %>% 
  nest() -> temp

temp %<>% 
  ungroup() %>% 
  mutate(sequence = map(temp$data, pull)) %>% 
  select(-data) %>% 
  left_join(., lat_lon) 


hcmap(
  "countries/bo/bo-all",
  showInLegend = FALSE,
  borderColor = "#f1faee",
  borderWidth = 0.0001,
  nullColor = "#222438"
) %>% 
  hc_add_series(data = temp, type = "mapbubble",
                minSize = "2",
                maxSize = "50",
                color = hex_to_rgba("#E07A5F", alpha = 1),
                borderColor = "lightgray",
                showInLegend = FALSE) %>% 
  hc_motion(enabled = TRUE, series = c(0,1), labels = paste0("año: ", c(2010:2020)) ,
            autoPlay = TRUE, 
            updateInterval = 800, magnet = list(step =  1)) %>% 
  hc_tooltip(enabled = T, valueDecimals = 0, borderWidth = 0.01, 
             style = list(fontFamily = "Open Sans"), backgroundColor =  "white",
             headerFormat = "") %>% 
  hc_size(height = 600) -> mapa_motion

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
  arrange(desc(cantidad)) %>% select(from = sector_a, to = sector_b, weight = cantidad) %>% 
  filter(
    from != "Otro",
    to != "Otro"
  ) 
  
  
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
    style = list(fontFamily = "Open Sans", fontSize = 13),
    borderWidth = 0.01,
    backgroundColor =  "white", 
    valueSuffix = "%", 
    enabled = T, valueDecimals = 2
  ) %>%
  hc_plotOptions(
    dependencywheel = list(
      dataLabels = list(
        enabled = T,
        align = "left",
        verticalAlign = "top",
        style = list(fontSize = "13px", textOutline = FALSE, color = "black")
      )
    )
  ) %>% 
  hc_credits(
    enabled = TRUE,
    text = "(enero 2010 - junio 2020)",
    style = list(fontFamily = "Open Sans", fontSize = 13)
  )


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
  mutate(
    nivel_1 = case_when(
      nivel == 1 ~ "Latente",
      nivel == 2 ~ "Manifiesto",
      nivel == 3 ~ "Confrontación",
      nivel == 4 ~ "Enfrentamiento violento",
      nivel == 5 ~ "Crisis de gobernabilidad",
    )
  ) %>% 
  mutate(total = format(total, nsmall = 2, big.mark=".")) %>% 
  arrange(nivel)

hchart(temp, "column", hcaes(year, cantidad, group = nivel_1)) %>% 
  hc_plotOptions(series = list(stacking = "normal"), borderWidth = 0) %>%
  hc_legend(reversed = TRUE) %>% 
  hc_colors(c("#06D6A0", 
              "#C6A659",
              "#82769D",
              "#E01F52",
              "#073B4C"
  )) %>%
  hc_tooltip(enabled = T, valueDecimals = 0, borderWidth = 0.01,
             crosshairs = F, shared = T, backgroundColor = "white", sort = T,
             style = list(fontFamily = "Open Sans",
                          color = "black", fontSize = 13),
             headerFormat = "") %>%
  hc_chart(backgroundColor="#FFFFFF", borderColor = "transparent", style = list(fontFamily = "Open Sans",
                                                                                color = "black")) %>% 
  hc_xAxis(title = list(text = "Año")) %>% 
  hc_yAxis(title = list(text = "Frecuencia")) %>% 
  hc_credits(
    enabled = TRUE,
    text = "(enero 2010 - junioo 2020)",
    style = list(fontFamily = "Open Sans", fontSize = 13)
  ) -> hbr_gestion_nivel
  
# version en porcenajes %

conflictos <- df %>% 
  mutate(year = lubridate::year(fecha))

temp <- conflictos %>% filter(nivel > 0, year >= 2010) %>% select(id, nivel, year) %>% distinct() %>% 
  group_by(year, nivel) %>% 
  summarise(cantidad = n()) %>% 
  group_by(year) %>% 
  mutate(total = sum(cantidad)) %>%
  ungroup() %>%
  mutate(
    nivel = case_when(
      nivel == 1 ~ "Latente",
      nivel == 2 ~ "Manifiesto",
      nivel == 3 ~ "Confrontación",
      nivel == 4 ~ "Enfrentamiento violento",
      nivel == 5 ~ "Crisis de gobernabilidad",
    )
  ) %>% mutate(cantidad = round((cantidad/total)*100,2)) %>% 
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



hbr_gestion_nivel_perc <- hbr_yn %>%
  hc_chart(type = "column") %>%
  hc_plotOptions(series = list(stacking = "normal"), borderWidth = 0) %>%
  hc_legend(reversed = TRUE) %>% 
  hc_colors(c("#06D6A0", 
              "#C6A659",
              "#82769D",
              "#E01F52",
              "#073B4C"
  )) %>%
  hc_tooltip(enabled = T, valueDecimals = 2, borderWidth = 0.01, sort = T,
             crosshairs = T, shared = TRUE, backgroundColor = "white", valueSuffix = "%",
             style = list(fontFamily = "Open Sans",
                          color = "black", fontSize = 13),
             headerFormat = "") %>%
  hc_chart(backgroundColor="#FFFFFF", borderColor = "transparent", style = list(fontFamily = "Open Sans",
                                                                                color = "black")) %>% 
  hc_xAxis(title = list(text = "Año")) %>% 
  hc_yAxis(title = list(text = "Porcentaje %"),
           max = 100) %>% 
  hc_credits(
    enabled = TRUE,
    text = "(enero 2010 - junio 2020)",
    style = list(fontFamily = "Open Sans", fontSize = 13)
  )

  
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
  mutate(
    nivel = case_when(
      nivel == 1 ~ "Latente",
      nivel == 2 ~ "Manifiesto",
      nivel == 3 ~ "Confrontación",
      nivel == 4 ~ "Enfrentamiento violento",
      nivel == 5 ~ "Crisis de gobernabilidad",
    )
  ) %>% 
  spread(nivel, cantidad) %>% 
  arrange(desc(total)) %>% 
  select(sector_a, total, Manifiesto, Latente,`Enfrentamiento violento`,`Crisis de gobernabilidad`,Confrontación) %>% 
  filter(total >= 33) # 33 equivale a hidrocarburos solicitado por cliente



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
  hc_plotOptions(series = list(stacking = "normal")) %>%
  hc_legend(reversed = TRUE) %>% 
  hc_colors(c("#06D6A0", 
              "#C6A659",
              "#82769D",
              "#E01F52",
              "#073B4C"
  )) %>%
  hc_tooltip(enabled = T, valueDecimals = 0, borderWidth = 0.01, sort = T,
             crosshairs = TRUE, shared = TRUE, backgroundColor = "white",
             style = list(fontFamily = "Open Sans",
                          color = "black", fontSize = 13),
             headerFormat = "") %>%
  hc_chart(backgroundColor="#FFFFFF", style = list(fontFamily = "Open Sans",
                                                   color = "black")) %>% 
  hc_size(height = 1200) %>% 
  hc_credits(
    enabled = TRUE,
    text = "(enero 2010 - junio 2020)",
    style = list(fontFamily = "Open Sans", fontSize = 13)
  )


# alternativa en porcentajes

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
  mutate(
    nivel = case_when(
      nivel == 1 ~ "Latente",
      nivel == 2 ~ "Manifiesto",
      nivel == 3 ~ "Confrontación",
      nivel == 4 ~ "Enfrentamiento violento",
      nivel == 5 ~ "Crisis de gobernabilidad",
    )
  ) %>%
  mutate(cantidad = round((cantidad/total)*100,2)) %>% 
  spread(nivel, cantidad) %>% 
  arrange(desc(total)) %>% 
  select(sector_a, total, Manifiesto, Latente,`Enfrentamiento violento`,`Crisis de gobernabilidad`,Confrontación) %>% 
  filter(total >= 33)  # 33 equivale a hidrocarburos solicitado por cliente


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



hbr_sector_nivel_perc <- hbr_sn %>%
  hc_chart(type = "bar") %>%
  hc_plotOptions(series = list(stacking = "normal")) %>%
  hc_legend(reversed = TRUE) %>% 
  hc_colors(c("#06D6A0", 
              "#C6A659",
              "#82769D",
              "#E01F52",
              "#073B4C"
  )) %>%
  hc_tooltip(enabled = T, valueDecimals = 2, borderWidth = 0.01,
             crosshairs = TRUE, shared = TRUE, backgroundColor = "white",
             style = list(fontFamily = "Open Sans",valueSuffix = "%",
                          color = "black", fontSize = 13),
             headerFormat = "<br><b>{point.key}</b><br>") %>%
  hc_chart(backgroundColor="#FFFFFF", style = list(fontFamily = "Open Sans",
                                                   color = "black")) %>% 
  hc_yAxis(title = list(text = "Porcentaje %"),
           max = 100) %>% 
  hc_size(height = 1200) %>% 
  hc_credits(
    enabled = TRUE,
    text = "(enero 2010 - junio 2020)",
    style = list(fontFamily = "Open Sans", fontSize = 13)
  )



#-----
# Tortas ALCANCE 
#------

conflictos <- df %>% 
  mutate(year = lubridate::year(fecha))

df1 <- conflictos %>% filter(year >= 2010) %>%  select(id, alcance) %>% distinct() %>% 
  group_by(alcance) %>% 
  summarise(frecuencia = n()) %>% 
  ungroup() %>% 
  mutate(
    porcentaje = round(prop.table(frecuencia)*100,2),
    porcentaje = format(porcentaje, nsmall = 2, big.mark=",")
  ) %>% 
  arrange(alcance)


pie_alcance_total <- df1 %>%
  hchart(
    "pie", hcaes(x = alcance, y = frecuencia),
    name = "Total Conflictos", borderWidth = 0
  ) %>% 
  hc_tooltip(
    valueDecimals = 2, borderWidth = 0.001,
    style = list(fontFamily = "Open Sans", fontSize = 13),
    pointFormat=paste("<br>Alcance: <b>{point.alcance}</b><br>
                      Porcentaje: <b>{point.porcentaje} % </b>"),
    headerFormat = "",
    fontFamily = "Open Sans",
    backgroundColor = "white") %>% 
    hc_colors(c("#E01F52", "#C6A659", "#06D6A0", "#466B77", "#073B4C", 
              "#FCBF49", "#F77F00")) %>% 
    hc_chart(style = list(fontFamily = "Open Sans")) %>% 
  hc_credits(
    enabled = TRUE,
    text = "(enero 2010 - junio 2020)",
    style = list(fontFamily = "Open Sans", fontSize = 13)
  )

# Facet pies

df2 <- conflictos %>% filter(year >= 2010) %>%  select(id, alcance, year) %>% distinct() %>% 
  group_by(alcance, year) %>% 
  summarise(frecuencia = n()) %>% 
  ungroup() %>% 
  group_split(year) %>% 
  map(., ~mutate(., frecuencia = prop.table(frecuencia)*100)) %>% 
  bind_rows() %>% 
  mutate_if(is.numeric, round, 2) %>% 
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
      name = "Total Conflicots", borderWidth = 0, 
    ) %>% 
    hc_tooltip(
      valueDecimals = 2, borderWidth = 0.001, backgroundColor = "white",
      style = list(fontFamily = "Open Sans", fontSize = 13),
      pointFormat=paste("<br>Alcance: <b>{point.alcance}</b><br>
                      Cantidad: <b>{point.value} %</b>"),
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
  mutate(
    porcentaje = round(prop.table(frecuencia)*100,2),
    porcentaje = format(porcentaje, nsmall = 2, big.mark=",")
  ) %>% 
  arrange(ambito)

pie_ambito_total <- df1 %>%
  hchart(
    "pie", hcaes(x = ambito, y = frecuencia),
    name = "Total Conflictos", borderWidth = 0
  ) %>% 
  hc_tooltip(
    valueDecimals = 2, borderWidth = 0.01, backgroundColor = "white",
    style = list(fontFamily = "Open Sans"),
    pointFormat=paste("Ámbito: <b>{point.ambito}</b><br>
                      Porcentaje: <b>{point.porcentaje} % </b>"),
    headerFormat = "",
    fontFamily = "Open Sans",
    borderWidth = 0.8) %>% 
  hc_colors(c("#81B29A", "#63585F", 
              "#B4B5BA", "#261F23", "#575D7C", "#9194C6", "#75184D"
  )) %>% 
  hc_chart(style = list(fontFamily = "Open Sans")) %>% 
  hc_credits(
    enabled = TRUE,
    text = "(enero 2010 - junio 2020)",
    style = list(fontFamily = "Open Sans", fontSize = 13)
  )


# Facet pies

df2 <- conflictos %>% filter(year >= 2010) %>%  select(id, ambito, year) %>% distinct() %>% 
  group_by(ambito, year) %>% 
  summarise(frecuencia = n()) %>% 
  ungroup() %>% 
  group_split(year) %>% 
  map(., ~mutate(., frecuencia = prop.table(frecuencia)*100)) %>% 
  bind_rows() %>% 
  mutate_if(is.numeric, round, 2) %>% 
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
      name = "Total Conflicots", borderWidth = 0
    ) %>% 
    hc_tooltip(
      valueDecimals = 2, borderWidth = 0.01, backgroundColor = "white",
      style = list(fontFamily = "Open Sans", fontSize = 13),
      pointFormat=paste("<br>Ámbito: <b>{point.ambito}</b><br>
                      Porcentaje: <b>{point.value} %</b>"),
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

col <- c(rep(c("#2C6E49", "#E07A5F", "#3D405B", "#81B29A", "#63585F",
        "#E01F52", "#C6A659", "#06D6A0", "#466B77", "#073B4C", 
        "#D8B970", "#5FA1B7", "#118AB2", "#BAAB89", "#1E4D5C", 
        "#B4B5BA", "#261F23", "#575D7C", "#9194C6", "#75184D"), 2), 
  "#FCBF49", "#F77F00", "#1478AA")

col <- col[1:nrow(colores)]

colores %<>% 
  mutate(
    color = col
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
      style = list(fontSize = "13px", textOutline = FALSE, color = "black")
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
  tooltip = list(valueDecimals = 2)
) %>% 
  hc_chart(
    style = list(fontFamily = "Open Sans")
  ) %>% 
  hc_tooltip(backgroundColor = "white", borderWidth = 0.001, valueSuffix = "%") %>% 
  hc_size(height = 700) %>% 
  hc_credits(
    enabled = TRUE,
    text = "(enero 2010 - junio 2020)",
    style = list(fontFamily = "Open Sans", fontSize = 13)
  ) -> tree_map_demandante_year


#-----------
#  Tree map Demandado vs años
#-----------
conflictos <- df %>% 
  mutate(year = lubridate::year(fecha))

conflictos %>% 
  count(sector_b, year) %>% 
  mutate(
    porcentaje = prop.table(n)*100,
    porcentaje = round(porcentaje, 3)
  ) %>% 
  mutate_if(is.character, replace_na, "Sin clasficación") %>% 
  rename(value = n) %>% 
  arrange(sector_b, desc(value))-> temp

colores <- temp %>% select(sector_b) %>% unique

col <- c(rep(c("#2C6E49", "#E07A5F", "#3D405B", "#81B29A", "#63585F",
               "#E01F52", "#C6A659", "#06D6A0", "#466B77", "#073B4C", 
               "#D8B970", "#5FA1B7", "#118AB2", "#BAAB89", "#1E4D5C", 
               "#B4B5BA", "#261F23", "#575D7C", "#9194C6", "#75184D"), 2), 
         "#FCBF49", "#F77F00")

col <- col[1:nrow(colores)]


colores %<>% 
  mutate(
    color = col
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
      style = list(fontSize = "13px", textOutline = FALSE, color = "black")
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
  data_to_hierarchical(temp, c(sector_b, year), porcentaje, colors = cols),
  type = "treemap",
  levelIsConstant = T,
  allowDrillToNode = T,
  drillUpButton = list(
    text = "< Volver"
  ),
  levels = lvl_opts,
  tooltip = list(valueDecimals = 2)
) %>% 
  hc_chart(
    style = list(fontFamily = "Open Sans")
  ) %>% 
  hc_tooltip(backgroundColor = "white", borderWidth = 0.001, valueSuffix = "%") %>% 
  hc_size(height = 700) %>% 
  hc_credits(
    enabled = TRUE,
    text = "(enero 2010 - junio 2020)",
    style = list(fontFamily = "Open Sans", fontSize = 13)
  ) -> tree_map_demandado_year

#---------
#  packed bubble departamento sector demandante
#--------
conflictos <- df %>% 
  mutate(year = lubridate::year(fecha))

hchart(conflictos %>% select(id, departamento, sector_a) %>% 
                      distinct() %>% 
                      group_by(departamento, sector_a) %>% 
                      summarise(frecuencia = n()),
                    "packedbubble",
                    hcaes(name = sector_a, value = frecuencia, group = departamento)) %>% 
  hc_colors(c("#D8B970", "#5FA1B7", "#118AB2", "#BAAB89", "#1E4D5C", 
              "#2C6E49", "#E07A5F", "#3D405B", "#81B29A", "#63585F", 
              "#B4B5BA")) %>% 
  hc_plotOptions(packedbubble = list(
    minSize = 5,
    maxSize = 100,
    zMin = 0,
    zMax = 2000,
    layoutAlgorithm = list(
      gravitationalConstant = 0.01,
      splitSeries = T,
      seriesInteraction = F,
      dragBetweenSeries = T,
      parentNodeLimit = T
    )
  )
  ) %>% 
  hc_tooltip(enabled = T, valueDecimals = 2, borderWidth = 0.001, 
             style = list(fontFamily = "Open Sans"), backgroundColor =  "white",
             pointFormat=paste("Departamento: <b>{point.departamento}</b><br>
                               Sector demandante: <b>{point.sector_a}</b><br>
                               Total: <b>{point.frecuencia}</b>"),
             headerFormat = "") %>% 
  hc_chart(style = list(fontFamily = "Open Sans")) %>% 
  hc_credits(
    enabled = TRUE,
    text = "(enero 2010 - junio 2020)",
    style = list(fontFamily = "Open Sans", fontSize = 13)
  ) -> hc_pa_bub



#---------
#  packed bubble tipo anual (tiempo)
#--------

conflictos <- df %>% 
  mutate(year = lubridate::year(fecha))

hchart(conflictos %>% mutate(gestion = lubridate::year(fecha)) %>% 
                       filter(gestion >= 2010) %>% 
                       select(id, tipo, gestion) %>% 
                       # distinct() %>% 
                       group_by(gestion, tipo) %>% 
                       summarise(frecuencia = n()),
                     "packedbubble",
                     hcaes(name = tipo, value = frecuencia, group = gestion)) %>% 
  hc_colors(c("#E01F52", "#C6A659", "#06D6A0", "#466B77", "#073B4C", 
              "#D8B970", "#5FA1B7", "#118AB2", "#BAAB89", "#1E4D5C", 
              "#2C6E49", "#E07A5F")) %>% 
  hc_plotOptions(packedbubble = list(
    minSize = 5,
    maxSize = 100,
    zMin = 0,
    zMax = 5000,
    layoutAlgorithm = list(
      gravitationalConstant = 0.01,
      splitSeries = T,
      seriesInteraction = F,
      dragBetweenSeries = T,
      parentNodeLimit = T
    )
  )
  ) %>% 
  hc_tooltip(enabled = T, valueDecimals = 2, borderWidth = 0.01, 
             style = list(fontFamily = "Open Sans"), backgroundColor =  "white",
             pointFormat=paste("Año: <b>{point.gestion}</b><br>
                               Tipo demanda: <b>{point.tipo}</b><br>
                               Total eventos: <b>{point.frecuencia}</b>"),
             headerFormat = "") %>% 
  hc_chart(style = list(fontFamily = "Open Sans")) %>% 
  hc_credits(
    enabled = TRUE,
    text = "(enero 2010 - junio 2020)",
    style = list(fontFamily = "Open Sans", fontSize = 13)
  ) -> hc_pa_bub2


#---------
#  alternativa tipo anual
#--------
df %>% 
  mutate(gestion = lubridate::year(fecha)) %>% 
  filter(gestion >= 2010) %>% 
  select(id, tipo, gestion) %>% 
  group_by(gestion, tipo) %>% 
  summarise(frecuencia = n()) %>% 
  ungroup() %>% 
  filter(tipo != "Otro") %>% 
  # group_split(gestion) %>% 
  # map(., ~mutate(., frecuencia = prop.table(frecuencia)*100)) %>% 
  # bind_rows() %>% 
  mutate_if(is.numeric, round, 2) -> temp
  
hchart(temp, "streamgraph", hcaes(gestion, frecuencia, group = tipo),
         label = list(
           enabled = TRUE, minFontSize = 5, maxFontSize = 20,
           style = list(
             fontWeight = 70,
             textOutline = "0.5px gray",
             color = "white"
           )
         )
  ) %>% 
  hc_tooltip(shared = T, table = T, sort = T, borderWidth = 0.001,
             style = list(fontFamily = "Open Sans"), backgroundColor = "white") %>% 
  hc_yAxis(visible = F) %>% 
  hc_xAxis(title = list(text = "")) %>% 
  hc_chart(style = list(fontFamily = "Open Sans")) %>% 
  hc_plotOptions(
    series = list(
      marker = list(radius = 3, enabled = FALSE, symbol = "circle"),
      states = list(hover = list(halo = list(size = 1)))
    )
  ) %>% 
  hc_size(height = 800) %>% 
  hc_colors(colors = col[1:17]) %>% 
  hc_credits(
    enabled = TRUE,
    text = "(enero 2010 - junio 2020)",
    style = list(fontFamily = "Open Sans", fontSize = 13)
  ) %>% 
  hc_legend(layout = "vertical") -> rio_tipo



#------
# Heat map tipos vs medidas
#------

conflictos <- df %>% 
  mutate(year = lubridate::year(fecha))

temp <- conflictos %>% mutate(gestion = lubridate::year(fecha)) %>% 
  filter(gestion >= 2010) %>% 
  select(id, tipo, medida_de_presion) %>% 
  group_by(tipo, medida_de_presion) %>% 
  summarise(frecuencia = n()) %>% 
  filter(tipo != "Otro") %>% 
  spread(medida_de_presion, frecuencia) %>% 
  mutate_if(is.numeric, replace_na, 0) %>% 
  gather(medida_de_presion, frecuencia, -tipo)
  

hchart(temp,
       "heatmap",
       hcaes(
         x = medida_de_presion,
         y = tipo,
         value = frecuencia
       )) %>% 
  hc_colorAxis(
    stops = color_stops(4, c("#222438", "#E07A5F", "#F2CC8F", "#BBEAD3"))
  ) %>% 
  hc_chart(backgroundColor="white", borderColor = "transparent", 
           style=list(fontFamily = "Open Sans", fontSize = 12)) %>% 
  hc_yAxis(mayorGridLineWidth = 1, 
           gridLineColor = "white",
           title = list(
             text = "Tipo de evento")) %>% 
  hc_xAxis(mayorGridLineWidth = 1,
           minorGridLineWidth = 5, 
           gridLineColor = "white", 
           title = list(text = "Medida de presión")) %>% 
  hc_tooltip(enabled = T, valueDecimals = 2, borderWidth = 0.01, 
             style = list(fontFamily = "Open Sans"),
             pointFormat=paste("
                               <b>{point.frecuencia}</b> eventos<br>"),
             headerFormat = "") %>% 
  hc_size(height = 800) %>% 
  hc_credits(
    enabled = TRUE,
    text = "(enero 2010 - junio 2020)",
    style = list(fontFamily = "Open Sans", fontSize = 13)
  ) -> heat_tipo_med


#-------------------------
# medida de presión
#-------------------------
df %>% 
  mutate(
    medida_de_presion = str_replace(medida_de_presion, "Bloqueo de calles y avenidas", "Bloqueo de calles, avenidas o carretereas"),
    medida_de_presion = str_replace(medida_de_presion, "Bloqueo de carreteras", "Bloqueo de calles, avenidas o carretereas"),
    medida_de_presion = str_replace(medida_de_presion, "Huelga de hambre de menos de tres días", "Huelga de hambre"),
    medida_de_presion = str_replace(medida_de_presion, "Huelga de hambre de más de tres días", "Huelga de hambre")
  ) %>% 
  filter(medida_de_presion != "Otra(s)") %>% 
  count(medida_de_presion) %>% 
  remove_missing() %>% 
  mutate(
    prop = prop.table(n)*100,
    prop = round(prop, 2), 
    n_1 = round(n/10, 0)
  ) %>% 
  filter(prop > 5) %>% 
  mutate(
    prop = format(prop, nsmall = 2, big.mark=","),
    n = format(n, nsmall = 2, big.mark=".")
  ) %>% 
  filter(!medida_de_presion %in% c("Anuncio/ amenaza de medidas de presión", "Declaración de estado de emergencia/movilización permanente")) %>% 
  arrange(desc(prop)) -> temp

colores <- c("#264653","#2a9d8f","#e9c46a","#f4a261","#e76f51")

temp %>% 
  hchart(
    "item", 
    hcaes(name = medida_de_presion, y = n_1),
    marker = list(symbol = "circle"),
    showInLegend = TRUE
  ) %>% 
  hc_colors(colors = col) %>% 
  hc_chart(style = list(fontFamily = "Open Sans")) %>% 
  hc_tooltip(enabled = T, valueDecimals = 2, borderWidth = 0.001, 
             style = list(fontFamily = "Open Sans"), backgroundColor =  "white",
             pointFormat =paste("<b>{point.medida_de_presion}</b><br>
                               Medida usada <b>{point.n}</b> veces<br>
                               <b>{point.prop} %</b> sobre el total<br>"),
             headerFormat = "") %>% 
  hc_credits(
    enabled = TRUE,
    text = "cada círculo representa a 10 medidas de presión (enero 2010 - junio 2020)",
    style = list(fontFamily = "Open Sans", fontSize = 13)
  ) %>% 
  hc_legend(layout = "horizontal") -> medidas_presion


# medida de presion sleccionadas
df %>% 
  mutate(
    medida_de_presion = str_replace(medida_de_presion, "Bloqueo de calles y avenidas", "Bloqueo de calles, avenidas o carretereas"),
    medida_de_presion = str_replace(medida_de_presion, "Bloqueo de carreteras", "Bloqueo de calles, avenidas o carretereas"),
    medida_de_presion = str_replace(medida_de_presion, "Huelga de hambre de menos de tres días", "Huelga de hambre"),
    medida_de_presion = str_replace(medida_de_presion, "Huelga de hambre de más de tres días", "Huelga de hambre")
  ) %>% 
  filter(medida_de_presion != "Otra(s)") %>% 
  count(medida_de_presion) %>% 
  remove_missing() %>% 
  mutate(
    prop = prop.table(n)*100,
    prop = round(prop, 2), 
    n_1 = round(n/10, 0), 
    prop = format(prop, nsmall = 2, big.mark=","),
    n = format(n, nsmall = 2, big.mark=".")
  ) %>% 
 filter(medida_de_presion %in% c("Anuncio/ amenaza de medidas de presión", "Declaración de estado de emergencia/movilización permanente", "Ultimátum")) %>% 
 arrange(desc(prop)) -> temp

colores <- c("#264653", "#f4a261","#e76f51")

temp %>% 
  hchart(
    "item", 
    hcaes(name = medida_de_presion, y = n_1),
    marker = list(symbol = "square"),
    showInLegend = TRUE
  ) %>% 
  hc_colors(colors = col) %>% 
  hc_chart(style = list(fontFamily = "Open Sans")) %>% 
  hc_tooltip(enabled = T, valueDecimals = 2, borderWidth = 0.001, 
             style = list(fontFamily = "Open Sans"), backgroundColor =  "white",
             pointFormat =paste("<b>{point.medida_de_presion}</b><br>
                               Medida usada <b>{point.n}</b> veces<br>
                               <b>{point.prop} %</b> sobre el total<br>"),
             headerFormat = "") %>% 
  hc_credits(
    enabled = TRUE,
    text = "cada cuadrado representa a 10 medidas de presión (enero 2010 - junio 2020)",
    style = list(fontFamily = "Open Sans", fontSize = 13)
  ) %>% 
  hc_legend(layout = "horizontal") -> medidas_presion_1



#-------------------------
# salida
#-------------------------
df %>% 
  count(salida) %>% 
  remove_missing() %>% 
  filter(salida != "Otra") %>% 
  mutate(
    salida = str_replace(salida, "Continúa", "Continúa el conflicto"),
    prop = prop.table(n)*100,
    prop = round(prop, 2), 
    n_1 = round(n/10, 0), 
    prop_1 = format(prop, nsmall = 2, big.mark=","),
    n = format(n, nsmall = 2, big.mark=".")
  ) %>% 
  arrange(desc(prop)) -> temp


colores <- c("#264653","#2a9d8f","#e9c46a","#f4a261","#e76f51", "#e63946","#d90429", "#50514f", "#293241")

hchart(temp, "column", hcaes(x = salida, y = prop, color = salida)) %>% 
  hc_tooltip(enabled = T, valueDecimals = 2, borderWidth = 0.001, valueSuffix = "%",
             style = list(fontFamily = "Open Sans"), backgroundColor =  "white",
             pointFormat =paste("<b>{point.salida}: </b>{point.prop_1}%"),
             headerFormat = "") %>% 
  hc_credits(
    enabled = TRUE,
    text = "(enero 2010 - junio 2020)",
    style = list(fontFamily = "Open Sans", fontSize = 13)
  ) %>% 
  hc_chart(style = list(fontFamily = "Open Sans")) %>% 
  hc_yAxis(title = list(
    text = "Porcentaje (%)")) %>% 
  hc_colors(colors = colores) -> salidas
  
  
# highchart() %>% 
#   hc_add_dependency("modules/pattern-fill.js") %>% 
#   hc_size(heigh = 500) %>% 
#   hc_xAxis(type = 'category') %>% 
#   hc_tooltip(
#     borderColor = "#CACACA",
#     pointFormat = 'El porcentaje de la categoría  "<b>{point.name}</b>" es <b>{point.y}%</b>'
#   ) %>% 
#   hc_add_series(
#     type = "column",
#     showInLegend = FALSE,
#     pointWidth = 90,
#     pointPadding = 0.2,
#     borderColor = "transparent",
#     borderWidth = 0,
#     data = list(
#       list(
#         name = "Se desconoce",
#         y = 37,
#         color = list(
#           pattern = list(
#             image = 'https://www.flaticon.com/svg/static/icons/svg/2476/2476199.svg',
#             aspectRatio = 0.3
#           )
#         )
#       ),
#       list(
#         name = 'Continúa el conflicto',
#         y = 36,
#         color = list(
#           pattern = list(
#             image = 'https://www.flaticon.es/svg/static/icons/svg/2345/2345112.svg',
#             aspectRatio = 0.3
#           )
#         )
#       ),
#       list(
#         name = 'Acuerdo total',
#         y = 11,
#         color = list(
#           pattern = list(
#             image = 'https://www.flaticon.com/svg/static/icons/svg/1042/1042600.svg',
#             aspectRatio = 0.7
#           )
#         )
#       ),
#       list(
#         name = 'Acuerdo parcial',
#         y = 7,
#         color = list(
#           pattern = list(
#             image = 'https://www.flaticon.com/svg/static/icons/svg/126/126473.svg',
#             aspectRatio = 1.2
#           )
#         )
#       ),
#       list(
#         name = 'Retroceso de uno<br> de los actores',
#         y = 4,
#         color = list(
#           pattern = list(
#             image = 'https://www.flaticon.com/svg/static/icons/svg/507/507257.svg',
#             aspectRatio = 1.6
#           )
#         )
#       ),
#       list(
#         name = 'Cuarto intermedio',
#         y = 3,
#         color = list(
#           pattern = list(
#             image = 'https://www.flaticon.es/svg/static/icons/svg/899/899054.svg',
#             aspectRatio = 1.3
#           )
#         )
#       )
#     )
#   ) %>% 
#   hc_chart(style = list(fontFamily = "Open Sans")) %>% 
#   hc_tooltip(backgroundColor = "white", borderWidth = 0.001) %>% 
#   hc_credits(
#     enabled = TRUE,
#     text = "(enero 2010 - junio 2020)",
#     style = list(fontFamily = "Open Sans", fontSize = 13)
#   ) -> salidas


# tipo de conflcitos y duración
df %>% 
  group_by(id, tipo) %>% 
  summarise(
    min = as.Date(min(fecha)),
    max = as.Date(max(fecha))
  ) %>% 
  mutate(
    diferencia = max - min,
    diferencia = diferencia + 1,
    diferencia = as.numeric(diferencia)
  ) %>% 
  ungroup() %>% 
  group_by(tipo) %>% 
  summarise(
    n = n(),
    median = median(diferencia),
    mean = mean(diferencia),
    max = max(diferencia),
    min = min(diferencia)
  ) %>% 
  mutate(
    mean = round(mean, 2),
    mean_1 = format(mean, nsmall = 2, big.mark=","),
    max_1 = format(max, nsmall = 0, big.mark=".")
  ) %>% 
  filter(tipo != "Otro") %>% 
  arrange(mean) %>% 
  mutate_if(is.numeric, round, 1) -> temp



highchart() %>% 
  hc_add_series(data = temp, "scatter", hcaes(x = tipo, y = mean), 
                color = "#870845", name = "Rt", linkedTo = "error") %>% 
  hc_chart(style = list(fontFamily = "Open Sans")) %>% 
  hc_plotOptions(
    scatter = list(
      marker = list(radius = 9, enabled = T, symbol = "circle"),
      states = list(hover = list(halo = list(size = 1)))
    )
  ) %>% 
  hc_xAxis(title = list(text = "Causas de los conflictos"), 
           categories = temp$tipo) %>% 
  hc_yAxis(title = list(text = "Días de duración del conflicto")) %>% 
  hc_tooltip(enabled = T, valueDecimals = 1, borderWidth = 0.01, backgroundColor = "white",
             pointFormat=paste("<b>{point.tipo}</b><br>
                               Promedio de duración de conflictos: <b>{point.mean_1} días</b><br>
                               Duración mínima: <b>{point.min}</b> día<br>
                               Duración máxima: <b>{point.max_1}</b> días<br>"), 
             headerFormat = "<b>{point.tipo}</b>", 
             style = list(fontFamily = "Open Sans")) %>% 
  hc_chart(inverted = TRUE, style = list(fontFamily = "Open Sans")) %>% 
  hc_size(height = 800) -> tipo_duracion


# TIEMPO Y DEMANDANTE 
df %>% 
  group_by(id, sector_b) %>% 
  summarise(
    min = as.Date(min(fecha)),
    max = as.Date(max(fecha))
  ) %>% 
  mutate(
    diferencia = max - min,
    diferencia = diferencia + 1,
    diferencia = as.numeric(diferencia)
  ) %>% 
  ungroup() %>% 
  group_by(sector_b) %>% 
  summarise(
    n = n(),
    median = median(diferencia),
    mean = mean(diferencia),
    max = max(diferencia),
    min = min(diferencia)
  ) %>% 
  mutate(
    mean = round(mean, 2),
    mean_1 = format(mean, nsmall = 2, big.mark=","),
    max_1 = format(max, nsmall = 0, big.mark=".")
  ) %>% 
  filter(sector_b != "Otro") %>% 
  arrange(mean) %>% 
  mutate_if(is.numeric, round, 1) -> temp


highchart() %>% 
  hc_add_series(data = temp, "scatter", hcaes(x = sector_b, y = mean), 
                color = "#E01F52", name = "Rt", linkedTo = "error") %>% 
  hc_chart(style = list(fontFamily = "Open Sans")) %>% 
  hc_plotOptions(
    scatter = list(
      marker = list(radius = 9, enabled = T, symbol = "circle"),
      states = list(hover = list(halo = list(size = 1)))
    )
  ) %>% 
  hc_xAxis(title = list(text = "Sector demandado"), 
           categories = temp$sector_b) %>% 
  hc_yAxis(title = list(text = "Días de duración del conflicto")) %>% 
  hc_tooltip(enabled = T, valueDecimals = 1, borderWidth = 0.01, backgroundColor = "white",
             pointFormat=paste("<b>{point.tipo}</b><br>
                               Promedio de duración de conflictos: <b>{point.mean_1} días</b><br>
                               Duración mínima: <b>{point.min}</b> día<br>
                               Duración máxima: <b>{point.max_1}</b> días<br>"), 
             headerFormat = "<b>{point.tipo}</b>", 
             style = list(fontFamily = "Open Sans")) %>% 
  hc_chart(inverted = TRUE, style = list(fontFamily = "Open Sans")) %>% 
  hc_size(height = 800) -> demandado_duracion



#------
# Drill Down Niveles vs años
#------

conflictos <- df %>% 
  mutate(year = lubridate::year(fecha))


temp1 <-  conflictos %>% filter(nivel > 0, year >= 2010) %>% select(id, nivel, year) %>% 
  # distinct() %>% 
  group_by(year, nivel) %>% 
  summarise(cantidad = n()) %>% 
  group_by(year) %>% 
  mutate(total = sum(cantidad)) %>%
  ungroup() %>%
  mutate(
    nivel = case_when(
      nivel == 1 ~ "Latente",
      nivel == 2 ~ "Manifiesto",
      nivel == 3 ~ "Confrontación",
      nivel == 4 ~ "Enfrentamiento violento",
      nivel == 5 ~ "Crisis de gobernabilidad",
    )
  ) %>% group_by(nivel) %>% 
  summarise(cantidad = sum(cantidad))


temp2 <- conflictos %>% filter(nivel > 0, year >= 2010) %>% select(id, nivel, year) %>% 
  # distinct() %>% 
  group_by(year, nivel) %>% 
  summarise(cantidad = n()) %>% 
  group_by(year) %>% 
  mutate(total = sum(cantidad)) %>%
  ungroup() %>%
  mutate(
    nivel = case_when(
      nivel == 1 ~ "Latente",
      nivel == 2 ~ "Manifiesto",
      nivel == 3 ~ "Confrontación",
      nivel == 4 ~ "Enfrentamiento violento",
      nivel == 5 ~ "Crisis de gobernabilidad",
    )
  ) %>% 
  group_nest(nivel) %>% 
  mutate(
    id = nivel,
    type = "column",
    data = map(data, mutate, name = year, y = cantidad),
    data = map(data, list_parse)
  )




x <- c("Cantidad")
y <- c("{point.cantidad}")

tt <- tooltip_table(x,y)

nivel_drill <- hchart(
  temp1,
  "column",
  hcaes(x = nivel, y = cantidad, name = nivel, drilldown = nivel),
  name = "Cantidad",
  colorByPoint = TRUE
) %>% 
  hc_drilldown(
    allowPointDrilldown = TRUE,
    series = list_parse(temp2)
  ) %>% 
  hc_tooltip(
    pointFormat = tt, # "{point.name} {point.pop}"
    useHTML = TRUE,
    valueDecimals = 0
  ) %>% 
  hc_yAxis(
    title = list(text = "Cantidad de eventos"),
    # type = "logarithmic",
    minorTickInterval = 'auto'
  ) %>% 
  hc_xAxis(
    title = ""
  ) %>% 
  hc_colors(c("#06D6A0", 
              "#C6A659",
              "#82769D",
              "#E01F52",
              "#073B4C"
  )) %>% 
  hc_chart(style = list(fontFamily = "Open Sans")) %>% 
  hc_credits(
    enabled = TRUE,
    text = "(enero 2010 - junio 2020)",
    style = list(fontFamily = "Open Sans", fontSize = 13)
  )


#------
# Barras demandante vs tipo
#------

conflictos <- df %>% 
  mutate(year = lubridate::year(fecha))



temp <- merge((conflictos %>% select(id, sector_a) %>% distinct()),
              (conflictos %>% select(id, tipo) %>% distinct())) %>% 
  group_by(sector_a, tipo) %>% 
  summarise(cantidad = n()) %>% 
  ungroup() %>% 
  filter(tipo != "Otro") %>% 
  mutate(porcentaje = prop.table(cantidad)*100) %>% 
  arrange(desc(cantidad)) %>% 
  group_by(sector_a) %>% 
  mutate(total = sum(cantidad)) %>% select(-porcentaje) %>% 
  ungroup() %>%
  mutate(tipo = paste0("Tipo: ",tipo)) %>% 
  spread(tipo, cantidad) %>% 
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



hbr_sector_tipo <- hbr_sn %>%
  hc_chart(type = "bar") %>%
  hc_plotOptions(series = list(stacking = "normal")) %>%
  hc_legend(reversed = TRUE) %>% 
  hc_colors(c("#E01F52", "#C6A659", "#06D6A0", "#466B77", "#073B4C",
              "#D8B970", "#5FA1B7", "#118AB2", "#BAAB89", "#1E4D5C", 
              "#2C6E49", "#E07A5F", "#3D405B", "#81B29A", "#63585F", 
              "#B4B5BA", "#261F23", "#575D7C"
  )) %>%
  hc_tooltip(enabled = T, valueDecimals = 0, borderWidth = 0.01,
             # crosshairs = TRUE, shared = TRUE,
             style = list(fontFamily = "Open Sans",
                          color = "black", fontSize = 14),
             headerFormat = "<b>{point.key}</b><br>") %>%
  # hc_add_theme(hc_theme_smpl(
  #   yAxis = list(
  #     labels = list(style = list(fontSize = "15px"), useHTML = TRUE)
  #   ),
  #   xAxis = list(
  #     labels = list(style = list(fontSize = "15px"), useHTML = TRUE)
  #   )
  # )) %>%
  hc_chart(backgroundColor="#FFFFFF", style = list(fontFamily = "Open Sans",
                                                   color = "black")) %>% 
  hc_title(text = "Tipos de conflicto en sectores demandantes") %>% 
  hc_size(height = 1200) %>% 
  hc_credits(
    enabled = TRUE,
    text = "(enero 2010 - junio 2020)",
    style = list(fontFamily = "Open Sans", fontSize = 13)
  )

#---------
#  packed bubble tipos vs medidas (alternativa)
#--------

conflictos <- df %>%
  mutate(year = lubridate::year(fecha))

temp <- conflictos %>% mutate(gestion = lubridate::year(fecha)) %>% 
  filter(gestion >= 2010) %>% 
  select(id, tipo, medida_de_presion) %>% 
  group_by(tipo, medida_de_presion) %>% 
  summarise(frecuencia = n()) %>% 
  filter(tipo != "Otro") %>% 
  spread(medida_de_presion, frecuencia) %>% 
  mutate_if(is.numeric, replace_na, 0) %>% 
  gather(medida_de_presion, frecuencia, -tipo)

unique(temp$tipo)

hchart(conflictos %>% mutate(gestion = lubridate::year(fecha)) %>% 
         filter(gestion >= 2010) %>% 
         select(id, tipo, medida_de_presion) %>% 
         group_by(tipo, medida_de_presion) %>% 
         summarise(frecuencia = n()) %>% 
         filter(tipo != "Otro") %>% 
         spread(medida_de_presion, frecuencia) %>% 
         mutate_if(is.numeric, replace_na, 0) %>% 
         gather(medida_de_presion, frecuencia, -tipo),
       "packedbubble",
       hcaes(name = medida_de_presion, value = frecuencia, group = tipo)) %>% 
  hc_colors(c("#E01F52", "#C6A659", "#06D6A0", "#466B77", "#073B4C", 
              "#D8B970", "#5FA1B7", "#118AB2", "#BAAB89", "#1E4D5C",
              "#2C6E49", "#E07A5F", "#3D405B", "#81B29A", "#63585F",
              "#B4B5BA")) %>%
  hc_plotOptions(packedbubble = list(
    minSize = 5,
    maxSize = 100,
    zMin = 0,
    zMax = 5000,
    layoutAlgorithm = list(
      gravitationalConstant = 0.01,
      splitSeries = T,
      seriesInteraction = F,
      dragBetweenSeries = T,
      parentNodeLimit = T
    )
  )
  ) %>% 
  hc_tooltip(enabled = T, valueDecimals = 2, borderWidth = 0.01, 
             style = list(fontFamily = "Open Sans"), backgroundColor =  "white",
             pointFormat=paste("Tipo demanda: <b>{point.tipo}</b><br>
                               Medida de presión: <b>{point.medida_de_presion}</b><br>
                               Total eventos: <b>{point.frecuencia}</b>"),
             headerFormat = "") %>% 
  hc_chart(style = list(fontFamily = "Open Sans")) %>% 
  hc_credits(
    enabled = TRUE,
    text = "(enero 2010 - junio 2020)",
    style = list(fontFamily = "Open Sans", fontSize = 13)
  ) -> hcbub_tipo_medida


#-----------
#  Tree map Tipo vs medidas (alternativa)
#-----------

conflictos <- df %>%
  mutate(year = lubridate::year(fecha))

conflictos %>% 
  count(sector_b, year) %>% 
  mutate(
    porcentaje = prop.table(n)*100,
    porcentaje = round(porcentaje, 3)
  ) %>% 
  mutate_if(is.character, replace_na, "Sin clasficación") %>% 
  rename(value = n) %>% 
  arrange(sector_b, desc(value))-> temp


temp <- conflictos %>% mutate(gestion = lubridate::year(fecha)) %>% 
  filter(gestion >= 2010) %>% 
  select(id, tipo, medida_de_presion) %>% 
  group_by(tipo, medida_de_presion) %>% 
  summarise(frecuencia = n()) %>% 
  filter(tipo != "Otro") %>% 
  spread(medida_de_presion, frecuencia) %>% 
  mutate_if(is.numeric, replace_na, 0) %>% 
  gather(medida_de_presion, frecuencia, -tipo)

colores <- temp %>% select(tipo) %>% unique

col <- c("#2C6E49", "#E07A5F", "#3D405B", "#81B29A", "#63585F",
         "#E01F52", "#C6A659", "#06D6A0", "#466B77", "#073B4C", 
         "#D8B970", "#5FA1B7", "#118AB2", "#BAAB89", "#1E4D5C", 
         "#B4B5BA")

col <- col[1:nrow(colores)]


colores <- as.data.frame(colores) %>% 
  mutate(color = col)

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
      style = list(fontSize = "13px", textOutline = FALSE, color = "black")
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
  data_to_hierarchical(temp, c(tipo, medida_de_presion), frecuencia, colors = cols),
  type = "treemap",
  levelIsConstant = T,
  allowDrillToNode = T,
  drillUpButton = list(
    text = "< Volver"
  ),
  levels = lvl_opts,
  tooltip = list(valueDecimals = 2)
) %>% 
  hc_chart(
    style = list(fontFamily = "Oswald")
  ) %>% 
  hc_tooltip(backgroundColor = "white", borderWidth = 0.001) %>% 
  hc_size(height = 700) %>% 
  hc_credits(
    enabled = TRUE,
    text = "(enero 2010 - junio 2020)",
    style = list(fontFamily = "Oswald", fontSize = 13)
  ) -> tree_map_tipo_medida
