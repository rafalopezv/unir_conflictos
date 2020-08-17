library(highcharter)
library(tidyverse)
library(magrittr)
library(lubridate)

source("codigo/funciones.R")
Sys.setlocale(locale = "es_ES.UTF-8")

df <- rio::import("input/base_limpia.xlsx")

colores <- c("#264653","#2a9d8f","#e9c46a","#f4a261","#e76f51", "#e63946", "#d90429", "#50514f", "#293241")
colores_1 <- c("#222438","#262842","#3D405B","#CCF0F9","#870845", "#753855", "#E07A5F", "#F2CC8F", "#5BBF8F", "#BBEAD3")
colores_2  <- c("#262842", "#5BBF8F", "#BBEAD3", "#870845", "#222438", "#E07A5F", "#3D405B", "#F2CC8F", "#CCF0F9", "#753855")

# razones conflicto
df %>% 
  select(id, tipo) %>% 
  unique() %>% 
  count(tipo) %>% 
  mutate(
    prop = prop.table(n)*100,
    prop = round(prop, 2)
  ) %>% 
  arrange(desc(n)) %>% 
  hchart(
    "item", 
    hcaes(name = tipo, y = n),
    marker = list(symbol = "square"),
    showInLegend = TRUE
  ) %>% 
  hc_colors(colors = colores_2) %>% 
  hc_chart(style = list(fontFamily = "IBM Plex Mono")) %>% 
  hc_tooltip(enabled = T, valueDecimals = 2, borderWidth = 0.01, 
             style = list(fontFamily = "IBM Plex Mono"), backgroundColor =  "white",
             pointFormat=paste("<b>{point.tipo}</b><br>
                               <b>{point.n}</b> conflictos<br>
                               <b>{point.prop} %</b> sobre el total<br>"),
             headerFormat = "") %>% 
    hc_credits(
      enabled = TRUE,
      text = "cada cuadrado representa a un conflicto"
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
  geom_text(aes(x = 1, y = 5.5, label = dia), family = "Source Code Pro Black",
            size = 17, color = "#222438") +
  facet_wrap(~etiqueta) + 
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA),
    strip.background = element_rect(fill = "lightgray", colour = NA),
    strip.text =  element_text(color = "#222438", family = "Source Code Pro Light", size = 18),
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
           style=list(fontFamily = "Source Code Pro", fontSize = 12)) %>% 
  hc_tooltip(enabled = T, valueDecimals = 2, borderWidth = 0.01, 
             style = list(fontFamily = "IBM Plex Mono"),
             pointFormat=paste("<b>{point.etiqueta}</b><br>
                               <b>{point.n}</b> conflictos<br>"),
             headerFormat = "") %>% 
  hc_size(height = 800) -> años_juntos


# graficar
fechas %>% 
  filter(año == 2010) %>% 
  mutate(valor = case_when(
    n > 0 ~ "Día con conflicto",
    T ~"Día sin conflicto"
  )) -> temp

temp %>% filter(valor == "Día sin conflicto") %>% nrow()

a <- temp %>% pull(fecha)
b <- temp %>% pull(valor)

calendario_heatmap(a, b) + 
  theme(
    legend.position = "top",
    legend.title = element_blank()
  ) + 
  scale_fill_manual(values = rev(c("#F2CC8F", "#3D405B"))) + 
  labs(
    subtitle = "2010: 43 días sin conflictos"
  ) +
  theme(
    plot.subtitle =  element_text(vjust = -1, hjust = 0.5, size = 20),
    strip.text =  element_blank(), 
    axis.text.x = element_text(size = 19),
    axis.text.y = element_text(size = 19),
    legend.text = element_text(size = 20),
    legend.position = c(0.5, 1.4)
  ) -> cal_2010
  

fechas %>% 
  filter(año == 2011) %>% 
  mutate(valor = case_when(
    n > 0 ~ "Día con conflicto",
    T ~"Día sin conflicto"
  )) -> temp

c <- temp %>% filter(valor == "Día sin conflicto") %>% nrow()

a <- temp %>% pull(fecha)
b <- temp %>% pull(valor)

calendario_heatmap(a, b) + 
  theme(
    legend.position = "top",
    legend.title = element_blank()
  ) + 
  scale_fill_manual(values = rev(c("#F2CC8F", "#3D405B"))) + 
  labs(
    subtitle = paste0("2011: ", c, " días sin conflictos")
  ) +
  theme(
    plot.subtitle =  element_text(vjust = -1, hjust = 0.5, size = 20),
    strip.text =  element_blank(), 
    axis.text.x = element_text(size = 19),
    axis.text.y = element_text(size = 19),
    legend.text = element_text(size = 20),
    legend.position = "none"
  ) -> cal_2011

fechas %>% 
  filter(año == 2012) %>% 
  mutate(valor = case_when(
    n > 0 ~ "Día con conflicto",
    T ~"Día sin conflicto"
  )) -> temp

c <- temp %>% filter(valor == "Día sin conflicto") %>% nrow()

a <- temp %>% pull(fecha)
b <- temp %>% pull(valor)

calendario_heatmap(a, b) + 
  theme(
    legend.position = "top",
    legend.title = element_blank()
  ) + 
  scale_fill_manual(values = rev(c("#F2CC8F", "#3D405B"))) + 
  labs(
    subtitle = paste0("2012: ", c, " días sin conflictos")
  ) +
  theme(
    plot.subtitle =  element_text(vjust = -1, hjust = 0.5, size = 20),
    strip.text =  element_blank(), 
    axis.text.x = element_text(size = 19),
    axis.text.y = element_text(size = 19),
    legend.text = element_text(size = 20),
    legend.position = "none"
  ) -> cal_2012


fechas %>% 
  filter(año == 2013) %>% 
  mutate(valor = case_when(
    n > 0 ~ "Día con conflicto",
    T ~"Día sin conflicto"
  )) -> temp

c <- temp %>% filter(valor == "Día sin conflicto") %>% nrow()

a <- temp %>% pull(fecha)
b <- temp %>% pull(valor)

calendario_heatmap(a, b) + 
  theme(
    legend.position = "top",
    legend.title = element_blank()
  ) + 
  scale_fill_manual(values = rev(c("#F2CC8F", "#3D405B"))) + 
  labs(
    subtitle = paste0("2013: ", c, " días sin conflictos")
  ) +
  theme(
    plot.subtitle =  element_text(vjust = -1, hjust = 0.5, size = 20),
    strip.text =  element_blank(), 
    axis.text.x = element_text(size = 19),
    axis.text.y = element_text(size = 19),
    legend.text = element_text(size = 20),
    legend.position = "none"
  ) -> cal_2013

fechas %>% 
  filter(año == 2014) %>% 
  mutate(valor = case_when(
    n > 0 ~ "Día con conflicto",
    T ~"Día sin conflicto"
  )) -> temp

c <- temp %>% filter(valor == "Día sin conflicto") %>% nrow()

a <- temp %>% pull(fecha)
b <- temp %>% pull(valor)

calendario_heatmap(a, b) + 
  theme(
    legend.position = "top",
    legend.title = element_blank()
  ) + 
  scale_fill_manual(values = rev(c("#F2CC8F", "#3D405B"))) + 
  labs(
    subtitle = paste0("2014: ", c, " días sin conflictos")
  ) +
  theme(
    plot.subtitle =  element_text(vjust = -1, hjust = 0.5, size = 20),
    strip.text =  element_blank(), 
    axis.text.x = element_text(size = 19),
    axis.text.y = element_text(size = 19),
    legend.text = element_text(size = 20),
    legend.position = "none"
  ) -> cal_2014


fechas %>% 
  filter(año == 2015) %>% 
  mutate(valor = case_when(
    n > 0 ~ "Día con conflicto",
    T ~"Día sin conflicto"
  )) -> temp

c <- temp %>% filter(valor == "Día sin conflicto") %>% nrow()

a <- temp %>% pull(fecha)
b <- temp %>% pull(valor)

calendario_heatmap(a, b) + 
  theme(
    legend.position = "top",
    legend.title = element_blank()
  ) + 
  scale_fill_manual(values = rev(c("#F2CC8F", "#3D405B"))) + 
  labs(
    subtitle = paste0("2015: ", c, " días sin conflictos")
  ) +
  theme(
    plot.subtitle =  element_text(vjust = -1, hjust = 0.5, size = 20),
    strip.text =  element_blank(), 
    axis.text.x = element_text(size = 19),
    axis.text.y = element_text(size = 19),
    legend.text = element_text(size = 20),
    legend.position = "none"
  ) -> cal_2015


fechas %>% 
  filter(año == 2016) %>% 
  mutate(valor = case_when(
    n > 0 ~ "Día con conflicto",
    T ~"Día sin conflicto"
  )) -> temp

c <- temp %>% filter(valor == "Día sin conflicto") %>% nrow()

a <- temp %>% pull(fecha)
b <- temp %>% pull(valor)

calendario_heatmap(a, b) + 
  theme(
    legend.position = "top",
    legend.title = element_blank()
  ) + 
  scale_fill_manual(values = rev(c("#F2CC8F", "#3D405B"))) + 
  labs(
    subtitle = paste0("2016: ", c, " días sin conflictos")
  ) +
  theme(
    plot.subtitle =  element_text(vjust = -1, hjust = 0.5, size = 20),
    strip.text =  element_blank(), 
    axis.text.x = element_text(size = 19),
    axis.text.y = element_text(size = 19),
    legend.text = element_text(size = 20),
    legend.position = "none"
  ) -> cal_2016


fechas %>% 
  filter(año == 2017) %>% 
  mutate(valor = case_when(
    n > 0 ~ "Día con conflicto",
    T ~"Día sin conflicto"
  )) -> temp

c <- temp %>% filter(valor == "Día sin conflicto") %>% nrow()

a <- temp %>% pull(fecha)
b <- temp %>% pull(valor)

calendario_heatmap(a, b) + 
  theme(
    legend.position = "top",
    legend.title = element_blank()
  ) + 
  scale_fill_manual(values = rev(c("#F2CC8F", "#3D405B"))) + 
  labs(
    subtitle = paste0("2017: ", c, " días sin conflictos")
  ) +
  theme(
    plot.subtitle =  element_text(vjust = -1, hjust = 0.5, size = 20),
    strip.text =  element_blank(), 
    axis.text.x = element_text(size = 19),
    axis.text.y = element_text(size = 19),
    legend.text = element_text(size = 20),
    legend.position = "none"
  ) -> cal_2017

fechas %>% 
  filter(año == 2018) %>% 
  mutate(valor = case_when(
    n > 0 ~ "Día con conflicto",
    T ~"Día sin conflicto"
  )) -> temp

c <- temp %>% filter(valor == "Día sin conflicto") %>% nrow()

a <- temp %>% pull(fecha)
b <- temp %>% pull(valor)

calendario_heatmap(a, b) + 
  theme(
    legend.position = "top",
    legend.title = element_blank()
  ) + 
  scale_fill_manual(values = rev(c("#F2CC8F", "#3D405B"))) + 
  labs(
    subtitle = paste0("2018: ", c, " días sin conflictos")
  ) +
  theme(
    plot.subtitle =  element_text(vjust = -1, hjust = 0.5, size = 20),
    strip.text =  element_blank(), 
    axis.text.x = element_text(size = 19),
    axis.text.y = element_text(size = 19),
    legend.text = element_text(size = 20),
    legend.position = "none"
  ) -> cal_2018


fechas %>% 
  filter(año == 2019) %>% 
  mutate(valor = case_when(
    n > 0 ~ "Día con conflicto",
    T ~"Día sin conflicto"
  )) -> temp

c <- temp %>% filter(valor == "Día sin conflicto") %>% nrow()

a <- temp %>% pull(fecha)
b <- temp %>% pull(valor)

calendario_heatmap(a, b) + 
  theme(
    legend.position = "top",
    legend.title = element_blank()
  ) + 
  scale_fill_manual(values = rev(c("#F2CC8F", "#3D405B"))) + 
  labs(
    subtitle = paste0("2019: ", c, " días sin conflictos")
  ) +
  theme(
    plot.subtitle =  element_text(vjust = -1, hjust = 0.5, size = 20),
    strip.text =  element_blank(), 
    axis.text.x = element_text(size = 19),
    axis.text.y = element_text(size = 19),
    legend.text = element_text(size = 20),
    legend.position = "none",
    plot.margin = margin(0, 0, 0, 0, "cm")
  ) -> cal_2019

fechas %>% 
  filter(año == 2020) %>% 
  mutate(valor = case_when(
    n > 0 ~ "Día con conflicto",
    T ~"Día sin conflicto"
  )) -> temp

c <- temp %>% filter(valor == "Día sin conflicto") %>% nrow()

a <- temp %>% pull(fecha)
b <- temp %>% pull(valor)

calendario_heatmap(a, b) + 
  theme(
    legend.position = "top",
    legend.title = element_blank()
  ) + 
  scale_fill_manual(values = rev(c("#F2CC8F", "#3D405B"))) + 
  labs(
    subtitle = paste0("2020: ", c, " días sin conflictos")
  ) +
  theme(
    plot.subtitle =  element_text(vjust = -1, hjust = 0.5, size = 20),
    strip.text =  element_blank(), 
    axis.text.x = element_text(size = 19),
    axis.text.y = element_text(size = 19),
    legend.text = element_text(size = 20),
    legend.position = "none",
    plot.margin = margin(0, 0, 0, 0, "cm")
  ) -> cal_2020

# ¿quien se moviliza?
df %>% 
  count(sector_a) %>% 
  mutate(
    porcentaje = prop.table(n)*100,
    porcentaje = round(porcentaje, 3)
  ) %>% 
  rename(value = n) -> temp

hctreemap2(data = temp,
           group_vars = "sector_a",
           size_var = "value",
           color_var = "porcentaje",
           layoutAlgorithm = "squarified",
           levelIsConstant = FALSE
) %>% 
  hc_colorAxis(stops = color_stops(colors = rev(c("#073B4C", "#E01F52")))) %>% 
  hc_plotOptions(series = list(
    dataLabels = list(
      style = list(textOutline = FALSE,
                   fontSize = 12)
    )
  )) %>%  
  hc_legend(enabled = F)  %>% 
  hc_colors("transparent")  %>% 
  hc_tooltip(pointFormat = "<br>Número de conflictos: <b>{point.value}</b><br>
                             Porcentaje: <b>{point.colorValue}%</b>", 
             style = list(fontFamily = "Source Code Pro", fontSize = 12)) %>% 
  hc_chart(backgroundColor="#FFFFFF", borderColor = "transparent", 
           style=list(fontFamily = "Source Code Pro", fontSize = 12))  -> quienes

# quienes son los más demandados
df %>% 
  count(sector_b) %>% 
  mutate(
    porcentaje = prop.table(n)*100,
    porcentaje = round(porcentaje, 3)
  ) %>% 
  rename(value = n) -> temp

hctreemap2(data = temp,
           group_vars = "sector_b",
           size_var = "value",
           color_var = "porcentaje",
           layoutAlgorithm = "squarified",
           levelIsConstant = FALSE
) %>% 
  hc_colorAxis(stops = color_stops(colors = rev(c("#073B4C", "#D8B970")))) %>% 
  hc_plotOptions(series = list(
    dataLabels = list(
      style = list(textOutline = FALSE,
                   fontSize = 12)
    )
  )) %>%  
  hc_legend(enabled = F)  %>% 
  hc_colors("transparent")  %>% 
  hc_tooltip(pointFormat = "<br>Número de conflictos: <b>{point.value}</b><br>
                             Porcentaje: <b>{point.colorValue}%</b>", 
             style = list(fontFamily = "Source Code Pro", fontSize = 12)) %>% 
  hc_chart(backgroundColor="#FFFFFF", borderColor = "transparent", 
           style=list(fontFamily = "Source Code Pro", fontSize = 12))  -> a_quienes


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
    style = list(fontFamily = "Source Code Pro")
  ) %>% 
  hc_size(height = 700)  -> tree_map_1


hchart(
  data_to_hierarchical(temp, c(sector_a, sub_sector_a), porcentaje, colors = cols),
  type = "treemap",
  #levelIsConstant = T,
  #allowDrillToNode = T,
  levels = lvl_opts,
  tooltip = list(valueDecimals = FALSE)
) %>% 
  hc_chart(
    style = list(fontFamily = "Source Code Pro")
  ) %>% 
  hc_size(height = 700)  -> tree_map_2

#-----------------------------------
# duración conflictos
#-----------------------------------
df %>% group_by(id) %>% 
  summarise(fecha_inicio = as.Date(min(fecha)),
            fecha_final = as.Date(max(fecha)),
            duracion_dias = as.numeric(difftime(fecha_final, fecha_inicio, units = "days"))) %>% 
  mutate(
    duracion_dias = case_when(
      duracion_dias == 0 ~ 1,
      T ~ duracion_dias
    )
  ) -> temp



