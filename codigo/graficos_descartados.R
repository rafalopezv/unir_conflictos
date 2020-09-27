# sobre: graficos descartados 

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
             fontFamily = "Oswald",
             borderWidth = 0.8) %>% 
  hc_chart(style = list(fontFamily = "Oswald"))



hcmap <- hc_size(hcmap, 800, 800)

