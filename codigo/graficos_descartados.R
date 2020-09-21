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

