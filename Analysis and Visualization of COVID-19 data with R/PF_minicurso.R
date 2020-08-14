library(tidyverse)
library(here)
library(lubridate)
library(scales)
library(forcats)
library(geobr)
library(readr)
library(sf)
library(plotly)


theme_plot = function(plot_entrada){
  
  plot_entrada + theme_dark() + theme(legend.title = element_blank(),
                                      legend.direction = "vertical")
}

dados_br <- read.csv(here("data","caso_full.csv")) %>%
  filter(place_type == "state") %>%
  mutate(mes = month(date),
         dia_da_semana = format.Date(as.Date(date), "%a"))

glimpse(dados_br)

dados_br_semana <- dados_br %>%
  group_by(epidemiological_week)%>%
  summarise(casos_acumulado = max(new_confirmed, na.rm = TRUE),
            casos_novos_media = mean(new_confirmed, na.rm = TRUE),
            casos_novos_total = sum(new_confirmed, na.rm = TRUE),
            obitos_acumulado = max(new_deaths, na.rm = TRUE),
            obitos_novos_media = mean(new_deaths, na.rm = TRUE),
            obitos_novos_total = sum(new_deaths, na.rm = TRUE),
            dias = n(),
            regiao = "Brasil")

glimpse(dados_br_semana)

dados_br_semana <- dados_br_semana %>%
  ungroup() %>%
  mutate(pico_casos_novos = casos_novos_media == max(casos_novos_media),
         pico_obitos_novos = obitos_novos_media == max(obitos_novos_media))

glimpse(dados_br_semana)

dados_br_semana %>%
  filter(pico_casos_novos) %>%
  select(regiao,
         semana_pico_casos = epidemiological_week, 
         obitos_novos_total)

plot_um <- ggplot(dados_br_semana, aes(epidemiological_week, casos_novos_total,
                                       fill = casos_novos_total,
                                       text = paste0(regiao, ": ", casos_novos_total, " casos"))) +
  geom_col() +
  scale_fill_viridis_c(option = "V") +
  labs(x = "Semanas epidemiológicas",
       y = "Média de casos novos semanais")

ggplotly(plot_um, tooltip = c("text"))


dados_br_semana %>%
  filter(pico_obitos_novos) %>%
  arrange(epidemiological_week) %>%
  select(regiao,
         semana_pico_obitos = epidemiological_week, 
         obitos_novos_media, 
         obitos_novos_total, 
         obitos_acumulado)

plot_dois <- ggplot(dados_br_semana, aes(epidemiological_week, obitos_novos_total,
                                         fill = obitos_novos_total,
                                         text = paste0(regiao, ": ", obitos_novos_total, " obitos"))) +
  geom_col() +
  scale_fill_viridis_c(option = "V") +
  labs(x = "Semanas epidemiológicas",
       y = "Média de obitos por semana")

ggplotly(theme_plot(plot_dois), tooltip = c("text"))