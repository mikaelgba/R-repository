library(ggplot2)
library(ggthemes)
library(here)
library(lubridate)
library(tidyverse)

url_br <- "https://raw.githubusercontent.com/marcuswac/covid-br-data/master/covid-br-ms-country.csv"
covid_br <- read_csv(url_br, col_types = cols(data = col_date()))

covid_regiao <- covid_estados %>%
  
  group_by(regiao, data) %>% summarise(obitosNovos = sum(obitosNovos),
                                       obitosAcumulado = sum(obitosAcumulado)) %>% 
  filter(data >= lubridate::dmy("25-02-2020"))

glimpse(covid_regiao)
ultimo_dia <- max(covid_regiao$data)

plot_obitosNovos_regiao <- ggplot(covid_regiao, aes(data, obitosNovos)) + 
  geom_area(aes(fill = regiao)) + 
  scale_x_date(expand = c(0, 0)) +
  coord_cartesian(xlim = c(lubridate::dmy("25-02-2020"), ultimo_dia)) +
  scale_fill_brewer(name = "Região", palette = "Set3") + 
  labs(x = "Notificados", y = "Obitos") + 
  ggtitle("Novos Obitos por Região")

plot_obitosNovos_regiao + theme_dark() + theme(legend.title = element_blank(),
                                legend.position = c(0.24, 0.68),
                                legend.direction = "vertical")

plot_obitosAcumulados_regiao <- ggplot(covid_regiao, aes(data, obitosAcumulado)) + 
  geom_area(aes(fill = regiao)) + 
  scale_x_date(expand = c(0, 0)) +
  coord_cartesian(xlim = c(lubridate::dmy("25-02-2020"),ultimo_dia)) +
  scale_fill_brewer(name = "Região", palette = "Set3") + 
  labs(x = "Notificados", y = "Obitos") + 
  ggtitle("Total de Obitos por Região")

plot_obitosAcumulados_regiao + theme_dark() + theme(legend.title = element_blank(),
                                     legend.position = c(0.24, 0.68),
                                     legend.direction = "vertical")

ggsave("plot_Obitos_novos_por_regiao.png", plot_obitosNovos_regiao)
ggsave("plot_Obitos_acumulados_por_regiao.png", plot_obitosAcumulados_regiao)