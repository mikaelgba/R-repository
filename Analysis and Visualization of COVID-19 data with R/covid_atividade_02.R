library(ggplot2)
library(ggthemes)
library(here)
library(lubridate)
library(tidyverse)

url_estados <- "https://raw.githubusercontent.com/marcuswac/covid-br-data/master/covid-br-ms-states.csv"
covid_reg <- read_csv(url_estados, col_types = cols(data = col_date()))

glimpse(covid_reg)

covid_regiao <- covid_reg %>%
  group_by(regiao, data) %>% summarise(casosNovos = sum(casosNovos),
                                       casosAcumulado = sum(casosAcumulado),
                                       obitosNovos = sum(obitosNovos),
                                       obitosAcumulado = sum(obitosAcumulado)) %>% 
  filter(data >= lubridate::dmy("25-02-2020"))

glimpse(covid_reg)
ultimo_dia <- max(covid_regiao$data)

plot_obitosNovos_regiao <- ggplot(covid_regiao, aes(data, obitosNovos)) + 
  geom_area(aes(fill = regiao)) + 
  scale_x_date(expand = c(0, 0)) +
  coord_cartesian(xlim = c(lubridate::dmy("25-02-2020"), ultimo_dia)) +
  scale_fill_brewer(name = "Região", palette = "Set3") + 
  labs(x = "Notificados", y = "Obitos") + 
  ggtitle("Novos Obitos por Região")

p1 <- plot_obitosNovos_regiao + theme_dark() + theme(legend.title = element_blank(),
                                legend.position = c(0.24, 0.68),
                                legend.direction = "vertical")

plot_obitosAcumulados_regiao <- ggplot(covid_regiao, aes(data, obitosAcumulado)) + 
  geom_area(aes(fill = regiao)) + 
  scale_x_date(expand = c(0, 0)) +
  coord_cartesian(xlim = c(lubridate::dmy("25-02-2020"),ultimo_dia)) +
  scale_fill_brewer(name = "Região", palette = "Set3") + 
  labs(x = "Notificados", y = "Obitos") + 
  ggtitle("Total de Obitos por Região")

p2 <- plot_obitosAcumulados_regiao + theme_dark() + theme(legend.title = element_blank(),
                                     legend.position = c(0.24, 0.68),
                                     legend.direction = "vertical")

plot_casosAcumulados_regiao <- ggplot(covid_regiao, aes(data, casosAcumulado)) + 
  geom_area(aes(fill = regiao)) + 
  scale_x_date(expand = c(0, 0)) +
  coord_cartesian(xlim = c(lubridate::dmy("25-02-2020"),ultimo_dia)) +
  scale_fill_brewer(name = "Região", palette = "Set3") + 
  labs(x = "Notificados", y = "Casos") + 
  ggtitle("Total de Casos por Região")

p3 <- plot_casosAcumulados_regiao + theme_dark() + theme(legend.title = element_blank(),
                                                    legend.position = c(0.24, 0.68),
                                                    legend.direction = "vertical")

url_br <- "https://raw.githubusercontent.com/marcuswac/covid-br-data/master/covid-br-ms-country.csv"
covid_br <- read_csv(url_br, col_types = cols(data = col_date()))

covid_brasil <- covid_br %>%
  group_by(regiao, data) %>%
  summarise(casosAcumulado = sum(casosAcumulado),
            obitosAcumulado = sum(obitosAcumulado)) %>%
  filter(data >= lubridate::dmy("25-02-2020"))

plot_obitos_Acumulados_brasil <- ggplot(covid_brasil, aes(data, obitosAcumulado)) + 
  geom_area(aes(fill = regiao)) + 
  scale_x_date(expand = c(0, 0)) +
  coord_cartesian(xlim = c(lubridate::dmy("25-02-2020"), ultimo_dia)) +
  scale_fill_brewer(name = "Região", palette = "Set3") + 
  labs(x = "Notificados", y = "Obitos") + 
  ggtitle("Novos Obitos no Brasil")

p4 <- plot_obitos_Acumulados_brasil + theme_dark() + theme(legend.title = element_blank(),
                                                     legend.position = c(0.24, 0.68),
                                                     legend.direction = "vertical")

plot_casos_Acumulados_brasil <- ggplot(covid_brasil, aes(data, casosAcumulado)) + 
  geom_area(aes(fill = regiao)) + 
  scale_x_date(expand = c(0, 0)) +
  coord_cartesian(xlim = c(lubridate::dmy("25-02-2020"),ultimo_dia)) +
  scale_fill_brewer(name = "Região", palette = "Set3") + 
  labs(x = "Notificados", y = "Obitos") + 
  ggtitle("Total de casos no Brasil")

p5 <- plot_casos_Acumulados_brasil + theme_dark() + theme(legend.title = element_blank(),
                                                          legend.position = c(0.24, 0.68),
                                                          legend.direction = "vertical")

glimpse(covid_regiao)
glimpse(covid_brasil)

ggsave("plot_Obitos_novos_por_regiao.png", p1)
ggsave("plot_Obitos_acumulados_por_regiao.png", p2)
ggsave("plot_casos_acumulados_por_regiao.png", p3)
ggsave("plot_Obitos_acumulados_por_brasil.png", p4)
ggsave("plot_casos_acumulados_por_brasil.png", p5)