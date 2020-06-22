# Windows UTF-8
library(ggthemes)
library(here)
library(lubridate)
library(tidyverse)

url_br <- "https://raw.githubusercontent.com/marcuswac/covid-br-data/master/covid-br-ms-country.csv"
covid_br <- read_csv(url_br, col_types = cols(data = col_date()))

covid_br <- covid_br %>%
  
  mutate(dia_da_semana = lubridate::wday(data, label = TRUE)) %>%
  filter(casosAcumulado >= 10000)

glimpse(covid_br)

ggplot(covid_br, aes(x = data, y = casosNovos)) + geom_col()

ggplot(covid_br, aes(x = data, y = casosNovos)) + geom_col() + 
  labs(x = "Dia da notificação", y = "Casos novos") +
  ggtitle("Casos novos por dia no Brasil")

ggplot(covid_br, aes(x = data, y = casosNovos)) + 
  geom_col(colour = "white") +
  labs(x = "Dia da notificação", y = "Casos novos") +
  ggtitle("Casos novos por dia no Brasil")

ggplot(covid_br, aes(x = data, y = casosNovos)) +
  geom_col(colour = "white", fill = "navyblue") +
  labs(x = "Dia da notificação", y = "Casos novos") +
  ggtitle("Casos novos por dia no Brasil")

ggplot(covid_br, aes(x = data, y = casosNovos)) +
  geom_col(aes(fill = dia_da_semana), colour = "white") +
  labs(x = "Dia da notificação", y = "Casos novos") +
  ggtitle("Casos novos por dia no Brasil")

ggplot(covid_br, aes(x = data, y = casosNovos)) +
  geom_col(aes(fill = dia_da_semana), colour = "white") +
  labs(x = "Dia da notificação", y = "Casos novos") +
  ggtitle("Casos novos por dia no Brasil") +
  theme(legend.title = element_blank(), legend.position = c(0.25, 0.9),
        legend.direction = "horizontal")

url_estados <- "https://raw.githubusercontent.com/marcuswac/covid-br-data/master/covid-br-ms-states.csv"
covid_estados <- read_csv(url_estados, col_types = cols(data = col_date()))

glimpse(covid_estados)

covid_regiao <- covid_estados %>%
  
  group_by(regiao, data) %>%
  
  summarise(casosNovos = sum(casosNovos),
            casosAcumulado = sum(casosAcumulado),
            obitosNovos = sum(obitosNovos),
            obitosAcumulado = sum(obitosAcumulado),
            populacao = sum(populacaoTCU2019)) %>%
  
  filter(data >= lubridate::dmy("01-04-2020")) # dmy:day-month-year

glimpse(covid_regiao)

ultimo_dia <- max(covid_regiao$data)

covid_regiao_ultimo_dia <- covid_regiao %>%
  ungroup() %>%
  filter(data == ultimo_dia)

covid_regiao_ultimo_dia %>%
  filter(casosAcumulado == max(casosAcumulado) |
           casosAcumulado == min(casosAcumulado)) %>%
  select(data, regiao, casosAcumulado, populacao)

covid_regiao_ultimo_dia %>%
  ungroup() %>%
  mutate(casosAcumuladoPor100k = (casosAcumulado*100000)/(populacao)) %>%
  filter(casosAcumuladoPor100k == max(casosAcumuladoPor100k) |
           casosAcumuladoPor100k == min(casosAcumuladoPor100k)) %>%
  select(data, regiao, casosAcumuladoPor100k)

ggplot(covid_regiao, aes(data, casosAcumulado)) +
  geom_area(aes(fill = regiao)) +
  labs(x = "Dia da notificação", y = "Casos novos") +
  ggtitle("Casos acumulados no Brasil")

plot_regiao <- ggplot(covid_regiao, aes(data, casosAcumulado)) +
  geom_area(aes(fill = regiao)) +
  scale_fill_brewer(name = "Região", palette = "Set1") +
  labs(x = "Dia da notificação", y = "Casos novos") +
  ggtitle("Casos acumulados no Brasil")

plot_regiao

plot_regiao <- plot_regiao +
  scale_x_date(expand = c(0, 0))

plot_regiao

plot_regiao <- plot_regiao +
  coord_cartesian(xlim = c(lubridate::dmy("01-05-2020"), ultimo_dia))

plot_regiao

plot_regiao + theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text = element_text(size = 12)) +
  theme(panel.background = element_blank()) +
  theme(axis.line.x = element_line(color = "gray80", size = 0.5)) +
  theme(axis.ticks = element_line(color = "gray80", size = 0.5))

my_theme <- theme(plot.title = element_text(hjust = 0.5),
                  axis.text = element_text(size = 12),
                  panel.background = element_blank(),
                  axis.line.x = element_line(color = "gray80", size = 0.5),
                  axis.ticks = element_line(color = "gray80", size = 0.5))

plot_regiao + my_theme
plot_regiao + ggthemes::theme_fivethirtyeight()
plot_regiao + ggthemes::theme_economist()
plot_regiao + ggthemes::theme_wsj()
plot_regiao + theme_dark()
plot_regiao + theme_minimal()
ggsave("plot_casos_por_regiao.png", plot_regiao)
