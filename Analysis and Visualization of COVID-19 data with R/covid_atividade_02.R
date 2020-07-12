library(ggplot2)
library(ggthemes)
library(here)
library(lubridate)
library(tidyverse)

func_plot = function(dados, labs_X, labs_Y, titulo, ultimo_dia, entrada_tipo){
  
  if(entrada_tipo == "casosAcumulado"){
    
    ggplot(dados, aes(data, casosAcumulado))  + 
      geom_area(aes(fill = regiao)) + 
      scale_x_date(expand = c(0, 0)) +
      coord_cartesian(xlim = c(lubridate::dmy("25-02-2020"), ultimo_dia)) +
      scale_fill_brewer(name = "Região", palette = "Set3") + 
      labs(x = labs_X, y = labs_Y) + 
      ggtitle(titulo)
  } else {
    
    ggplot(dados, aes(data, obitosAcumulado)) + 
      geom_area(aes(fill = regiao)) + 
      scale_x_date(expand = c(0, 0)) +
      coord_cartesian(xlim = c(lubridate::dmy("25-02-2020"), ultimo_dia)) +
      scale_fill_brewer(name = "Região", palette = "Set3") + 
      labs(x = labs_X, y = labs_Y) + 
      ggtitle(titulo)
  }
}

theme_plot = function(plot_entrada){
  
  plot_entrada + theme_dark() + theme(legend.title = element_blank(),
                                                      legend.position = c(0.24, 0.68),
                                                      legend.direction = "vertical")
}
#-------------------------------------------------------------------------------
url_br <- "https://raw.githubusercontent.com/marcuswac/covid-br-data/master/covid-br-ms-country.csv"
covid_br <- read_csv(url_br, col_types = cols(data = col_date()))

glimpse(covid_br)

covid_brasil <- covid_br %>%
  group_by(regiao, data) %>%
  summarise(casosAcumulado = sum(casosAcumulado),
            obitosAcumulado = sum(obitosAcumulado)) %>%
  filter(data >= lubridate::dmy("25-02-2020"))

glimpse(covid_brasil)
#-------------------------------------------------------------------------------
url_estados <- "https://raw.githubusercontent.com/marcuswac/covid-br-data/master/covid-br-ms-states.csv"
covid_reg <- read_csv(url_estados, col_types = cols(data = col_date()))

glimpse(covid_reg)

covid_regiao <- covid_reg %>%
  group_by(regiao, data) %>% summarise(casosNovos = sum(casosNovos),
                                       casosAcumulado = sum(casosAcumulado),
                                       obitosNovos = sum(obitosNovos),
                                       obitosAcumulado = sum(obitosAcumulado)) %>% 
  filter(data >= lubridate::dmy("25-02-2020"))

glimpse(covid_regiao)
ultimo_dia <- max(covid_regiao$data)

# plots ------------------------------------------------------------------------
plot_casos_Acumulados_brasil <- func_plot(covid_brasil,
                                           "Notificados","Casos", 
                                           "Total de casos no Brasil", 
                                          ultimo_dia, "casosAcumulado")
p1 <- plot_casos_Acumulados_brasil
p1 <- theme_plot(p1)
#-------
plot_obitos_Acumulados_brasil <- func_plot(covid_brasil,
                                           "Notificados","Obitos", 
                                           "Total de obitos no Brasil",
                                           ultimo_dia, "obitosAcumulado")
p2 <- plot_obitos_Acumulados_brasil
p2 <- theme_plot(p2)
#-------

plot_casosAcumulado_regiao <- func_plot(covid_regiao,
                                     "Notificados","Obitos", 
                                     "Total de casos por região",
                                     ultimo_dia, "casosAcumulado")

p3 <- plot_casosAcumulado_regiao
p3 <- theme_plot(p3)
#-------

plot_obitosAcumulado_regiao <- func_plot(covid_regiao,
                                     "Notificados","Obitos", 
                                     "Total de obitos por região",
                                     ultimo_dia, "obitosAcumulado")

p4 <- plot_obitosAcumulado_regiao
p4 <- theme_plot(p4)
#-------

p1
p2
p3
p4

ggsave("plot_casos_acumulados_por_brasil.png", p1)
ggsave("plot_Obitos_acumulados_por_brasil.png", p2)
ggsave("plot_casos_acumulados_pelo_regiao.png", p3)
ggsave("plot_Obitos_acumulados_pelo_regiao.png", p4)