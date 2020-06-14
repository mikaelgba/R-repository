library(dplyr) # for manipulating data
library(ggplot2) # for plotting data

casos_estados <- read.csv("https://raw.githubusercontent.com/marcuswac/covid-br-data/master/covid-br-ms-states.csv")

glimpse(casos_estados) 
summary(casos_estados) 
names(casos_estados)

casos_sp_rj <- casos_estados %>% filter(estado == "SP" | estado == "RJ") %>% 
  mutate(data = as.Date(data, format = "%Y-%m-%d"))

plot1 <- ggplot(casos_sp_rj, aes(x = data, y = casosAcumulado,
                                 color = estado)) + geom_line() 
plot1

ggsave("casos_covid19_sp_rj.pdf", plot1)