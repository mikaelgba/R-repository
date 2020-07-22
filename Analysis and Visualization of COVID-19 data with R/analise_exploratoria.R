library(tidyverse)
library(here)
library(lubridate)
library(scales)

dados_pb <- read.csv2(here("Analysis and Visualization of COVID-19 data with R/data", "dados-pb.csv"), stringsAsFactors = TRUE, 
                     fileEncoding = "latin1",
                     na.strings = c("undefined", "", "null")) %>%
  rename(id = 1) %>%
  janitor::clean_names() %>%
  mutate_at(vars(starts_with("data")), as_date)

glimpse(dados_pb)

# 1 

#sexo_casos <- dados_pb %>% group_by(sexo) %>%
#  summarise(quantidade = n()) %>% 
#  mutate(percentual = percent(quantidade / sum(quantidade), accuracy = 1))

#glimpse(sexo_casos)

# 2

#prof_casos <- dados_pb %>%
#  select(cbo, resultado_teste) %>%
#  filter(resultado_teste == "Positivo", cbo != "NA")

#glimpse(prof_casos)

#prof_casos <- prof_casos %>% group_by(cbo) %>%
#  summarise(quantidade = n())

#prof_casos[order(prof_casos$quantidade, decreasing = TRUE),]

# 3

#muni_casos <- dados_pb %>%
#  select(municipio, resultado_teste) %>%
#  filter(resultado_teste == "Positivo")

#muni_casos <- muni_casos %>% group_by(municipio) %>%
#  summarise(quantidade = n())
  
#muni_casos[order(muni_casos$quantidade, decreasing = TRUE),]

# 4
#idade_casos <- dados_pb %>% 
#  select(idade, resultado_teste) %>%
#  filter(resultado_teste == "Positivo")
  
#glimpse(idade_casos)  
  
#idade_casos <- dados_pb %>% group_by(idade <= 12) %>%
#  summarise(quantidade = n()) %>%
#  mutate(percentual = percent(quantidade / sum(quantidade), accuracy = 1))

# idade_casos

# 5 

temp_sint <- dados_pb %>%
  select(data_teste, data_inicio_sintomas) %>%
  filter(data_teste != "NA") %>%
  filter(data_inicio_sintomas != "NA")

glimpse(temp_sint)

temp_sint <- temp_sint %>%
  mutate(tempo_sintomas_teste = as.integer(data_teste - data_inicio_sintomas))

glimpse(mean(temp_sint[["tempo_sintomas_teste"]]))