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
#  filter(resultado_teste == "Positivo" & !is.na(idade)) %>%
#  select(idade) 

#idade_casos_12 <- idade_casos %>% group_by(idade <= 12) %>%
#  summarise(quantidade = n()) %>% 
#  mutate(percentual = percent(quantidade / sum(quantidade), accuracy = 1)) %>%
#  select(percentual)

#idade_casos_60 <- idade_casos %>% group_by(idade >= 60) %>%
#  summarise(quantidade = n()) %>% 
#  mutate(percentual = percent(quantidade / sum(quantidade), accuracy = 1)) %>%
#  select(percentual)

#print("Idade igual ou menor que 12 anos")
#glimpse(idade_casos_12[order(idade_casos_12$percentual),])
#print("Idade igual ou maior que 60 anos")
#glimpse(idade_casos_60[order(idade_casos_60$percentual),])
  
# 5

#temp_sint <- dados_pb %>%
#  filter(!is.na(data_inicio_sintomas) & !is.na(data_teste)) %>%
#  mutate(tempo_sintomas_teste = as.integer(data_teste - data_inicio_sintomas))

#glimpse(mean(temp_sint[["tempo_sintomas_teste"]]))

# 6

#data_max <- dados_pb %>% 
#  filter(!is.na(data_inicio_sintomas)) %>%
#  group_by(data_inicio_sintomas) %>%
#  summarise(quantidade = n())

#data_max[order(data_max$quantidade, decreasing = TRUE),]

# 7

#sint_comuns <- dados_pb %>% filter(!is.na(sintomas) & resultado_teste == "Positivo") %>%
#  select(sintomas) %>%
#  group_by(sintomas) %>%
#  summarise(quantidade = n())
  
# sint_comuns[order(sint_comuns$quantidade, decreasing = TRUE),]

# 8 

#sint_comuns_idade_60 <- dados_pb %>% 
#  filter(!is.na(sintomas) & resultado_teste == "Positivo" & idade >= 60) %>%
#  group_by(sintomas) %>%
#  summarise(quantidade = n())

#sint_comuns_idade_60[order(sint_comuns_idade_60$quantidade, decreasing = TRUE),]


# 9

#cidade_inf <- dados_pb %>% 
#  filter(!is.na(municipio) 
#         & evolucao_caso == "Cura"
#         & resultado_teste == "Positivo") %>%
#  group_by(municipio) %>%
#  summarise(quantidade = n())
  
#cidade_inf[order(cidade_inf$quantidade, decreasing = TRUE),]

#10

tipo_test <- dados_pb %>% filter(!is.na(tipo_teste)) %>%
  group_by(tipo_teste) %>%
  summarise(quantidade = n()) %>% 
  mutate(percentual = percent(quantidade / sum(quantidade), accuracy = 1)) %>%
  select(tipo_teste, percentual)
    
tipo_test[order(tipo_test$percentual, decreasing = TRUE),] 
    