# windows utf-8
getwd()
setwd('G:/Projetos Programacao/R_repository/Introduction to Data Analysis with R')
library(tidyverse)
library(ggplot2)
library (descr)

controle_fin <- read.csv("datas/05/aplicativo_controle_financeiro.csv")
glimpse(controle_fin)


# 1. Qual a média, média aparada excluindo 10% dos valores extremos e a
#mediana das horas logadas no aplicativo?

#Media aparada
mean(controle_fin[["horas_logado"]], trim = 0.1)
#Mediana
median(controle_fin[["horas_logado"]])

# -->1.a Por que a mediana é menor do que a media?

# A média é afetada pelos valores extremos que existem no conjunto de dados, 
# sejam muito altos ou muito baixos. 
# Já na mediana isso não acontece, 
# a mediana ainda será igual a (4.62), pois ela é o centro da distribuicao.

# -->1.b Por que a mediana é mais próxima da média aparada?

# A média aparada elimina valores extremos que podem afetar o resultado final, 
# mas mantem um sub-conjunto de dados, os quais estão dentro de um intervalo
# de valores,tornando assim o resultado final mais coeso.

#-------------------------------------------------------------------------------
# 2. Qual o desvio padrão das horas logadas no aplicativo?

#Desvio Padrão
sd(controle_fin[["horas_logado"]])

# -->2.1 a. O que o valor representa?

# O valor do desvio padrão representa o quanto aquele conjunto de dados é 
# uniforme. Quanto mais próximo o desvio padrão estiver de 0, 
# mais uniforme são os dados.

#-------------------------------------------------------------------------------
# 3. Calcule o 1o, 2o e 3o quartis das horas logadas.

#Amplitude Interquatilica
quantile(controle_fin[["horas_logado"]], p=c(0.25, 0.5, 0.75))

# --> 3.a O que o resultado quer dizer?

# Quer dizer que o 1o(25%) das amostras passam ~2hs ou menos logados
# no aplicativo.

# O 2o(50%) das amostras passam mais do que 4hs e meia logados no aplicativo.

# O 3o(75%) das amostras passam mais do que 8hs logados no aplicativo.

#-------------------------------------------------------------------------------
# 4. Visualize os quartis das horas logadas através de um gráfico boxsplot.

boxplot(controle_fin[["horas_logado"]])

# -->4.a Há algum outlier?

# Há um outlier 

# -->4.b O que o outlier significa nesse contexto?

# Significa que há um usuario que passa bem além do tempo medio de horas logadas
# no qual pode alterar consideravelmente o resultado na media caso ele não seja 
# excluido pela media aparada
# EX: media com esse usuario = 5.316013
# media aparada =  5.014279

#-------------------------------------------------------------------------------
# 5. Organize as horas logadas em quatro intervalos e construa uma tabela de
# frequência.

# -->5.a Construa um histograma utilizando os intervalos.

hist(controle_fin[["horas_logado"]], breaks = seq(from = min(controle_fin[["horas_logado"]]), 
                                                  to = max(controle_fin[["horas_logado"]]), 
                                                  length = 11))

# -->5.b Interprete o histograma

# 

#-------------------------------------------------------------------------------
# 6. Qual a relação entre as horas logadas no seu aplicativo e as horas logadas
#no aplicativo do Banco do Brasil? Use o gráfico de dispersão.

plot(controle_fin$horas_logado, controle_fin$horas_logado_BB, 
     xlab="Aplicativo Financeiro", 
     ylab="Aplicativo do BB")

# -->6.a O que o gráfico quer dizer?

# Há uma tentencia de que quanto mais tempo as pessoas usam o aplicativo
# financeiro, menos elas usam o aplicativo do BB

#-------------------------------------------------------------------------------
# 7. Visualize a relação entre as variáveis acima através da compartimentação
#hexagonal.
ggplot(controle_fin, (aes(x = horas_logado, y = horas_logado_BB))) + 
  stat_binhex(colour="black") + 
  theme_bw() + 
  scale_fill_gradient(low="red") +
  labs(x="APP Financeiro", y="APP BB")

# -->7.a O que o gráfico quer dizer?

# Ele indica que há um maior agrupamento de dados das pessoas que passam mais 
# tempo no aplicatibo do BB do que no financeiro

#-------------------------------------------------------------------------------
# 8. Indique a proporção do uso de cartão de crédito por canal (android e ios)
#através de uma tabela de contingência


canais <- CrossTable(controle_fin$horas_logado, controle_fin$canal, 
                    prop.c=FALSE, prop.chisq=FALSE, prop.t=FALSE)

#-------------------------------------------------------------------------------
# 9. Construa gráficos boxplots para comparar as horas logadas por canal.

canal_android <- controle_fin %>%
  group_by(canal, horas_logado) %>%
  filter(canal == "android")

glimpse(canal_android)

canal_ios <- controle_fin %>%
  group_by(canal, horas_logado) %>%
  filter(canal == "ios")

glimpse(canal_ios)

boxplot(canal_android[["horas_logado"]], canal_ios[["horas_logado"]],
        main = "Comparação por canal",
        names = c("Android", "Ios"),
        col = c("orange"),
        border = "brown",
        horizontal = FALSE)

#9.a O que o resultado quer dizer?

# prefiro não opniar, não porque não sei descrever o resultado, mas por 
# causa que não sei se criei ele da forma correta

#-------------------------------------------------------------------------------  
# 10. Utilize o gráfico violino para fazer a mesma comparação.

vio <- ggplot(controle_fin, aes(x=canal, y=horas_logado)) + 
  geom_violin()
vio

# -->10.a Que informação foi agregada com essa visualização comparada com a 
#anterior?

# É visivel proporção de horas logadas em relação ao canal usado, também deixa 
# claro que seja android ou ios, ambos, a maioria dos usuarios passam menos de 
# 10h logados no app financeiro.
