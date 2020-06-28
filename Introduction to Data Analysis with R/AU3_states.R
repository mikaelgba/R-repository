# windows utf-8
getwd()
setwd('G:/Projetos Programacao/R_repository/Introduction to Data Analysis with R')
states <- read.csv("datas/03/state.csv")
states

#Media
mean(states[["Population"]])
#media por exclusao 
mean(states[["Population"]], trim = 0.1)
#Mediana
median(states[["Population"]])
#Media poderada
weighted.mean(states[["Murder.Rate"]], w = states[["Population"]])

---------
#Medidas de Dispensao
#Desvio Padrao
sd(states[["Population"]])

---------
#Medidas separatrizes
#Amplitude Interquatilica
IQR(states[["Population"]])
#percetil 
quantile(states[["Murder.Rate"]], p=c(.05, .25, .5, .75, .95, 1))

---------
boxplot(states[["Population"]]/1000000, ylab="Population (millions)")

breaks <- seq(from=min(states[["Population"]]), 
              to=max(states[["Population"]]), 
              length=11)

pop_freq <- cut(states[["Population"]], 
                breaks=breaks, 
                right=TRUE, 
                include.lowest = TRUE)

states['PopFreq'] <- pop_freq
table(pop_freq)

---------
#Histograma
options(scipen = 999)
hist(states[["Population"]], breaks=breaks)

hist(states[["Murder.Rate"]], freq=FALSE )
lines(density(states[["Murder.Rate"]]), lwd=3, col="blue")

dfw <- read.csv('datas/03/dfw_airline.csv')
barplot(as.matrix(dfw)/6, cex.axis = 0.8, cex.names = 0.7)
  