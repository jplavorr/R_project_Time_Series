install.packages("gtrendsR")
install.packages("tidyverse")
library(gtrendsR)
library(tidyverse)

data_scientist <- 
  gtrends(c("Estatística", 
            "Ciência de dados"),
          geo = c("BR"),    
          #default para todo o mundo, podemos utilizar também: c("BR", "US")
          time = "2020-01-01 2022-10-31",  #Data inicial Data final
          gprop = c("web"), #Opções: "news","images", "froogle", "youtube"
          category = 0,     #Zero � defaul, uma lista de categorias pode ser conferida abaixo
          hl = "pt-BR"        
  )


plot(data_scientist) + ggtitle("Interesse ao Longo do Tempo") + ylab("Hits Procurados")
Estatistica=data_scientist$interest_over_time%>%filter(keyword=="Estatística")
serie = ts(Estatistica$hits,start=c(2020,1),frequency=60)
plot(serie) 


acf(serie, main="Autocorrelations" , ylab=" " , ci.col = "black")
pacf(serie, main="Partial Autocorrelations" , ylab="")
###############################
# Teste de raizes unitárias
################################
library(tseries)
library( urca )

plot(serie)
adf.test(serie);
pp.test(serie);

################################
#Sazonalidade
################################
plot(decompose(serie));
Elec.decom <- decompose(serie, type = "mult");
Trend <- Elec.decom$trend;
Seasonal <-Elec.decom$seasonal; 
plot(Seasonal)
#ggseasonplot(serie) + ggtitle("Seasonal Plot") + ylab("Busca")


ts_plot_season <- function(x = x) {
  season <- cycle(x)
  season.factor <- factor(season)
  ggplot() + 
    geom_boxplot(mapping = aes(x = season.factor,
                               y = x)) +
    labs(x = "Periodo", y =  "Serie")
}

ts_plot_season(serie)


###########################################
#Modelagem
###########################################
fit_ets <- ets(serie)
print(summary(fit_ets))
checkresiduals(fit_ets)


###########
#Fit Arima model
###########
#d=1 diz para diferenciar uma vez
fit_arima <- auto.arima(serie, d=0,D=0, stepwise = FALSE,approximation = FALSE, trace = TRUE)
print(summary(fit_arima))
checkresiduals(fit_arima)
sqrt(58.25)


##################################
#Forecasting
##################################
###########
#h=24 são 24 meses a frente
fcst <- forecast(fit_arima, h=10) 
autoplot(fcst)
#Os ultimos 180 meses
autoplot(fcst, include=20)
print(summary(fcst))



