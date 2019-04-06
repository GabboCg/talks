#'---
#'author: Gabriel Cabrera G.
#'title: ayudantia 2 
#'date: 06/08/2018
#'---

# libraries 
# install.packages("IntroCompFinR", repos="http://R-Forge.R-project.org")

#cargamos la libreria 
if(!require("pacman")) install.packages("pacman")
p_load("PerformanceAnalytics","tbl2xts","readxl","tidyverse")

source("IntroCompFinR.R")

# Parte 1  ----------------------------------------------------------------

# acá están los retornos ya calculados, para replicarlos vean el apunte
stocks <- read_xlsx("stocks.xlsx")

mean <- apply(stocks[2:4], 2 , function(x) mean(x)) 
sd   <- apply(stocks[2:4], 2 , function(x) sd(x))
cov  <- cov(stocks[2:4])   

# graficamos el trade-off riesgo-retorno
g1 <- ggplot(mapping = aes(sd, mean, label = c("NODS","SBUX","MSFT"))) + geom_point() + geom_text(hjust = 0, vjust = 0)
g1 <- g1 + scale_y_continuous(breaks = seq(0,0.2, by = 0.01),limits = c(0,0.08)) + scale_x_continuous(breaks = seq(0,0.2, by = 0.02),limits = c(0,0.2))
g1 <- g1 + theme_bw() + xlab("Riesgo") + ylab("Retorno") +  ggtitle("Trade-off Riesgo-Retorno",subtitle = "Tres activos riesgosos")
g1   

# construimos los pesos 
weights <- rep(1,3)/3

# construimos el portfolio
getPortfolio(mean, cov, weights)


# Parte 2 -----------------------------------------------------------------

# portfolio de minima varianza 
globalmin <- globalMin.portfolio(mean, cov, shorts = TRUE)

g2 <- ggplot() + geom_point(mapping = aes(globalmin$sd, globalmin$er, color = "1")) + geom_point(mapping = aes(sd, mean, color = "2")) 
g2 <- g2 + scale_y_continuous(breaks = seq(0,0.2, by = 0.01),limits = c(0,0.06)) + scale_x_continuous(breaks = seq(0,0.2, by = 0.02),limits = c(0,0.2)) 
g2 <- g2 + scale_color_manual("", values = c("green", "red"), labels = c("Min Var.", "Stocks 1"))
g2 <- g2 + theme_bw() + xlab("Riesgo") + ylab("Retorno") +  ggtitle("Trade-off Riesgo-Retorno", subtitle = "Tres activos riesgosos & minima varianza")
g2


# Parte 3 -----------------------------------------------------------------

# portfolio de minima varianza sujeto a un retorno objetivo 
# retorno igual a Nordstrom
port.nods <- efficient.portfolio(mean, cov, mean[1], shorts = TRUE)

# retorno igual a Starbucks
port.sbux <- efficient.portfolio(mean, cov, mean[2], shorts = TRUE)

# retorno igual a Microsoft
port.msft <- efficient.portfolio(mean, cov, mean[3], shorts = TRUE)

mean.2 <- c(port.nods$er, port.sbux$er, port.msft$er)
sd.2 <- c(port.nods$sd, port.sbux$sd, port.msft$sd)

g3 <- ggplot() + geom_point(mapping = aes(sd, mean, color = "1")) + geom_point(mapping = aes(sd.2, mean.2, color = "2")) 
g3 <- g3 + geom_point(mapping = aes(globalmin$sd, globalmin$er, color = "3"))
g3 <- g3 + scale_y_continuous(breaks = seq(0,0.2, by = 0.01),limits = c(0,0.06)) + scale_x_continuous(breaks = seq(0,0.2, by = 0.02),limits = c(0,0.2)) 
g3 <- g3 + scale_color_manual("", values = c("blue", "red", "green"), labels = c("Stocks 1", "Stocks 2", "Min var."))
g3 <- g3 + theme_bw() + xlab("Riesgo") + ylab("Retorno") +  ggtitle("Trade-off Riesgo-Retorno", subtitle = "Tres activos riesgosos & minima varianza")
g3


# Parte 4  ----------------------------------------------------------------

# Tasa libre de riesgo
risk_free <- 0.005

# Portafolio tangente 
port.tang <- tangency.portfolio(mean, cov, risk_free, shorts = TRUE)

#sharpe ratio 
sharpe.ratio <- (port.tang$er - risk_free)/port.tang$sd

g4 <- ggplot() + geom_point(mapping = aes(sd, mean, color = "1")) + geom_point(mapping = aes(sd.2, mean.2, color = "2")) 
g4 <- g4 + geom_point(mapping = aes(port.tang$sd, port.tang$er, color = "3")) +  geom_point(mapping = aes(globalmin$sd, globalmin$er, color = "4"))
g4 <- g4 + geom_abline(intercept = risk_free, slope = sharpe.ratio) 
g4 <- g4 + scale_y_continuous(breaks = seq(0,0.2, by = 0.01),limits = c(0,0.06)) + scale_x_continuous(breaks = seq(0,0.2, by = 0.02),limits = c(0,0.2)) 
g4 <- g4 + scale_color_manual("", values = c("blue", "red", "orange", "green"), labels = c("Stocks 1", "Stocks 2", "Tang. Port", "Min var."))
g4 <- g4 + theme_bw() + xlab("Riesgo") + ylab("Retorno") +  ggtitle("Trade-off Riesgo-Retorno", subtitle = "Tres activos riesgosos & minima varianza")
g4


# Parte 5 -----------------------------------------------------------------

# frontera eficiente con venta corta 
eff.front.short <- efficient.frontier(mean, cov, nport = 25, alpha.min = -2, alpha.max = 1.5, shorts = TRUE)

g5 <- ggplot() + geom_point(mapping = aes(eff.front.short$sd, eff.front.short$er, color = "1")) + geom_point(mapping = aes(sd, mean, color = "2")) 
g5 <- g5 + geom_point(mapping = aes(sd.2, mean.2, color = "3")) + geom_point(mapping = aes(port.tang$sd, port.tang$er, color = "4")) 
g5 <- g5 + geom_point(mapping = aes(globalmin$sd, globalmin$er, color = "5"))
g5 <- g5 + geom_abline(intercept = risk_free, slope = sharpe.ratio)
g5 <- g5 + scale_y_continuous(breaks = seq(0,0.2, by = 0.01), limits = c(0,0.08)) 
g5 <- g5 + scale_x_continuous(breaks = seq(0,0.2, by = 0.02), limits = c(0,0.2)) 
g5 <- g5 + scale_color_manual("", values = c("black","blue", "red", "orange", "green"), labels = c("Frontera", "Stocks 1", "Stocks 2", "Tang. Port", 
                                                                                                   "Min var."))
g5 <- g5 + theme_bw() + xlab("Riesgo") + ylab("Retorno") +  ggtitle("Trade-off Riesgo-Retorno", subtitle = "Tres activos riesgosos & minima varianza")
g5

# Parte 6 -----------------------------------------------------------------
stocks$mercado <- rnorm(59, 0.05, 0.01)
stocks_new <- tbl_xts(stocks)

print(TreynorRatio(stocks_new[,1], stocks_new[,4], Rf = 0.005, scale = NA, modified = FALSE))
