#'---
#'author: Gabriel Cabrera G.
#'title: Ayudantía 2
#'subtitile: Introducción a quantmod
#'date: 30/07/2018
#'---

# instalamos la librería
install.packages("tidyverse")
install.packages("quantmod")

# "llamamos" a la libreria quantmod
library("quantmod")
library("tidyverse")


# Parte 1  ---------------------------------------------------------------------

# descargamos los datos 
getSymbols("^GSPC", src = "yahoo", from = "2010-01-01", to = "2018-07-30", periodicity = "daily")

# Creamos un nuevo objeto que ahora es un data.frame
gspc <- as.data.frame(GSPC)

# Graficamos usando chartSeries sin análisis técnico 
chartSeries(GSPC, TA=NULL)

# Graficamos usando chartSeries con volume
chartSeries(GSPC)

# Graficando S&P 500 con Valume y los tres últimos meses
chartSeries(GSPC,subset = "last 3 months")

# Usando ggplot2 
g1 <- ggplot(gspc) + geom_line(mapping = aes(index(gspc),GSPC.Adjusted))
g1 <- g1 + labs(title = "S&P 500", subtitle = "Desde Enero 2010 a 2018") + xlab("Fecha") + ylab("")
g1 <- g1 + theme_bw()
g1 # que malo hay?? 


# Parte 2 ----------------------------------------------------------------------

# Nuevos tickers
tickers <- c("ORCL","AMD","IBM","NVDA")

# descargamos los tickers
getSymbols(tickers, src = "yahoo", from = "2010-01-01", to = "2018-07-30", periodicity = "daily")

# Precio de cierre
list <-  lapply(tickers, function(x) Cl(get(x)))
precio.cierre <- do.call(merge,list)

# removemos los objetos que no vamos a usar
rm(tickers, AMD, IBM, NVDA, ORCL, list)

# calculamos los  retornos
retornos <- data.frame(apply(precio.cierre, 2, function(x) Delt(x, type = "log")), fecha = index(precio.cierre)) %>%  
            rename(orcl = ORCL.Close, amd = AMD.Close, ibm = IBM.Close, nvda = NVDA.Close) %>% 
            na.omit() 

# calculamos los retornos acumulados
acumulados <- data.frame(apply(retornos[1:4], 2, function(x) cumsum(x)), fecha = index(precio.cierre[-1]))

# graficamos los retornos acumulados forma 1 
library("reshape2")

# Cambiamos la forma de los datos
reshape <- melt(acumulados, id.vars = "fecha")

# graficamos los retornos acumulados forma 1
g3 <- ggplot(reshape) + geom_line(mapping = aes(fecha,value, color = variable))
g3 <- g3 + labs(title = "Retornos Acumulados", subtitle = "Oracle, AMD, IBM y Nvidia")
g3 <- g3 + theme_bw() + xlab("Fecha") + ylab("Retornos Acumulados")
g3 <- g3 + scale_color_manual("Tickers", values = c("red","blue","green","orange"))
g3 <- g3 + theme(legend.position = "bottom")
g3

# graficamos los retornos acumulados forma 2
g2 <- ggplot(acumulados)
g2 <- g2 + geom_line(aes(fecha,orcl,color = "red"))
g2 <- g2 + geom_line(aes(fecha,amd,color = "blue"))
g2 <- g2 + geom_line(aes(fecha,ibm,color = "green"))
g2 <- g2 + geom_line(aes(fecha,nvda,color = "orange"))
g2 <- g2 + labs(title = "Retornos Acumulados", subtitle = "Oracle, AMD, IBM y Nvidia")
g2 <- g2 + theme_bw() + xlab("Fecha") + ylab("Retornos Acumulados")
g2 <- g2 + scale_color_manual("Tickers",values = c("blue", "green", "orange", "red"), labels = c("amd","ibm","nvda","orcl"))
g2 <- g2 + theme(legend.position = "bottom")
g2


# Parte 3 ----------------------------------------------------------------------

# Ratio de Sharpe
library("tseries")

# forma 1 
SR_orcl <- (mean(retornos$orcl) - 0)/sd(retornos$orcl)

# forma 2 
SR_orcl <- (mean(as.ts(retornos[1])) - 0)/sd(as.ts(retornos[1]))

# cargamos la librería fBasics
library("fBasics")

summary <- basicStats(retornos[1:4])[c("Mean", "Stdev", "Median", "Minimum", "Maximum", "nobs","Skewness","Kurtosis"),]

# aplicar jarqueberatest, test de normalidad
apply(retornos[1:4], 2, function(x) jarqueberaTest(x))  

# jarquebera de Fbasics
jarqueberaTest(retornos$orcl)
jarqueberaTest(retornos$amd)
jarqueberaTest(retornos$ibm)
jarqueberaTest(retornos$nvda)

# vemos el jb de tseries
set.seed(100)
x <- runif(5000)
jarque.bera.test(x)

# con funciones
jb <- function(x){
      n <- length(x)         
      m1 <- sum(x)/n         
      m2 <- sum((x-m1)^2)/n  
      m3 <- sum((x-m1)^3)/n  
      m4 <- sum((x-m1)^4)/n  
      b1 <- (m3/m2^(3/2))^2  
      b2 <- (m4/m2^2)        
      statistic <- n*b1/6+n*(b2-3)^2/24
      p_value <- 1 - pchisq(statistic,df = 2)
      print(p_value)
}

# probamos la funcion
jb(x)


