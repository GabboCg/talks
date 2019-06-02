## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = FALSE)
knitr::knit_hooks$set(purl = hook_purl)


## ----echo = TRUE, results='hide', message=FALSE, warning=FALSE-----------
library("tidyverse")
library("quantmod")


## ----eval=TRUE-----------------------------------------------------------
theme_xkcd <- theme(
  panel.background = element_rect(fill="white"),
  #axis.ticks = element_line(colour=NA),
  panel.grid = element_line(colour="white"),
  #axis.text.y = element_text(colour=NA),
  axis.text.x = element_text(colour="black"),
  text = element_text(size=10, family="Humor Sans")
)


## ----echo=TRUE-----------------------------------------------------------
# Valores para el ejemplo
s <- seq(0,130,by=10) # Precio 


## ----echo = TRUE,eval=TRUE-----------------------------------------------
long.callpayoff <- function(s,k,v,r,t){
    data.frame("payoff" = pmax(0,s-k), "periodo" = s) %>% 
    mutate("profit" = payoff - v*exp(-r*t))
}

payoff.longcall <- long.callpayoff(s,100,5,0,0)


## ----fig.width=4,fig.height=2.75,fig.align='center',warning=FALSE--------
g1 <- ggplot(payoff.longcall) + geom_line(aes(x = periodo, y = payoff), 
                                              colour="red", size=1.5) + 
      xlab("Tiempo de Expiracion") + ylab("Payoff")
g1 <- g1 + theme_bw() + ggtitle("Payoff de una Option Call")
g1 <- g1 + labs(subtitle = "Posicion Larga en Call")
g1 <- g1 + scale_x_continuous(limits=c(70, 130)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
g1


## ----fig.width=4,fig.height=2.75,fig.align='center',warning=FALSE--------
g2 <- ggplot(payoff.longcall) + geom_line(aes(x = periodo, y = profit), 
                                          colour="red", size=1.5) + xlab("Tiempo de Expiracion") + ylab("Profit")
g2 <- g2 + theme_bw() + ggtitle("Profit de una Option Call") + labs(subtitle = "Posicion Larga en Call") 
g2 <- g2 + geom_hline(yintercept=0, linetype="dashed",color = "blue", size=1.5)
g2 <- g2 + scale_x_continuous(breaks = round(seq(min(payoff.longcall$periodo), 
                              max(payoff.longcall$periodo), by =10),1),
                              limits=c(70, 130)) + 
      scale_y_continuous(breaks = round(seq(min(payoff.longcall$profit), 
                                            max(payoff.longcall$profit), by = 10),1),
                         limits = c(limits=c(-5, 30))) 
g2 <- g2 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
g2


## ----echo=TRUE,eval=TRUE-------------------------------------------------
long.putpayoff <- function(s,k,v,r,t){
    data.frame("payoff" = pmax(k-s,0), "periodo" = s) %>% 
    mutate("profit" = payoff - v*exp(-r*t))
}

payoff.longput <- long.putpayoff(s,70,7,0,0)


## ----fig.width=4,fig.height=2.75,fig.align='center',warning=FALSE--------
g3 <- ggplot(payoff.longput) + geom_line(aes(x = periodo, y = payoff), colour="red", size=1.5) + xlab("Tiempo de Expiracion") + ylab("Payoff")
g3 <- g3 + theme_bw() + ggtitle("Payoff de una Option Put") + labs(subtitle = "Posicion Larga en Put")
g3 <- g3 + scale_x_continuous(limits=c(40, 100)) + scale_y_continuous(limits=c(0, 30))  + theme(panel.grid.major =                  element_blank(), panel.grid.minor = element_blank())
g3


## ----fig.width=4,fig.height=2.75,fig.align='center',warning=FALSE--------
g4 <- ggplot(payoff.longput) + geom_line(aes(x = periodo, y = profit), colour="red", size=1.5) + xlab("Tiempo de Expiracion") + ylab("Profit")
g4 <- g4 + theme_bw() + ggtitle("Profit de una Option Put") + labs(subtitle = "Posicion Larga en Put") 
g4 <- g4 + geom_hline(yintercept=0, linetype="dashed",color = "blue", size=1.5)
g4 <- g4 + scale_x_continuous(breaks = round(seq(min(payoff.longput$periodo), max(payoff.longput$periodo), by =                     10),1),limits=c(40, 100)) + scale_y_continuous(breaks = round(seq(min(payoff.longput$profit),                            max(payoff.longput$profit), by = 10),1),limits = c(limits=c(-7, 30))) 
g4 <- g4 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
g4


## ----eval=TRUE,echo=TRUE-------------------------------------------------
short.callpayoff <- function(s,k,v,r,t){
  data.frame("payoff" = pmin(k-s,0),"periodo" = s) %>% 
    mutate("profit" = payoff + v*exp(-r*t))
} 

payoff.shortcall <- short.callpayoff(s,100,5,0,0)


## ----fig.width=4,fig.height=2.75,fig.align='center',warning=FALSE--------
g5 <- ggplot(payoff.shortcall) + geom_line(aes(x = periodo, y = payoff), colour="red", size=1.5) + xlab("Tiempo de Expiracion") + ylab("Payoff")
g5 <- g5 + theme_bw()+ ggtitle("Payoff de una Option Call") + labs(subtitle = "Posicion Corta en Call")
g5 <- g5 + scale_x_continuous(limits=c(70, 130)) + scale_y_continuous(limits=c(-30, 0)) + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
g5


## ----fig.width=4,fig.height=2.75,fig.align='center',warning=FALSE--------
g6 <- ggplot(payoff.shortcall) + geom_line(aes(x = periodo, y = profit), colour="red", size=1.5) + 
      xlab("Tiempo de Expiracion") + ylab("Profit")
g6 <- g6 + theme_bw() + ggtitle("Profit de una Option Call") + labs(subtitle = "Posicion Corta en Call") 
g6 <- g6 + geom_hline(yintercept=0, linetype="dashed",color = "blue", size=1.5)
g6 <- g6 + scale_x_continuous(breaks = round(seq(min(payoff.shortcall$periodo), max(payoff.shortcall$periodo), by =                 10),1),limits=c(70, 130)) + scale_y_continuous(breaks = round(seq(min(payoff.shortcall$profit),                           max(payoff.shortcall$profit), by = 10),1),limits = c(limits=c(-30, 5))) 
g6 <- g6 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
g6


## ----eval=TRUE,echo=TRUE-------------------------------------------------
short.putpayoff <- function(s,k,v,r,t){
    data.frame("payoff" = pmin(s-k,0),"periodo" = s) %>% 
    mutate("profit" = payoff + v*exp(-r*t))
}

payoff.shortput <- short.putpayoff(s,70,7,0,0)


## ----fig.width=4,fig.height=2.75,fig.align='center',warning=FALSE--------
g7 <- ggplot(payoff.shortput) + geom_line(aes(x = periodo, y = payoff), colour="red", size=1.5) + xlab("Tiempo de Expiracion") + ylab("Payoff")
g7 <- g7 + theme_bw() + ggtitle("Payoff de una Option Put") + labs(subtitle = "Posicion Corta en Put")
g7 <- g7 + scale_x_continuous(limits=c(40, 100)) + scale_y_continuous(limits=c(-30, 0)) + 
      theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
g7


## ----fig.width=4,fig.height=2.75,fig.align='center',warning=FALSE--------
g8 <- ggplot(payoff.shortput) + geom_line(aes(x = periodo, y = profit), colour="red", size=1.5) + xlab("Tiempo de Expiracion") + ylab("Profit")
g8 <- g8 + theme_bw() + ggtitle("Profit de una Option Put") + labs(subtitle = "Posicion Corta en Put") 
g8 <- g8 + geom_hline(yintercept=0, linetype="dashed",color = "blue", size=1.5)
g8 <- g8 + scale_x_continuous(breaks = round(seq(min(payoff.shortput$periodo), max(payoff.shortput$periodo), by =                   10),1),limits=c(40, 100)) + scale_y_continuous(breaks = round(seq(min(payoff.shortput$profit),                           max(payoff.shortput$profit), by = 10),1),limits = c(limits=c(-30, 7))) 
g8 <- g8 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
g8  


## ----eval=TRUE,echo=TRUE, size = "tiny"----------------------------------
bullspread.call <- function(S,K1,K2,precio1,precio2){
  
  if(K1>=S) stop("K1 debe ser menor a S.")
  if(S>=K2) stop("K2 debe ser mayor a S.")
  
  #larga
  callP1 = precio1
  #corto
  callP2 = precio2
  
  stock=unique(round(seq(0,K1,length.out=6)))
  stock=c(stock,round(seq(K1,K2,length.out=4)))
  stock=c(stock,round(seq(K2,K2+K1,length.out=6)))
  stock=unique(stock)
  payoff=rep(0,length(stock))
  profit=rep(0,length(stock))
  
  
  for(i in 1:length(stock)){
    if(stock[i]<=K1) payoff[i]=0
    if(stock[i]>=K2) payoff[i]=K2-K1
    if(stock[i]<K2 & stock[i]>K1) payoff[i]=stock[i]-K1
    profit[i]=payoff[i]+(callP2-callP1)
  }
  
  data <- data.frame(stock,payoff,profit)
  return(data)
} 


## ----echo=TRUE-----------------------------------------------------------
# Posición larga en call
long.callpayoff <- function(s,k,v,r,t){
  data.frame("payoff" = pmax(0,s-k), "stock" = s) %>% 
    mutate("profit" = payoff - v*exp(-r*t))
}

# Posición corta en call
short.callpayoff <- function(s,k,v,r,t){
  data.frame("payoff" = pmin(k-s,0),"stock" = s) %>% 
    mutate("profit" = payoff + v*exp(-r*t))
} 



## ----eval=TRUE-----------------------------------------------------------
bull.spread <- bullspread.call(32,30,35,3,1)


## ----echo=TRUE-----------------------------------------------------------
# Extraemos la St
s <- bull.spread$stock

# Call larga
long.call <- long.callpayoff(s,30,3,0,0)
# Call corta
short.call <- short.callpayoff(s,35,1,0,0)


## ----fig.width=4,fig.height=2.75,fig.align='center',warning=FALSE--------
g1 <- ggplot(bull.spread[4:10,]) + geom_line(aes(long.call$stock[4:10],long.call$profit[4:10],color = "blue"),linetype="dashed"
                                             ,size=1) 
g1 <- g1 + geom_line(aes(short.call$stock[4:10],short.call$profit[4:10], color = "red"),linetype="dashed",size=1) 
g1 <- g1 + geom_line(aes(bull.spread$stock[4:10],bull.spread$profit[4:10],color = "green"),size=1)
g1 <- g1 + ggtitle("Estrategia Bull Spreads") + xlab("stock") + ylab("Profit") + theme_bw()
g1 <- g1 + geom_hline(yintercept=0, linetype="dashed",color = "orange", size=1)
g1 <- g1 + scale_color_manual(name = " ",labels = c("Call larga", "Bull Spread", "Call corta"),values =                                      c("blue"="blue","green"="green","red" = "red"))
g1 <- g1 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="bottom") 
g1 


## ----eval=TRUE,echo=FALSE,fig.width=4.2,fig.height=2.85, fig.align='center'----
library("FinancialMath")

bull.call <- bull.call(31,30,35,0.0000001,5,3,1,plot=TRUE)



## ----eval=TRUE,echo=FALSE,fig.align='center'-----------------------------
knitr::kable(head(bull.call$Payoff,4),caption = "Bull Spread Call")
# Ver las demás estrategias en la librería


## ----eval=TRUE,echo=FALSE,fig.align='center'-----------------------------
knitr::kable(tail(bull.call$Payoff,4),caption = "Bull Spread Call")
# Ver las demás estrategias en la librería


## ----echo=TRUE-----------------------------------------------------------
# Parámetros
S0 <- 42
K <- 40
r <- 0.1
T <- 1/2
sigma <- 0.2


## ----echo=TRUE-----------------------------------------------------------
bs.opm <- function(S,K,T,riskfree,sigma,type){
  d1<-(log(S/K)+(riskfree+0.5 * sigma^2) * T)/(sigma * sqrt(T))
  d2<-d1-sigma * sqrt(T)
  if(type=="Call"){
    opt.val<-S * pnorm(d1)-K * exp(-riskfree * T) * pnorm(d2)
  }
  if(type=="Put"){
    opt.val<-K * exp(-riskfree * T) * pnorm(-d2)-S * pnorm(-d1)
  }
  opt.val
}


## ----eval=TRUE,echo = TRUE-----------------------------------------------
# Para la call
C <- bs.opm(S = S0, K = K, T = T, riskfree = r, sigma = sigma, type = "Call" ) 
C


## ----eval=TRUE,echo = TRUE-----------------------------------------------
# Para la put
C <- bs.opm(S = S0, K = K, T = T, riskfree = r, sigma = sigma, type = "Put" )
C


## ----echo=TRUE,eval=TRUE,message=FALSE-----------------------------------
library("fOptions") 


## ----echo=TRUE,eval=TRUE-------------------------------------------------
# Con libreria call
GBSOption(TypeFlag = "c", S = S0, X = K, Time = T, r = r, b = r, sigma = sigma)@price


## ----echo=TRUE,eval=TRUE-------------------------------------------------
# Con libreria put
GBSOption(TypeFlag = "p", S = S0, X = K, Time = T, r = r, b = r, sigma = sigma)@price


## ----eval=TRUE,echo=TRUE,message=FALSE-----------------------------------
opciones <- getOptionChain("ORCL",Exp="2019-09-20")

# Nos quedamos con las columnas del 1 al 2 y del 4 al 6.
calls <- opciones$calls[,c(1:2,4:6)]
# Nos quedamos con las columnas del 1 al 2 y del 4 al 6.
puts  <- opciones$puts[,c(1:2,4:6)] 


## ----eval=TRUE,echo=TRUE,message=FALSE-----------------------------------
# Al 31 de mayo la valuación, 31 de mayo del 2015 fue domingo, por ende, usamos el viernes 29
getSymbols("ORCL", from = "2015-05-29", to = "2018-06-01", periodicity = "daily")

orcl <- as.data.frame(ORCL) %>%  # a data.frame 
        mutate(ret = log(ORCL.Adjusted/lag(ORCL.Adjusted))) %>%  # retorno
        select(ORCL.Adjusted,ret) %>% # Seleccionamos el retorno y precio de ajuste
        na.omit() # por el NA que se produce del retorno

# extraemos el precio a la fecha que nos piden
precio <- tail(orcl$ORCL.Adjusted,1)


## ----eval=TRUE,echo=TRUE,message=FALSE-----------------------------------
getSymbols("DGS3MO",src ="FRED", to = "2018-06-01", periodicity = "daily")

rf <- as.numeric(subset(DGS3MO["2018-05-31"]))*0.01 

expiracion.date <- as.Date("2018-08-17")
valuacion.date <- as.Date("2018-06-01")
TTM <- as.numeric((expiracion.date-valuacion.date)/365)


## ----eval=TRUE,echo=TRUE-------------------------------------------------
vol.hist <- sd(orcl$ret)*sqrt(252)


## ----eval=TRUE,echo=TRUE-------------------------------------------------

bs.call <- calls %>% 
           dplyr::filter(Strike == 45 | Strike == 50) %>% 
           select(Strike,Last,Bid,Ask)

bs.put <- puts %>% 
          dplyr::filter(Strike == 45 | Strike == 50) %>% 
          select(Strike,Last,Bid,Ask)


## ----eval=TRUE,echo=TRUE-------------------------------------------------
# call
GBSOption(TypeFlag = "c", S = precio, X = 45, Time = TTM, r = rf, b = rf, 
          sigma = vol.hist)@price
GBSOption(TypeFlag = "c", S = precio, X = 50, Time = TTM, r = rf, b = rf, 
          sigma = vol.hist)@price


## ----eval=TRUE,echo=TRUE-------------------------------------------------
# put
GBSOption(TypeFlag = "p", S = precio, X = 45, Time = TTM, r = rf, b = rf, sigma = vol.hist)@price
GBSOption(TypeFlag = "p", S = precio, X = 50, Time = TTM, r = rf, b = rf, sigma = vol.hist)@price


## ----eval=TRUE,echo=TRUE-------------------------------------------------
library("fOptions")


## ----eval=TRUE,echo=TRUE,fig.align='center',fig.height=3.4,fig.width=6.2,results='hide'----
# Europea 
CRRTree = BinomialTreeOption(TypeFlag = "ce", S = 50, X = 52, Time = 2, r = 0.05, 
                             b = 0.05, sigma = 0.3, n = 2)
CRRTree


## ----eval=TRUE,echo=FALSE,fig.align='center',fig.height=3,fig.width=6,results='hide'----
BinomialTreePlot(CRRTree, dy = 1, cex = 0.8, ylim = c(-6, 7), xlab = "n", 
                 ylab = "valores Opción")

title(main = "Arbol Binomial Opción")


## ---- eval=TRUE, echo=TRUE, fig.align='center', fig.height=3.2, fig.width=6, results='hide'----
# Americana
CRRTree.1 = BinomialTreeOption(TypeFlag = "ca", S = 50, X = 52, Time = 2, r = 0.05, 
                               b = 0.05, sigma = 0.3, n = 2)

CRRTree.1


## ---- eval=TRUE, echo=FALSE, fig.align='center', fig.height=3.2, fig.width=6, results='hide'----
BinomialTreePlot(CRRTree.1, dy = 1, cex = 0.8, ylim = c(-6, 7), xlab = "n", ylab = "valores Opción") 
title(main = "Arbol Binomial Opción")

