library("tidyverse")
library("quantmod")

# Valores para el ejemplo
s <- seq(0,130,by=10) # Precio 

long.callpayoff <- function(s,k,v,r,t){
  
    data.frame("payoff" = pmax(0,s-k), "periodo" = s) %>% 
    mutate("profit" = payoff - v*exp(-r*t))
  
}

payoff.longcall <- long.callpayoff(s,100,5,0,0)

g1 <- ggplot(payoff.longcall) + 
  geom_line(aes(x = periodo, y = payoff), colour="red", size=1.5) + 
  xlab("Tiempo de Expiracion") + ylab("Payoff") + 
  theme_bw() + ggtitle("Payoff de una Option Call") + 
  labs(subtitle = "Posicion Larga en Call") + 
  scale_x_continuous(limits=c(70, 130)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

g2 <- ggplot(payoff.longcall) + 
  geom_line(aes(x = periodo, y = profit), colour="red", size=1.5) + 
  xlab("Tiempo de Expiracion") + 
  ylab("Profit") + 
  theme_bw() + 
  ggtitle("Profit de una Option Call") + 
  labs(subtitle = "Posicion Larga en Call") +
  geom_hline(yintercept=0, linetype="dashed",color = "blue", size=1.5) + 
  scale_x_continuous(breaks = round(seq(min(payoff.longcall$periodo), 
                                        max(payoff.longcall$periodo), by =10),1),
                                    limits=c(70, 130)) + 
  scale_y_continuous(breaks = round(seq(min(payoff.longcall$profit), 
                                        max(payoff.longcall$profit), by = 10),1),
                                    limits = c(limits=c(-5, 30))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

long.putpayoff <- function(s,k,v,r,t){
  
    data.frame("payoff" = pmax(k-s,0), "periodo" = s) %>% 
    mutate("profit" = payoff - v*exp(-r*t))

}

payoff.longput <- long.putpayoff(s,70,7,0,0)

g3 <- ggplot(payoff.longput) + 
  geom_line(aes(x = periodo, y = payoff), colour="red", size=1.5) + 
  xlab("Tiempo de Expiracion") + 
  ylab("Payoff") + 
  theme_bw() + 
  ggtitle("Payoff de una Option Put") + 
  labs(subtitle = "Posicion Larga en Put") + 
  scale_x_continuous(limits=c(40, 100)) + 
  scale_y_continuous(limits=c(0, 30)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

g4 <- ggplot(payoff.longput) + 
  geom_line(aes(x = periodo, y = profit), colour="red", size=1.5) + 
  xlab("Tiempo de Expiracion") + 
  ylab("Profit") + 
  theme_bw() + 
  ggtitle("Profit de una Option Put") + 
  labs(subtitle = "Posicion Larga en Put") + 
  geom_hline(yintercept=0, linetype="dashed",color = "blue", size=1.5) +
  scale_x_continuous(breaks = round(seq(min(payoff.longput$periodo), 
                                        max(payoff.longput$periodo), by = 10),1),
                                    limits=c(40, 100)) + 
  scale_y_continuous(breaks = round(seq(min(payoff.longput$profit),                            
                                        max(payoff.longput$profit), by = 10),1),
                                    limits = c(limits=c(-7, 30))) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

short.callpayoff <- function(s,k,v,r,t){
  data.frame("payoff" = pmin(k-s,0),"periodo" = s) %>% 
    mutate("profit" = payoff + v*exp(-r*t))
} 

payoff.shortcall <- short.callpayoff(s,100,5,0,0)

g5 <- ggplot(payoff.shortcall) + 
  geom_line(aes(x = periodo, y = payoff), colour="red", size=1.5) + 
  xlab("Tiempo de Expiracion") + 
  ylab("Payoff") + 
  theme_bw() + 
  ggtitle("Payoff de una Option Call") + 
  labs(subtitle = "Posicion Corta en Call") + 
  scale_x_continuous(limits=c(70, 130)) + 
  scale_y_continuous(limits=c(-30, 0)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

g6 <- ggplot(payoff.shortcall) + 
  geom_line(aes(x = periodo, y = profit), colour="red", size=1.5) + 
  xlab("Tiempo de Expiracion") + 
  ylab("Profit") + 
  theme_bw() + 
  ggtitle("Profit de una Option Call") + 
  labs(subtitle = "Posicion Corta en Call") + 
  geom_hline(yintercept=0, linetype="dashed",color = "blue", size=1.5) + 
  scale_x_continuous(breaks = round(seq(min(payoff.shortcall$periodo), 
                                        max(payoff.shortcall$periodo), by = 10),1),
                                    limits=c(70, 130)) + 
  scale_y_continuous(breaks = round(seq(min(payoff.shortcall$profit), 
                                        max(payoff.shortcall$profit), by = 10),1),
                                    limits = c(limits=c(-30, 5))) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

short.putpayoff <- function(s,k,v,r,t){
  
    data.frame("payoff" = pmin(s-k,0),"periodo" = s) %>% 
    mutate("profit" = payoff + v*exp(-r*t))

}

payoff.shortput <- short.putpayoff(s,70,7,0,0)

g7 <- ggplot(payoff.shortput) + 
  geom_line(aes(x = periodo, y = payoff), colour="red", size=1.5) + 
  xlab("Tiempo de Expiracion") + 
  ylab("Payoff") + 
  theme_bw() + 
  ggtitle("Payoff de una Option Put") + 
  labs(subtitle = "Posicion Corta en Put") + 
  scale_x_continuous(limits=c(40, 100)) + 
  scale_y_continuous(limits=c(-30, 0)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

g8 <- ggplot(payoff.shortput) + 
  geom_line(aes(x = periodo, y = profit), colour="red", size=1.5) + 
  xlab("Tiempo de Expiracion") + 
  ylab("Profit") + 
  theme_bw() + 
  ggtitle("Profit de una Option Put") + 
  labs(subtitle = "Posicion Corta en Put") +
  geom_hline(yintercept=0, linetype="dashed",color = "blue", size=1.5)
  scale_x_continuous(breaks = round(seq(min(payoff.shortput$periodo), 
                                        max(payoff.shortput$periodo), by = 10),1),
                                    limits=c(40, 100)) + 
  scale_y_continuous(breaks = round(seq(min(payoff.shortput$profit),
                                        max(payoff.shortput$profit), by = 10),1),
                                    limits = c(limits=c(-30, 7))) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

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

long.callpayoff <- function(s,k,v,r,t){
  data.frame("payoff" = pmax(0,s-k), "stock" = s) %>% 
    mutate("profit" = payoff - v*exp(-r*t))
}

# Posición corta en call
short.callpayoff <- function(s,k,v,r,t){
  data.frame("payoff" = pmin(k-s,0),"stock" = s) %>% 
    mutate("profit" = payoff + v*exp(-r*t))
} 


bull.spread <- bullspread.call(32,30,35,3,1)

s <- bull.spread$stock

# call larga
long.call <- long.callpayoff(s,30,3,0,0)

# Call corta
short.call <- short.callpayoff(s,35,1,0,0)

g1 <- ggplot(bull.spread[4:10,]) + 
  geom_line(aes(long.call$stock[4:10], long.call$profit[4:10], color = "blue"),
            linetype="dashed" ,size=1) +  
  geom_line(aes(short.call$stock[4:10], short.call$profit[4:10], color = "red"), 
            linetype="dashed", size=1) +
  geom_line(aes(bull.spread$stock[4:10], bull.spread$profit[4:10], color = "green"), 
            size=1) + 
  ggtitle("Estrategia Bull Spreads") + 
  xlab("stock") + 
  ylab("Profit") + 
  theme_bw() + 
  geom_hline(yintercept=0, linetype="dashed", color = "orange", size=1) + 
  scale_color_manual(name = " ", labels = c("Call larga", "Bull Spread", "Call corta"), 
                     values = c("blue"="blue","green"="green","red" = "red")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position="bottom") 
 
library("FinancialMath")

bull.call <- bull.call(31,30,35,0.0000001,5,3,1,plot=TRUE)

# Parámetros
S0 <- 42
K <- 40
r <- 0.1
T <- 1/2
sigma <- 0.2

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

# Para la call
C <- bs.opm(S = S0, K = K, T = T, riskfree = r, sigma = sigma, type = "Call" ) 
C

# Para la put
C <- bs.opm(S = S0, K = K, T = T, riskfree = r, sigma = sigma, type = "Put" )
C

library("fOptions") 

# Con libreria call
GBSOption(TypeFlag = "c", S = S0, X = K, Time = T, r = r, b = r, sigma = sigma)@price

# Con libreria put
GBSOption(TypeFlag = "p", S = S0, X = K, Time = T, r = r, b = r, sigma = sigma)@price

opciones <- getOptionChain("ORCL",Exp="2019-09-20")

# Nos quedamos con las columnas del 1 al 2 y del 4 al 6.
calls <- opciones$calls[,c(1:2,4:6)]
# Nos quedamos con las columnas del 1 al 2 y del 4 al 6.
puts  <- opciones$puts[,c(1:2,4:6)] 

# Al 31 de mayo la valuación, 31 de mayo del 2015 fue domingo, por ende, usamos el viernes 29
getSymbols("ORCL", from = "2015-05-29", to = "2018-06-01", periodicity = "daily")

orcl <- as.data.frame(ORCL) %>%  # a data.frame 
        mutate(ret = log(ORCL.Adjusted/lag(ORCL.Adjusted))) %>%  # retorno
        select(ORCL.Adjusted,ret) %>% # Seleccionamos el retorno y precio de ajuste
        na.omit() # por el NA que se produce del retorno

# extraemos el precio a la fecha que nos piden
precio <- tail(orcl$ORCL.Adjusted,1)

getSymbols("DGS3MO",src ="FRED", to = "2018-06-01", periodicity = "daily")

rf <- as.numeric(subset(DGS3MO["2018-05-31"]))*0.01 

expiracion.date <- as.Date("2018-08-17")
valuacion.date <- as.Date("2018-06-01")
TTM <- as.numeric((expiracion.date-valuacion.date)/365)

vol.hist <- sd(orcl$ret)*sqrt(252)

bs.call <- calls %>% 
           dplyr::filter(Strike == 45 | Strike == 50) %>% 
           select(Strike,Last,Bid,Ask)

bs.put <- puts %>% 
          dplyr::filter(Strike == 45 | Strike == 50) %>% 
          select(Strike,Last,Bid,Ask)

# call
GBSOption(TypeFlag = "c", S = precio, X = 45, Time = TTM, r = rf, b = rf, 
          sigma = vol.hist)@price
GBSOption(TypeFlag = "c", S = precio, X = 50, Time = TTM, r = rf, b = rf, 
          sigma = vol.hist)@price

# put
GBSOption(TypeFlag = "p", S = precio, X = 45, Time = TTM, r = rf, b = rf, sigma = vol.hist)@price
GBSOption(TypeFlag = "p", S = precio, X = 50, Time = TTM, r = rf, b = rf, sigma = vol.hist)@price

library("fOptions")

# Europea 
CRRTree = BinomialTreeOption(TypeFlag = "ce", S = 50, X = 52, Time = 2, r = 0.05, b = 0.05, sigma = 0.3, n = 2)
CRRTree

BinomialTreePlot(CRRTree, dy = 1, cex = 0.8, ylim = c(-6, 7), xlab = "n", ylab = "valores Opción")

title(main = "Arbol Binomial Opción")

# Americana
CRRTree.1 = BinomialTreeOption(TypeFlag = "ca", S = 50, X = 52, Time = 2, r = 0.05, b = 0.05, sigma = 0.3, n = 2)
CRRTree.1

BinomialTreePlot(CRRTree.1, dy = 1, cex = 0.8, ylim = c(-6, 7), xlab = "n", ylab = "valores Opción") 
title(main = "Arbol Binomial Opción")

