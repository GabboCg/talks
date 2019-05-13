#'---
#'author: Gabriel Cabrera G.
#'title: ayudantia 3
#'date: 13/05/2019
#'---

# Librerias ---------------------------------------------------------------
if(!require("pacman")) install.packages("pacman")
p_load("tidyverse", "quantmod")


# Precio de un Bono: Ejemplo de Clases ------------------------------------

# yield-to-maturity = 6.9% anual 
tc <- 0.065
y  <- 0.069

# Creamos un vector con los pagos de los cupones más el principal
pago <- c(rep(tc*100/2,49),(100 + tc*100/2))

# Convertimos a data frame los pagos 
pago <- as.data.frame(pago)


# Forma 1
# Creamos un nuevo objeto con el operador %>% 
pago1 <- pago %>% 
  mutate(t1 = as.numeric(index(pago)),factor_desc = 1/(1+y/2)^(t1),
         val_present = pago*factor_desc) %>% 
  summarise(sum(val_present)) 

pago1


# Forma 2
# replicamos el objeto
pago2 <- pago

pago2$t2 <- as.numeric(rownames(pago2))

# Calculamos el factor de descuento
pago2$factor_desc <- 1 / (1 + y/2)^(pago2$t2)

# Calculamos el valor presente
pago2$val_present <-  pago2$factor_desc*pago2$pago

# Calculamos el precio
sum(pago2$val_present)


# Otra cosa es con funciones ----------------------------------------------

# p: valor nominal; tc: tasa cupón; t: madurez; y: yield to maturity
precio.bono <- function(p,tc,t,y){
  # rep returns a vector with value = p * r and times = ttm -1 
  pago   <- c(rep(tc*p, t - 1),p*(1 + tc))
  pago   <- as.data.frame(pago)
  pago$t <- as.numeric(rownames(pago))
  pago$factor_desc <- 1 / (1 + y)^(pago$t)
  pago$valor_prese <- pago$factor_desc*pago$pago
  sum(pago$valor_prese)
}

# Veamos si funciona
precio.bono(100,0.065/2,50,0.069/2)


# Relación precio del Bono y Yield ----------------------------------------

# Valoramos el siguiente Bono
precio.bono(p = 100, tc =0.05, t =10, y = 0.0429)

# Cosntruimos yields 
yields <- seq(0.02, 0.4, 0.01)

# Convertimos yields a data frame como antes 
yields <- as.data.frame(yields)

# Calaculamos el precio del bono para distintas yields
for (i in 1:nrow(yields)) {
  yields$precio[i] <- precio.bono(100, 0.10, 20, yields$yields[i])  
}

# Graficamos con ggplot2
g1 <- ggplot(data = yields,aes(x = yields*100, y = precio)) + geom_line(size = 1.5, color = "red") + geom_point(size = 3, color = "red")
g1 <- g1 + ggtitle("Relación inversa:", subtitle = "Precio del Bono vs Yield") + xlab("Yield (%)") + ylab("Precio del bono") 
g1 <- g1 + geom_ribbon(aes(ymin = 0, ymax = pmax(precio,0)), fill="pink", col="red", alpha=0.5) + theme_bw() 
g1 <- g1 + theme(panel.border = element_rect(colour = "black", fill = NA, size = .5),panel.grid.major = element_line(colour = "#d3d3d3"))    
g1

# Guarmados gráfico
ggsave("retorno-yield.png",width = 8.5, height = 4.5, dpi = 300)


# Con plot 
g2 <- plot(yields$yields*100,yields$precio,type = "l",col = "red",main = "Relación inversa: Precio del Bono vs Yield",
           xlab="Yield (%)", ylab="Precio del bono")


# Trabajando con yields reales --------------------------------------------

t10yr <- getSymbols(Symbols = "DGS10", src = "FRED", auto.assign = FALSE)

t10yr <- subset(t10yr["2000-01-01/2018-04-17"])

# Grafico con chartSeries de quantmod solo funciona con xts
chartSeries(t10yr)

t10yr.df <- as.data.frame(t10yr)

t10yr.df <- t10yr.df %>% 
  mutate(fecha = as.Date(rownames(t10yr.df))) %>% 
  na.omit()

g3 <- ggplot(data = t10yr.df,aes(x = fecha , y = DGS10)) + geom_line(size = 1, color = "green")
g3 <- g3 + ggtitle("10-Year US Treasury Yields", subtitle = "Desde 2000-01-01 hasta 2018-04-17") + ylab("Fecha") +xlab("Yield(%)") 
g3 <- g3 + theme_bw() + theme(panel.border = element_rect(colour = "black", fill = NA, size = .5),panel.grid.major = element_line(colour = "#d3d3d3"))
g3

# Guarmados gráfico
ggsave("treasury-yields.png",width = 8.5, height = 4.5, dpi = 300)


# Extraemos un valor en específico
t10yr_yield <- t10yr.df %>% 
  subset(fecha == "2017-03-03") 

t10yr_yield <- as.numeric(t10yr_yield$DGS10)*0.01               

# duracion de Macaulay
macaulay <- function(y,n,c,t,T){
  mac <- (1 + y)/y - (1+y+(n*(c-y)))/(c*((1+y)^n -1) + y)
  print(mac)
} 

macaulay <- macaulay(t10yr_yield,10,0.03)
macaulay

# duración modificada
modificada <- macaulay/(1+t10yr_yield)
modificada 

# Para la aproximación de la duración modificada
precio.arriba <- precio.bono(p = 100, tc = 0.03, t = 10, y = t10yr_yield + 0.01)
precio        <- precio.bono(p = 100, tc = 0.03, t = 10, y = t10yr_yield)
precio.abajo  <- precio.bono(p = 100, tc = 0.03, t = 10, y = t10yr_yield - 0.01)

# Calculo de aproximación duración modificada
aprox.dur.mod <- (precio.abajo - precio.arriba)/(2 * precio * 0.01)
aprox.dur.mod

# convexidad
convexidad <- function(p,tc,t,y,precio){
  # rep returns a vector with value = p * r and times = ttm -1 
  pago   <- c(rep(tc*p, t - 1),p*(1 + tc))
  pago   <- as.data.frame(pago)
  pago$t <- as.numeric(rownames(pago))
  pago$factor_desc <- 1 / (1 + y)^(pago$t)
  pago$valor_prese <- pago$factor_desc*pago$pago*((pago$t)^2 + pago$t)
  print(sum(pago$valor_prese)*(1/(precio*(1+y)^2)))
}

convexidad(100, 0.03, 10, t10yr_yield, precio.bono(p = 100, tc = 0.03, t = 10, y = t10yr_yield))

# Calculamos una aproximación de la convexidad
convexidad <- (precio.arriba + precio.abajo  - 2 * precio)/(precio * (0.01)^2)
convexidad


# Con librerias
p_load("derivmkts")

duration(precio, 3, 10, 100, 1, modified = TRUE)
duration(precio, 3, 10, 100, 1, modified = FALSE)

convexity(precio, 3, 10, 100, 1)

# Ejercicio ---------------------------------------------------------------
# Parte a
yield <- getSymbols(Symbols = "DAAA", src = "FRED", auto.assign = FALSE)
yield <- as.numeric(subset(yield["2016-09-30"]))*0.01

precio_bono <- function(p,tc,t,y){
  pago <- c(rep(tc * p, t - 1),p * (1 + tc))
  pago <- as.data.frame(pago)
  pago$t <- as.numeric(rownames(pago))
  pago$factor_desc <- 1 / (1 + y)^(pago$t)
  pago$valor_prese <- pago$factor_desc * pago$pago
  sum(pago$valor_prese)
}

macaulay <- function(y,n,c,t,T){
  mac <- (1 + y)/y - (1+y+(n * (c-y)))/(c * ((1+y)^n -1) + y)
}

convexidad <- function(p,tc,t,y){
  pago <- c(rep(tc * p, t - 1),p * (1 + tc))
  pago <- as.data.frame(pago)
  pago$t <- as.numeric(rownames(pago))
  pago$factor_desc <- 1 / (1 + y)^(pago$t)
  pago$valor_prese <- pago$factor_desc * pago$pago
  pago$valor_prese_t <- (pago$factor_desc * pago$pago) * pago$t * (pago$t + 1)
  (sum(pago$valor_prese_t)/sum(pago$valor_prese))/(1 + y)^2
}

# Parte b
duracion_macaulay <- macaulay(yield, 8, 0.03)
duracion_macaulay_mod <- duracion_macaulay /(1+yield)
convexidad <- convexidad(precio, 0.03, 8, yield)

# Parte c
duracion_pct_cambio <- -duracion_macaulay_mod*0.01
duracion_dolar_cambio <- duracion_pct_cambio*precio

convexidad_pct_cambio <- convexidad*0.5*(0.01)^2
convexidad_dolar_cambio <- convexidad_pct_cambio * precio

# Parte d
cambio_precio <- duracion_dolar_cambio + convexidad_dolar_cambio

nuevo_precio <- cambio_precio + precio