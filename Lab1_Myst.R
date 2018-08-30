# Laboratorio 1

# Remover todos los objetos del "Environment"
rm(list = ls())

###Cargar librerias a utilizar
(library(plotly))  #graficas interactivas
(library(Quandl))  #Descargar precios
(library(PortfolioAnalytics))  #teoria moderna de portafolios
(library(ROI))  #optimizacion para portafolio
(library(knitr))  #opciones de documentacion + codigo
(library(kableExtra))  #tablas en HTML


options(knitr.table.format = "html") 

# Cargar el token de QUANDL
Quandl.api_key("Us_v4rfs-m_kLT1skgsQ")

# Funcion para descagar precios
Bajar_Precios <- function(Columns, Tickers, Fecha_In, Fecha_Fn) {
  
  # Funcion para descargar N cantidad de activos desde QUANDL
  # -- Dependencias: QUANDL
  # -- Columns : columnas a incluir : character : c("date", "adj_close", ... )
  # -- Tickers : Tickers o claves de pizarra de los activos : character : "TSLA"
  # -- Fecha_In : Fecha Inicial : character : "2017-01-02"
  # -- Fecha_Fn : Fecha Final : character : "2017-08-02"
  
  # Peticion para descargar precios
  Datos <- Quandl.datatable("WIKI/PRICES", qopts.columns=Columns, ticker=Tickers,
                            date.gte=Fecha_In, date.lte=Fecha_Fn)
  return(Datos)
}
#tickers de accciones y datos a solicitar
tk <- c("TSLA","BBY","HD")
cs <-c("date","adj_close")

#fecha inicial y fecha inicial
fs <- c("2015-08-01","2016-08-01")

#descargar precios
Datos <- list()

for(i in 1:length(tk)){
  Datos[[i]] <- Bajar_Precios(Columns = cs,Tickers = tk[i],Fecha_In = fs[1],Fecha_Fn = fs[2])
}
names(Datos) <- tk

for(i in 1:length(tk))
  Datos[[i]]$adj_close_r <- c(0, diff(log(Datos[[i]]$adj_close)))

Rends <- xts(x = cbind(Datos[[1]]$adj_close_r, Datos[[2]]$adj_close_r, Datos[[3]]$adj_close_r),
             order.by = Datos[[1]]$date)[-1]
names(Rends) <- tk

Port1 <- portfolio.spec(assets=tk)

Port1 <- add.constraint(portfolio = Port1,
                        type = "full_investment")

# Restriccion 2: Limites superior e inferior para el valor de pesos
Port <- add.constraint(portfolio = Port1,
                       type = "box",
                       min=c(0.01,0.01,0.01),max=c(0.7,0.7,0.7))

Port1 <- add.objective(portfolio = Port1,type = "return",name = "mean")

Port1 <- optimize.portfolio(R = Rends,portfolio = Port1,optimize_method = "random",
                            trace=TRUE,search_size = 500)






