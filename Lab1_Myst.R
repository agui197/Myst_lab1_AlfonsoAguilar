# Laboratorio 1


#remover objetos del enviroment


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
  Datos <- Quandl.datatable(code = "WIKI/PRICES", qopts.columns=Columns, ticker=Tickers,
                            date.gte=Fecha_In, date.lte=Fecha_Fn)
  return(Datos)
}