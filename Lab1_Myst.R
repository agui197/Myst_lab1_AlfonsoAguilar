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




# 
# options(knitr.table.format = "html") 
# 
# # Capital inicial a considerar
# Capital_Inicial <- 100000
# Comision <- 0.005
# 
# # Cargar el token de QUANDL
# Quandl.api_key("Us_v4rfs-m_kLT1skgsQ")
# 
# # Funcion para descagar precios
# Bajar_Precios <- function(Columns, Tickers, Fecha_In, Fecha_Fn) {
#   
#   # Funcion para descargar N cantidad de activos desde QUANDL
#   # -- Dependencias: QUANDL
#   # -- Columns : columnas a incluir : character : c("date", "adj_close", ... )
#   # -- Tickers : Tickers o claves de pizarra de los activos : character : "TSLA"
#   # -- Fecha_In : Fecha Inicial : character : "2017-01-02"
#   # -- Fecha_Fn : Fecha Final : character : "2017-08-02"
#   
#   # Peticion para descargar precios
#   Datos <- Quandl.datatable("WIKI/PRICES", qopts.columns=Columns, ticker=Tickers,
#                             date.gte=Fecha_In, date.lte=Fecha_Fn)
#   return(Datos)
# }
# #tickers de accciones y datos a solicitar
# #tk <- c("TSLA","BBY","HD")
# cs <-c("date","adj_close")
# 
# 
# #fecha inicial y fecha inicial
# fs <- c("2015-08-01","2016-08-01")
# 
# #descargar precios
# Datos <- list()
# temp <-list()
# 
# for(i in 1:length(tk)){
#   Datos[[i]] <- Bajar_Precios(Columns = cs,Tickers = tk[i],Fecha_In = fs[1],Fecha_Fn = fs[2])
# }

#################
#EJERCICIO CON DIFERENTES ACTIVOS Y UNA BASE DE DATOS ACTUALIZADA
#################

# Capital inicial a considerar
Capital_Inicial <- 100000
Comision <- 0.005

# Cargar el token de QUANDL
Quandl.api_key("Us_v4rfs-m_kLT1skgsQ")
# acciones para ejercicio
tk <- c("DIS","BA","MMM")
Datos <- list()
temp <-list()


temp[[1]] <- Quandl("EOD/DIS")
temp[[2]] <-Quandl("EOD/BA")
temp[[3]] <-Quandl("EOD/MMM")

Datos[[1]] <-temp[[1]]$Date[1]
Datos[[2]] <-temp[[2]]$Date[1]
Datos[[3]] <-temp[[3]]$Date[1]



###Los datos se deben de voltear porque tenemos en el primer dato el mas nuevo
Datos[[1]]$date <- rev(temp[[1]]$Date[1:250])
Datos[[1]]$adj_close <- rev(temp[[1]]$Adj_Close[1:250])
Datos[[2]]$date <- rev(temp[[2]]$Date[1:250])
Datos[[2]]$adj_close <- rev(temp[[2]]$Adj_Close[1:250])
Datos[[3]]$date <- rev(temp[[3]]$Date[1:250])
Datos[[3]]$adj_close <- rev(temp[[3]]$Adj_Close[1:250])





for(i in 1:length(tk))
  Datos[[i]]$adj_close_r <- c(0, diff(log(Datos[[i]]$adj_close)))


Rends <- xts(x = cbind(Datos[[1]]$adj_close_r, Datos[[2]]$adj_close_r, Datos[[3]]$adj_close_r),
             order.by = Datos[[1]]$date)[-1]
names(Rends) <- tk

Port1 <- portfolio.spec(assets=tk)

Port1 <- add.constraint(portfolio = Port1,
                        type = "leverage",
                        min_sum=.99,max_sum=1.01)

# Restriccion 2: Limites superior e inferior para el valor de pesos
Port1 <- add.constraint(portfolio=Port1,
                        type="box", 
                        min=c(0.01, 0.01, 0.01), max=c(0.7, 0.7, 0.7))


Port1 <- add.objective(portfolio=Port1, type="return", name="mean")

Port1 <- optimize.portfolio(R=Rends, portfolio=Port1, optimize_method="random",
                            trace=TRUE, search_size=500)


Portafolios <- vector("list", length = length(Port1$random_portfolio_objective_results))

for(i in 1:length(Port1$random_portfolio_objective_results)) {
  Portafolios[[i]]$Pesos  <- Port1$random_portfolio_objective_results[[i]]$weights
  Portafolios[[i]]$Medias <- Port1$random_portfolio_objective_results[[i]]$objective_measures$mean
  Portafolios[[i]]$Vars   <- var.portfolio(R = Port1$R, weights = Portafolios[[i]]$Pesos)
  names(Portafolios[[i]]$Medias) <- NULL
  
  #al ser una lista portafolios es necesario que al i-esimo elemento se le tienen que poner corchetes
}

df_Portafolios <- data.frame(matrix(nrow=length(Port1$random_portfolio_objective_results),
                                    ncol=3, data = 0))
colnames(df_Portafolios) <- c("Rend","Var","Clase")


for(i in 1:length(Port1$random_portfolio_objective_results)) {
  
  df_Portafolios$Rend[i] <- round(Portafolios[[i]]$Medias*252,4)
  df_Portafolios$Var[i]  <- round(sqrt(Portafolios[[i]]$Vars)*sqrt(252),4)
  df_Portafolios$Clase[i] <- "No-Frontera"
  
  for(k in 1:length(tk)) {
    df_Portafolios[i,paste("Peso_", tk[k],sep="")] <- Portafolios[[i]]$Pesos[k]
    
    df_Portafolios[i,paste("Titulos_ini_", tk[k],sep="")] <-
      (Capital_Inicial*Portafolios[[i]]$Pesos[k])%/%Datos[[k]]$adj_close[1]
  }
  
  
}


### GRAFICAR PORTAFOLIOS




Plot_portafolios <- plot_ly(x=df_Portafolios$Var, y=df_Portafolios$Rend, type='scatter', mode='markers',
                            name = "Portafolios", marker = list(color="grey", size=7), hoverinfo='text', 
                            text = ~paste('Rendimiento: ', paste(df_Portafolios$Rend*100, "%") ,
                                          '<br> Riesgo: ', paste(df_Portafolios$Var*100, "%") )) %>% 
  layout(title = "Portafolios (Markowitz)",
         xaxis = list(title = "Riesgo (Desviaci?n Est?ndar Anualizada)",
                      showgrid = F),
         yaxis = list(title = "Valor Esperado (Rendimiento Anualizado)"),
         legend = list(orientation = 'h', y = -0.25))
Plot_portafolios

Port_1 <- df_Portafolios[which.max(df_Portafolios$Rend),]

# Portafolio con m?nima varianza
Port_2 <- df_Portafolios[which.min(df_Portafolios$Var),]

# Tasa libre de riesgo
rf <- 0.0025          
# Rendimiento de portafolio
rp <- df_Portafolios$Rend
# Varianza de portafolio
sp <- df_Portafolios$Var
# Indice de sharpe
sharpe <- (rp-rf)/sp

# Portafolio con m?ximo Sharpe ratio 
Port_3 <- df_Portafolios[which.max(sharpe),]

Ports <- cbind(rbind(Port_1, Port_2, Port_3),
               "Portafolio" = c("Maximo Rendimiento","Minima Varianza","Maximo Sharpe Ratio"))

Plot_portafolios <- plot_ly(x=df_Portafolios$Var, y=df_Portafolios$Rend, type='scatter', mode='markers',
                            name = "Portafolios", marker = list(color="grey", size=7), hoverinfo='text', 
                            text = ~paste('Rendimiento: ', paste(df_Portafolios$Rend*100, "%") ,
                                          '<br> Riesgo: ', paste(df_Portafolios$Var*100, "%") )) %>% 
  layout(title = "Portafolios (Markowitz)",
         xaxis = list(title = "Riesgo (Desviacion Estandar Anualizada)",
                      showgrid = F),
         yaxis = list(title = "Valor Esperado (Rendimiento Anualizado)"),
         legend = list(orientation = 'h', y = -0.25)) %>%
  add_trace(x = ~Ports$Var[1], y = ~Ports$Rend[1], name = Ports$Portafolio[1],
            mode = 'marker', marker = list(color="red", size=10)) %>%
  add_trace(x = ~Ports$Var[2], y = ~Ports$Rend[2], name = Ports$Portafolio[2],
            mode = 'marker', marker = list(color="blue", size=10)) %>%
  add_trace(x = ~Ports$Var[3], y = ~Ports$Rend[3], name = Ports$Portafolio[3],
            mode = 'marker', marker = list(color="orange", size=10))
Plot_portafolios

# Pesos y titulos iniciales, de todos los activos, para los 3 portafolios
Pesos_Titulos <- Ports[,-c(1,2,3)]

# Encontrar las columnas cuyo nombre contenga "Titulos_ini", con esas encontraremos mas facil los t?tulos
# por portafolio por activo
Ind <- grep(pattern = "Titulos_ini",x = colnames(Pesos_Titulos))
Historicos_Ports <- data.frame("Date" = Datos[[1]]$date)

# Crear data frame que contendr? los datos finales de cada estrategia
for(i in 1:length(Ports[,1])) {
  Historicos_Ports[[paste("Portafolio_",i,sep="")]] <- 
    (Datos[[1]]$adj_close*Pesos_Titulos[i,Ind[1]]  + 
       Datos[[2]]$adj_close*Pesos_Titulos[i,Ind[2]] +
       Datos[[3]]$adj_close*Pesos_Titulos[i,Ind[3]])
}


plot_ly(Historicos_Ports) %>%
  add_trace(x = ~Date, y = ~round(Portafolio_1,2), type = 'scatter', mode = 'lines', name = 'Maximo Rendimiento',
            line = list(color = 'red'), hoverinfo = "text", text = ~paste('Port_1',round(Portafolio_1,2))) %>%
  add_trace(x = ~Date, y = ~round(Portafolio_2,2), type = 'scatter', mode = 'lines', name = 'Minima Varianza',
            line = list(color = 'blue'), hoverinfo = "text", text = ~paste('Port_2',round(Portafolio_2,2)))  %>%
  add_trace(x = ~Date, y = ~round(Portafolio_3,2), type = 'scatter', mode = 'lines', name = 'Maximo Sharpe Ratio',
            line = list(color = 'orange'), hoverinfo = "text", text = ~paste('Port_3',round(Portafolio_3,2)))%>% 
  layout(title = "3 Portafolios distintos objetivos",
         xaxis = list(title = "Fechas", showgrid = T),
         yaxis = list(title = "Balance"), 
         legend = list(orientation = 'h', y = -0.25, x = 0.5))

