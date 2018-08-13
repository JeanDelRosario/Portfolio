#Todas las funciones asumen que se tiene la columna de fecha primero y todos las demas columnas
#son precios de diferentes acciones

#Ademas por ahora todo sera llevado a terminos anuales

crecimiento <- function(x, type = "logarithmic"){
  if(type == "logarithmic"){
    cbind(x[-1, 1], log( x[-1,2:ncol(x)] / x[-nrow(x),2:ncol(x)]) )
  }
  else if(type == "arithmetic"){
    cbind(x[-1, 1], ( x[-1,2:ncol(x)] - x[-nrow(x),2:ncol(x)]) / x[-nrow(x),2:ncol(x)] )
  }
}

rendimiento <- function(x){
  #Rendimiento promedio anual utilizando logaritmo natural
  x2 <- crecimiento(x)
  
  #Rendimiento promedio
  #Usare data.frame en el apply, debido a que si se usa una funcion con una sola columna de precios
  #Apply da error (ya que al quitarle la primera columna al data frame se convierte en un vector)
  data.frame( t( data.frame(media = (apply(data.frame(x2[,-1]), 2, mean) * 252 ),
                               desviacion = apply(data.frame(x2[,-1]), 2, sd) * sqrt(252) )))
}

rendimiento_geometrico <- function(x){
  x2 <- crecimiento(x, type = "arithmetic")
  
  rend <- apply(1+x2[,-1],2,function(z) prod(z))^(1/nrow(x2))
  #Anualizando y quitando 100%
  
  rend <- rend^252 - 1 
  #Rendimiento promedio
  t( data.frame(geometric_mean =  rend) )
}


regresion <- function(x, benchmark_name){
  #Alpha y Beta
  #Tomamos en cuenta que el alfa no toma en cuenta la TLR (Tasa Libre de Riesgo)
  x <- crecimiento(x)
  x2 <- x[, benchmark_name]
  regre <- apply(x[,2:12], MARGIN = 2, FUN = function(y) lm(y ~ x2)$coef)
  regre <- rbind(regre[1,]*252, regre[2,])
  regre <- rbind(regre, r_squared = t( apply(x[,2:12], MARGIN = 2, FUN = function(y) summary(lm(y ~ x2))$r.squared) ) )
  data.frame(regre, row.names = c("Alpha","Beta","R2"))
}

Sharpe <- function(x, benchmark_name, TLR = NULL){
  rend <- rendimiento(x)[1,]
  
  if(!is.null(TLR) ){
    TLR <- TLR/252
    Sharpe <- data.frame(row.names = "Sharpe", (rend[1,] - TLR) / rend[2,])
    
  }else if(!is.null(benchmark_name )){
    MAR <- rendimiento(data.frame(Date = x[,1], Bench = x[, benchmark_name]))[1,]
    
    x2 <- crecimiento(x)
    
    data.frame( (rend - MAR) / (apply( x2[,benchmark_name] - x2[,2:ncol(x)], 2 , sd) * sqrt(252) ), row.names = "Sharpe")
  }
}

Sortino <- function(x, benchmark_name = "DJI.Close"){
  
  x1 <- log( x[-1,2:ncol(x)] / x[-nrow(x),2:ncol(x)])
  x2 <- x1[, benchmark_name]
  
  downdev <- apply(x1, 2, function(z) sd( ifelse(z < x2, z - x2, 0)) * sqrt(252))
  
  rend <- rendimiento(x)[1,]
  MAR <- rendimiento(data.frame(Date = x[,1], Bench = x[, benchmark_name]))[1,]
  
  data.frame( (rend - MAR) / downdev, row.names = "Sortino")

}

Ratios <- function(x, benchmark_name, TLR, ndecil = 3){
  require(scales, quietly = TRUE)
  
  rend <- rendimiento(x)
  geo <- rendimiento_geometrico(x)
  regre <- regresion(x, benchmark_name)
  sharp <- Sharpe(df2, benchmark_name)
  sorti <- Sortino(df2, benchmark_name)
  
  round(rbind(rend, geo,regre, sharp, sorti), ndecil)
  
}
