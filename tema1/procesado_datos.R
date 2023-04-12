



## Nos hace un listado de archivos
## Especificamos full.names=T para que nos de la ruta
directorio <- "./tema1/datos/"
archivo <- list.files(directorio, full.names = T)
## leemos las lineas del archivo
predata <- readLines(archivo)
## obtenemos un objeto de la clase character
## obtenemos los datos y los convertimos a una matriz
## el signo negativo nos dice que no tome esos indices
## grep trabaja con expresiones regulares
predatos <- predata[-grep("@", predata)]
## convertimos a matriz para extraer el nombre
columnas <- t(matrix(
  unlist(strsplit(predata[grep("@attribute",
                               predata)], " ")),
  ncol = length(predata[grep("@attribute", predata)]),
  byrow = F
))[, 2]

datos <-
  as.data.frame(matrix(
    unlist(strsplit(predatos, ",")),
    ncol = length(columnas),
    byrow = T
  ))
colnames(datos) <- columnas

## hacemos un sumario de los datos
summary(datos)

## convertimos los datos a las clases correspondientes
str(datos)
colnames(datos)
datos[, c(1, 5:ncol(datos))] <-
  lapply(datos[, c(1, 5:ncol(datos))], as.factor)
datos[, 2:4] <- lapply(datos[, 2:4], as.numeric)

str(datos)

## Comparaciones dos a dos entre categoricas

## El chisq.test
chisq.test(table(datos$Gender,
                 datos$family_history_with_overweight))

chisq.csa <- chisq.test(table(datos$Gender,
                              datos$family_history_with_overweight))

chisq.csa$p.value

columnas.factores <- colnames(datos)[unlist(lapply(datos,
                                                   is.factor))]
## realizamos tantas combinaciones como se permiten
combinaciones <- as.data.frame(t(combn(columnas.factores, 2)))

combinaciones$p.value <- 1

datos.factoriales <- datos[, columnas.factores]

combinaciones$menor5 <- NA

## iteramos sobre cada variable factorial
## si es incorrecta la aproximacion, tener cuidado

for (i in 1:nrow(combinaciones)) {
    test.object <- chisq.test(table(datos[,combinaciones[i,1]],
                                                datos[,combinaciones[i,2]]))
    if(mean(test.object$observed)<5){
      
      combinaciones$menor5[i] <- mean(test.object$observed)
      
    }else{
      combinaciones$menor5[i] <- NA
    }
    combinaciones$p.value[i] <- test.object$p.value
    
}

combinaciones$p.adj <- p.adjust(combinaciones$p.value,"BH")

library(data.table)
library(dplyr)
datos.dt <- as.data.table(datos)

datos.dt %>% group_by(Gender,SMOKE) %>% sunnarise(n=n())


csa <- datos %>% group_by(Gender,NObeyesdad) %>% group_by(n=n()) 

datos$obesidadsino <- (as.factor(grepl("obesity",ignore.case = T,datos$NObeyesdad)))


datos$obecidadsino 

datos$CH20 <- as.numeric(datos$CH20)
### Haced una tabla con la n de cada subgrupo formado
### y el p valor asociado y los valores esperados medios

### ¿La obesidad esta asociada al genero?

obesidad <- datos$NObeyesdad
genero <- datos$Gender

tabla <- (table(obesidad,genero))

###ji cuadrado No esta asociado el genero y la obesidad?
### 

chisq.test(tabla)

### el genero no esta relacionado con la historia familiar de la obesidad?

historia <- datos$family_history_with_overweight
genero <- datos$Gender

tablah <- (table(historia,genero))

chisq.test(tablah)


### ¿Cuales comen calorico pero no tienen obesidad? ¿
### De los que fuman, y no tienen obesidad, tienen actividad fisica
### en este paso habra preguntas, pregunten !
### beben alcohol y caminan ?

### Si puedes calcula el p valor y de estas comparaciones, 
### y cuanta es la diferencia en el numero de observaciones por variable

### De forma similar a lo anterior
### Realiza t.test tantos como sean necesarios entre las variables numericas
### y las categoricas cuando hay solo dos niveles
###   que sucede cuando hay más de un nivel ?




