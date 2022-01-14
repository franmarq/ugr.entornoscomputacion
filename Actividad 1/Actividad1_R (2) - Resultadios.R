#Actividad 1

## Ejercicio 1
quiebra = function (capital,prob_cara) {
  p=prob_cara
  q=1-p
  vector <- c(capital)
  while (capital>0) {
    Resultado <- sample(c(-1,1), size=1, prob=c(p,q))
    capital <- c(capital+Resultado)
    vector <- append(vector, c(capital))
  }
  plot(ts(vector),xlab='Ensayos',ylab='Capital')
  abline(h=50,col='black',lty=2)
  length(vector) 
}

quiebra(20,0.5)

## Ejercicio 2. Ejercicio 2. Crear una función con nombre "dif.eq" que devuelva el vector (x1,x2,.,xn) 
## donde xn = r*xn-1*(1-xn-1)

###2.a

dif.eq <- function(x1,r,n) {

  vector2<- c(x1)
  
  for (i in 2:n) {
    calc <- r*vector2[i-1]*(1-vector2[i-1])
    vector2<- append(vector2,c(calc)) 
  }  
  plot(ts(vector2),xlab='Iteraccion',ylab='Resultado')
  abline(col='black')
  length(vector2) 
}



dif.eq(0.95,2,10)
dif.eq(0.95,2.99,500)


###2.b

dif.eq2 <- function(x1,r) {
  
  vector3<- c(x1)
  i=1

  repeat {
  i<- i+1
  calc2 <- r*vector3[i-1]*(1-vector3[i-1])
  vector3<- append(vector3,c(calc2)) 
  a<- vector3[i-1]
  b<- vector3[i]
#  print(i)
#  print(vector3)
#  print(a)
#  print(b)
  if (abs(b-a) < 0.02 ) {
    print(i-1)
    break
  }
  }
}


dif.eq2(0.95,2.99)


### Ejercicio 3. 

### 3.a

### El conjunto de datos cars, contenido en el libro datasets de R, recoge información acerca 
### de la velocidad de los coches (variable speed) y distancias necesarias para parar (variable 
### dist). Utilizando dichos datos realizar un análisis descriptivo básico de las variables 
### contenidas en él, ofreciendo la siguiente información:

dim(cars) #dimensiones

str(cars) #estrauctura

head(cars) #Primeros registros



### Descripcion mas detallada

install.packages('Hmisc')

library(Hmisc)

describe(cars)


### medidas de posición central, dispersión, asimetría y curtosis

library(dplyr)
install.packages('modeest')
library(modeest)   # Para hallar la moda
install.packages('moments')
library(moments)   # Para hallar las medidas de forma


### posición central

summary(cars)   # posicion, media, madiana
mfv(cars$speed) # Moda
mfv(cars$dist)  # Moda

### dispersión

var(cars$speed) # Varianza
sd(cars$speed)  # Desviacion estandar
#CV(cars$speed)

### asimetría y curtosis
skewness(cars$speed)    # Sesgo
kurtosis(cars$speed)    # Curtosis


### Graficos

install.packages('GGally')
library(GGally)
ggpairs(cars)
#ggplot(cars,aes(dist))+geom_histogram()

par(mfrow = c(2, 2))
hist(cars$speed, col = "orange")
hist(cars$dist, col = "green")
boxplot(cars$speed, col = "orange",xlab = "Data:Cars$Speed",ylab = "Speed")  
boxplot(cars$dist, col = "green",xlab = "Data:Cars$Dist",ylab = "Distance")  

par(mfrow = c(1, 1))




### 3.b

attach(cars)

Modelo<- lm(dist~speed)
summary(Modelo)


plot(speed,dist,
     xlab = "Speed",ylab="Distance") 
abline(Modelo, col="red")



### Ejercicio 4
### Consideremos los siguientes datos del calor latente en la fusión del hielo expresados 
### en cal/gm:
### Muestra A: 79.98 80.04 80.02 80.04 80.03 80.03 80.04 79.97 80.05 80.03 80.02 80.00 80.02
### Muestra B: 80.02 79.94 79.98 79.97 79.97 80.03 79.95 79.97
### a) Construir un gráfico de cajas y un gráfico cuantil-cuantil para comparar la distribución de cada muestra.
### b) Contrastar la igualdad de varianzas de las distribuciones de dichas muestras.
### c) Contrastar la igualdad de medias de las distribuciones de dichas muestras.


### construccion de los datos

Muestraa<- c(79.98,80.04,80.02,80.04,80.03,80.03,80.04,79.97,80.05,80.03,80.02,80.00,80.02)
Muestrab<- c(80.02,79.94,79.98,79.97,79.97,80.03,79.95,79.97)

max.len = max(length(Muestraa), length(Muestrab))

Muestrab = c(Muestrab, rep(NA, max.len - length(Muestrab)))

head(Muestrab)
dim(Muestraa)
dim(Muestrab)



ejer3.data<- data.frame(Muestraa,Muestrab)

head(ejer3.data,13)
dim(ejer3.data)

### 4.a
par(mfrow=c(1,2))
boxplot(ejer3.data)

#### se imputan valores NA con la media

ejer3.data$Muestrab [ is.na (ejer3.data$Muestrab)] <- mean (ejer3.data$Muestrab, na.rm = TRUE )
head(ejer3.data,13)

#### data[complete.cases(ejer3.data)]

qqplot(ejer3.data$Muestraa,ejer3.data$Muestrab)

### 4.b Contrastar la igualdad de varianzas de las distribuciones de dichas muestras.

var.test(ejer3.data$Muestraa, ejer3.data$Muestrab)

### 4.c Contrastar la igualdad de medias de las distribuciones de dichas muestras.

t.test(ejer3.data$Muestraa, ejer3.data$Muestrab)



### Ejercicio 5. Los siguientes datos corresponden a un estudio sobre el consumo de combustible 
### de cuatro fabricantes diferentes de vehículos de motor 1600cc.
### [...]  
### En la tabla se recogen datos de consumo de combustible (litros/100km) para muestras independientes 
### de 9 vehículos nuevos de cada fabricantes, después de recorrer la misma ruta en idénticas condiciones y con el mismo conductor.  


### Datos Fabricante Consumo (litros/100km)

A<- c(9.3
,8.9
,8.7
,9.1
,8.6
,9.1
,9.6
,9.7
,9.0)

B<- c(9.1
,8.3
,8.2
,9.0
,9.1
,8.3
,8.8
,8.0
,9.0)

C<- c(9.8
,10.4
,9.2
,9.7
,10.4
,8.9
,8.8
,9.7
,9.5)

D<- c(8.6
,8.9
,8.1
,7.8
,9.1
,8.0
,8.4
,8.1
,8.8)

ejer5.data<- data.frame(A,B,C,D)
head(ejer5.data,9)


### Data para ejecuci[on de los test
ejer5.data2<- stack(ejer5.data)
head(ejer5.data2)


### a) Construir un gráfico exploratorio de cajas que permita la comparación de los cuatro fabricantes.
par(mfrow = c(1, 1))
boxplot(ejer5.data, col = ncol(ejer5.data))

### b) Realizar un contraste de hipótesis apropiado que permita concluir si hay o no diferencia significativa entre los fabricantes respecto al consumo de combustible.
two.way <- aov(values ~ ind, data = ejer5.data2)

summary(two.way)

### c) Justificar las hipótesis asumidas en el contraste de hipótesis realizado en el apartado b).  






#### Borrar

data("iris")
iris <- filter(.data = iris, Species %in% c("versicolor", "virginica"))
head(iris)



