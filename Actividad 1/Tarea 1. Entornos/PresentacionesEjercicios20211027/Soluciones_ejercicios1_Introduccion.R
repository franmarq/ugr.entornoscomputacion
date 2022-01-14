# 1.Crear un vector v.1 que contenga los numeros impares entre 10 and 50. 
v.1<-seq(11,50,by=2)
# Calcular la suma de v.1.
sum(v.1)

# 2.Mostrar los cuatro primeros elementos del vector v.1. 
v.1[1:4]
# Calcular el producto de dichos elementos. 
prod(v.1[1:4])
# Repetir ahora con los elementos en posición 4, 9 y 11 del mismo vector v.1.
v.1[c(4,9,11)]
prod(v.1[c(4,9,11)])

# 3.Crear un vector v.2 con 100 numeros aleatorios de una uniforme continua en el intervalo [-3,3]. 
v.2<-runif(n=100,min=-3,max=3)
# Calcular la media y la varianza de dicho vector. 
mean(v.2)
var(v.2) # cuasivarianza
# Crear un vector s.v.2 que contenga los valores tipificados de v.2.
s.v.2<- (v.2-mean(v.2))/sd(v.2)
# Tambien se puede usar la funcion scale()
# s.v.2 <- scale(v.2)

# 4.Crear un vector c.v.2 con las sumas acumuladas de v.2
c.v.2<-cumsum(v.2)

# 5.Calcular el número de elementos positivos v.2 y almacenarlos en un nuevo vector v.3
sum(v.2>=0)
v.3<-v.2[v.2>0]

# 6.Reemplazar los valores negativos en v.2 con ceros.
v.2[v.2<0]<-0

# 7.Crear un vector v.4 con 100 números aleatorios de una normal con media 1 y varianza 0.25.
v.4<-rnorm(n=100,mean=1,sd=0.5)
# Representa la densidad de estos valores con plot(density(v.4))
plot(density(v.4))

# 8.Crear un vector v.5 con 100 números aleatorios de una gamma con media 1500 y varianza 100.
v.5<-rgamma(n=100,shape=22500,scale=1/15)
# Consultando help(rgamma) leemos que en un gamma con parámetros (shape=a y scale=s):
# la media es E(X) = a*s y la varianza es Var(X) = a*(s^2).
# Igualando E(X)=1500 y Var(X)=100 y resolviendo el sistema se deduce que s= 1/15 y a=22500

# Representa la densidad de estos valores
plot(density(v.5))
