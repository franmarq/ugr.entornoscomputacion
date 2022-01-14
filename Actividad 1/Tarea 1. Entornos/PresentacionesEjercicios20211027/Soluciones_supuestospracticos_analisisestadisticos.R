# Supuesto práctico 1. 
# El fichero de datos "poblacion.txt" recoge información
# básica demográfica y de consumo de una muestra ficticia de 6400 individuos. 
# Se pide:
# 
#   1. Leer el fichero de datos y almacenar su contenido en un data.frame con 
# nombre "datos1".

datos1<-read.table("poblacion.txt",header=TRUE)

# 2. Definir etiquetas para los 5 niveles de la variable nivel educativo ("educ")
# como sigue: 1, "Bachillerato incompleto";  2 , "Bachillerato";  
# 3, "Universitarios parciales"; 4, "Universitarios"; 
# 5, "Post-universitarios". 

datos1$educ<-factor(datos1$educ, levels=1:5,
      labels=c('Bachillerato incompleto',
      'Bachillerato','Universitarios parciales',
      'Universitarios','Post-universitarios'))
 
# 3. Obtener una tabla de frecuencias de la variable "educ". La tabla será 
# un objeto de tipo data.frame con 5 columnas: modalidades de la variable, 
# frecuencia absoluta, frecuencia relativa, absoluta acumulada y 
# relativa acumulada.

ni<-table(datos1$educ) # devuelve un vector con las frecuencias absolutas
Ni<-cumsum(ni) # devuelve las frecuencias absolutas acumuladas
fi<-prop.table(ni) # devuelve las relativas
Fi<-cumsum(fi)
mi.tabla<-data.frame(ni=as.numeric(ni),Ni=as.numeric(Ni),fi=as.numeric(fi),Fi=as.numeric(Fi))
rownames(mi.tabla)<-names(ni)
mi.tabla

 
# 4. Representar gráficamente la variable "educ" mediante un diagrama de 
# barras y un diagrama de sectores. Ambos gráficos deben aparecen en la 
# misma ventana gráfica dividida en dos columnas y una fila 
# (usar para ello la función par() especificando por ejemplo mfrow(c(1,2)).

win.graph()
par(mfrow=c(2,1))
barplot(table(datos1$educ),main='Diagrama de barras',xlab='nivel educativo',
        ylab='frecuencia',col = rainbow(5))
pie(table(datos1$educ),main='Diagrama de sectores',col = rainbow(5))

# 5. Representar gráficamente la variable "edad" mediante un histograma y 
# un gráfico de cajas. El histograma deberá realizarse definiendo como puntos
# de corte los percentiles 10%, 20%,.,90% de la variable. Y de nuevo ambos 
# gráficos deben aparecer en una misma ventana gráfica.                                                                                                                                                                                                 Representar gráficamente la variable "edad" mediante un histograma y un gráfico de cajas. El histograma deberá realizarse definiendo como puntos de corte los percentiles 10%, 20%,.,90% de la variable. Y de nuevo ambos gráficos deben aparecer en una misma ventana gráfica.

p.corte<-quantile(datos1$edad,probs=(0:10)/10)
win.graph()
par(mfrow=c(2,1))
hist(datos1$edad,breaks=p.corte,xlab='Edad',ylab='Frecuencia',
     col='lightgray',main='Histograma')
boxplot(datos1$edad,xlab='',ylab='Edad',
        col='lightgray',main='Diagrama de cajas')

# Supuesto práctico 2. 
# Como resultado de una nueva política empresarial, se ha aumentado 
# progresivamente la inversión en formación de los empleados en una 
# multinacional de software. Se sospecha que este incremento en 
# inversión ha tenido gran importancia en los beneficios de la empresa. 
# En la tabla adjunta se recogen datos correspondientes a los gastos en 
# inversión (en millones de euros) y a los beneficios brutos de la empresa 
# (en millones de euros) en los últimos diez meses. Se pide:
# 1.Crear un objeto de tipo data.frame con nombre "datos2"
# con los datos de la tabla.

datos2<-data.frame(Inversion=c(0.2,0.1,0.7,0.8,1.1,1.3,2.4,2.9,3.5,3.9),
        Beneficios=c(25.3,26.7,31.4,33.5,39.7,40.6,45.5,56.8,75.4,97.2))

# 2. Representar un diagrama diagrama de dispersión de la variable inversión 
# (eje horizontal) frente a la variable beneficios (eje vertical).

plot(datos2$Inversion,datos2$Beneficios,pch=21,bg='lightgray',
     main='Diagrama de dispersión',xlab='Inversión',ylab='Beneficios')
 
# 3. Ajustar un modelo lineal a los datos que permita predecir la variable 
# beneficios en función de la variable inversión.
mi.lm<-lm(Beneficios~Inversion,data=datos2 )
mi.lm

# 4. Representar gráficamente el modelo ajustado ( superponiendo la recta de 
# regresión al diagrama de dispersión obtenido en el apartado 2).

ajuste<-mi.lm$coef[1]+mi.lm$coef[2]*datos2$Inversion
lines(datos2$Inversion,ajuste,lwd=2,col=4)
legend('topleft',legend=c('observaciones','ajuste lineal'),pch=c(21,NA),
       lty=c(NA,1),col=c(1,4),lwd=c(NA,2),bt='n')

# 5. Usando el modelo ajustado predecir los beneficios esperados para un mes 
# donde la inversión en formación es de 1.5 millones de euros.

mi.lm$coef[1]+mi.lm$coef[2]*1.5
