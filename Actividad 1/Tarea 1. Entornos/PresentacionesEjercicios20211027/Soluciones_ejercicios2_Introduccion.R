# 1. Crear una matriz cuadrada de dimension tres que tenga los elementos 
#    (por filas) 1,1/2,...,1/9. Denominar dicha matriz como A.
A<-matrix(1/(1:9),ncol=3,byrow=T)

# 2. Calcular la traspuesta de A y multiplicar A por su traspuesta.
A%*%t(A)

# 3. Crear una matriz B como A pero eliminando la primera columna.
B<-A[,-1]

# 4. Crear una matriz C como A pero acumulando los valores de A por filas.
C<-rbind(cumsum(A[1,]),cumsum(A[2,]),cumsum(A[3,]))

# 5. Crear una matriz cuadrada D de dimension cuatro. El cuadrante superior izquierdo de 
#    dimension tres debe ser la matriz A, y la ultima fila y la columna deben ser vectores de ceros
D<-rbind(cbind(A,0),0)


# 6. Crear una matriz E de dimension cuatro por cinco. Debe contener D en las cuatro primeras 
#    columnas y las medias por filas de D en ultima columna
E<-cbinc(D,rowMeans(D))

# 7. Convertir la matriz A en un data.frame (con nombre DA1) añadiendo nombres a las columnas
#    (por ejemplo "A1", "A2" y "A3"). Escribir dicho data.frame en un fichero de texto. 
#    Una vez hecho esto, leer el fichero y  cargar su contenido en un nuevo data.frame 
#    (con nombre DA2) usando una funcion adecuada. Despues visualizar DA2 y ver que es igual que DA1.
DA<-data.frame(A)
names(DA)<-c('A1','A2','A3')
write.table(DA,file='DA.txt',row.names=FALSE)
DA2<-read.table('DA.txt',header=TRUE)

# 8. Cargar el objeto de datos volcano (del package datasets), mostrarlo y calcular un resumen 
#    descriptivo usando la funcion summary().
library(datasets)
data(volcano)
summary(volcano)

# 9. Cargar el objeto de datos Nile (package datasets), mostrarlo y representarlo usando la funcion plot().
data(Nile)
plot(Nile)
