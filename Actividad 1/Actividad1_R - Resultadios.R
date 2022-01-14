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

## Ejercicio 2

## Funcion para dif.eq



### Funcion para dif.eq2