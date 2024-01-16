
#Creo la matriz
A <- matrix(c(0.26,0.26,0.26,0.26,0.26,0.26,0.26,0.26,0.26,
              0.47,0.47,0.47,0.47,0.47,0.47,0.47,0.47,0.47,
              0.53,0.53,0.53,0.53,0.53,0.53,0.53,0.53,0.53,
              0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,
              0.16,0.16,0.16,0.16,0.16,0.16,0.16,0.16,0.16,
              0.97,0.97,0.97,0.97,0.97,0.97,0.97,0.97,0.97,
              0.18,0.18,0.18,0.18,0.18,0.18,0.18,0.18,0.18,
              0.62,0.62,0.62,0.62,0.62,0.62,0.62,0.62,0.62,
              0.32,0.32,0.32,0.32,0.32,0.32,0.32,0.32,0.32,
              0.48,0.48,0.48,0.48,0.48,0.48,0.48,0.48,0.48), nrow=10, ncol=9,
            byrow  = TRUE)
#Calculo Rango de la matriz
A
qr(A)
qr(A)$rank
#Calculo de número de matriz completa, matriz descompuesta y ahorro de números

#El número de elementos de la matriz completa es igual al número de filas por 
#el número de columnas

A
num_Mcompleta = 10*9
num_Mcompleta

#El número de elementos de la matriz descompuesta. Expresando la matriz A como
#un producto de un vector columna por un vector fila, en nuestro caso sería:

v1 <- matrix(c(0.26,0.47,0.53,0.25,0.16,0.97,0.18,0.62,0.32,0.48)
              , nrow=10, ncol=1,byrow  = TRUE)
v2 <- matrix(c(1,1,1,1,1,1,1,1,1), nrow=1, ncol=9, byrow = TRUE)
v1
v2
v1%*%v2 #Observamos que es igual a la matriz A

#Para transimitir la información del contenido en estos dos vectores solo 
#necesitamos los elementos de V1 + los elementos de V2. 

V1 = 10
v2 = 9
v1+v2 = 19 #Si v1 + v2 = A, y A tiene 90 elementos, 90 - 19 = 71, luego nos
           #hemos ahorrado 71 elementos tras la descomposición.

#---------------------

B = matrix(c(0, (7*(sqrt(2))/2), 0, 0, 0,
             0, 0, 0, 0, -(343*(sqrt(2))/2),
             ((sqrt(2))/2), 0, 0, 0, 0,
             0, 0,(2401*(sqrt(2))/2), 0, 0,
             0, 0, 0, (49*(sqrt(2))/2), 0, 
             0, 0, 0, (49*(sqrt(2))/2), 0,
             ((sqrt(2))/2), 0, 0, 0, 0,
             0, 0,(2401*(sqrt(2))/2), 0, 0,
             0, 0, 0, 0, (343*(sqrt(2))/2),
             0, -(7*(sqrt(2))/2), 0, 0, 0), nrow=10, ncol=5, byrow=TRUE)

#Calcula una norma matricial de x usando LAPACK. La norma puede ser la norma 
#del uno ( "O" ), la norma del infinito ( "I" ), la norma de Frobenius ( "F" ), 
#el módulo máximo ( "M" ) entre los elementos de una matriz, o la norma 
#“espectral” o "2" -norma, según lo determinado por el valor de type 

#norm(x, type = c("O", "I", "F", "M", "2"))
#Calculamos la norma 2 de B
B
norm(B, type = '2')
norm(B, type = 'O')
norm(B, type = 'I')
norm(B, type = 'F')
norm(B, type = 'M')

#Calculamos la descomposición en valores singulares la matriz

B2 <- matrix(c(0, (7*(sqrt(2))/2), 0, 0, 0,
         0, 0, 0, 0, -(343*(sqrt(2))/2),
         ((sqrt(2))/2), 0, 0, 0, 0,
         0, 0,(2401*(sqrt(2))/2), 0, 0,
         0, 0, 0, (49*(sqrt(2))/2), 0), nrow=5, ncol=5, byrow=TRUE)
       
       
svd(B)$d
svd(B2)$d

res <- svd(B)$d - svd(B2)$d
res
norm(res, type = '2')


