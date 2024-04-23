#### Função para criar a matriz ####
criar_matriz <- function(l, c){
  matriz <- matrix(1:(l*c), nrow = l, ncol = c)
  for(i in 1:l){
    vl <- as.vector(strsplit(readline(paste("matriz[", i, ",]: ", sep="")), ";"))
    vl <- as.numeric(vl[[1]][1:3])
    matriz[i,] <- vl
  }
  return(matriz)
}

#### Retro-substituição ####
# Para matriz nxn triangular(superior)
# Ax = b
mts <- function(A, b){
  n = ncol(A)
  x <- rep(0, n)
  x[n] = b[n]/A[n,][n]
  print(paste("x", n, " = ", x[n], sep=""))
  for(i in (n-1):1){
    x[i] = (b[i] - sum(A[i,][n:1]*x[n:1]))/A[i,][i]
    print(paste("x", i, " = ", x[i], sep=""))
  }
}

#Exemplo 1
matriz <- criar_matriz(3, 3)
3;2;4
0;0.3333333;0.6666667
0;0;-8
b <- c(1, 5/3, 0)
mts(matriz, b)

#Exemplo 2
matriz <- criar_matriz(3, 3)
1;2;3
0;1;2
0;0;1
b <- c(1, 2, 3)
mts(matriz, b)
