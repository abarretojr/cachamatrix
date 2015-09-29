## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
     inve <- NULL
     set <- function(y) {
          x <<- y
          inve <<- NULL
     }                                                    # Atribuir o valor a matrix
     
     get <- function() x                                  # Recuperar o valor da matrix
     setinverse <- function(inverse) inve <<- inverse     # Atribuir o valor inverso a matrix
     getinverse <- function() inve                        # Recuperar o valor inverso da matrix
     
     list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## Write a short comment describing this function
cacheSolve <- function(x, ...) {
     ## Devolver uma matriz que é o inverso da 'x'
     inve <- x$getinverse()
     if(!is.null(inve)) {
          message("Recebendo dados em cache.")
          return(inve)
     }                                          # Verifique se o inverso já foi calculado e obter o inverso do cache
     
     data <- x$get()
     inve <- solve(data)
     x$setinverse(inve)                         # Calcula o inverso da matriz e define o valor do inverso no cache
     inve
     
     ## Retornar uma matriz 'inve' que é o inverso de 'x'
}














