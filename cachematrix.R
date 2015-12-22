## Takes a squared matrix and calculates it inverse only if this inverse matrix
## is not already cached

## makeCacheMatrix takes a matrix and gives a list with 
## 1)get: to get input  matrix, 2)setinv: to set the value of solve calculation 
## and 3)getinv: to get the value of the inverse matrix 

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL                           
  get <- function() x            
  setinv <- function(solving) s <<- solving  
  getinv <- function() s
  list( get = get,
        setinv = setinv,
        getinv = getinv)
}


## To return matrix inverse of "x" cacheSolve checks if getinv cache a matrix. 
## It it does it prints the matrix. Otherwise, it calculates the inverse of 
## the cached matrix.

cacheSolve <- function(x, ...) {
        z <- x$getinv() 
  if(!is.null(z)) { 
    message("getting cached data")
    return(z) 
  }
  data <- x$get()  
  z <- solve(data, ...)  
  x$setinv(z) 
  z
}
