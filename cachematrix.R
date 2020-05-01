## Put comments here that give an overall description of what your
## functions do

## The point here is to write a pair of functions, namely, 
## "makeCacheMatrix" and "cacheSolve" that cache the inverse of a matrix

## Write a short comment describing this function
## makeCacheMatrix is a great functiion the will create a "matrix" object that can 
## cache its inverse for the input (which is an invertible square matrix)
##  In the following x creates x
##  x  in return gets get() function


makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    set <- function(y) {
    x <<- y
    inv <<- NULL
  }

    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
## cacheSolve is a function which computes the inverse of the "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached result")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

##nt <- matrix(rnorm(25),5,5)
##> m5 <- makeCacheMatrix(nt)
##> cacheSolve(m5)

##[,1]       [,2]        [,3]       [,4]       [,5]
##[1,]  0.6148723 -0.3109531 -0.01453515 -0.3828072 -0.3513001
##[2,] -0.2312961  0.5201504 -0.23230891 -0.2259652 -0.1591974
##[3,]  0.2649177  0.5239002  0.79776286  0.1184521 -0.6551267
##[4,]  0.1813507 -0.9060214  0.22754347 -0.4435704 -0.6165972
##[5,] -0.2862648 -0.3543201 -0.88990600  0.1396643 -0.1584269