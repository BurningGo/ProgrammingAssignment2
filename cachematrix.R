## Put comments here that give an overall description of what your
## functions do

## Function : makeCacheMatrix
## Make a cacheMatrix object which stores inverse value of the matrix

makeCacheMatrix <- function(x = matrix()) {
	inverse_val <- NULL
    set <- function(y) {
		val <<- y
        inverse_val <<- NULL
    }
    get <- function() val
    
	setinverse <- function(mean) inverse_val <<- mean
    getinverse <- function() inverse_val
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
    
	if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
	data <- x$get()
	Iden <- diag(dim(data)[2])
    inv <- solve(data,Iden, ...)
    x$setinverse(inv)
    inv
}




