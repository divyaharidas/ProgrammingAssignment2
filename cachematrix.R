## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation 
## and there may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly

## This R program creates a pair of functions that cache the inverse of a matrix

## makeCacheMatrix: 
## This function creates a special "matrix" object that can cache its inverse.
## Function Arguments: An invertible matrix called 'x'
## Return Value : A list containing functions that will 
## (i) set the matrix, (ii) get the matrix, (iii) set the inverse & (iv) g et the inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<-y
		inv <<- NULL
	}
	get <-function()x
	setinverse <- function(inverse)inv <<- inverse
	getinverse <- function()inv
	list(set = set, get = get, 
	     setinverse = setinverse, getinverse=getinverse)
}


## cacheSolve: 
## This function computes the inverse of the special "matrix" created by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then it should retrieve the inverse from the cache.
## Function Argument: Output of makeCacheMatrix() called as 'x'
## Return Value: Inverse of the Matrix x 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
   	matx <- x$get()
	inv <- solve(matx, ...)
	x$setinverse(inv)
	return(inv)
}
