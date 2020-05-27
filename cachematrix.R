## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates the Matrix Object that will cache its inverse
## cacheSolve will return the inverse of the matrix

## Write a short comment describing this function

## This function creates 4 methods that will allow to manipulate
## the Matrix Object, setting or getting a matrix from it, 
## and setting or getting its inverse
##
## The set method invalidates the inverse, so that a cached result
## will never be old and wrong.

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(mat) {
		x <<- mat
		inverse <<- NULL
	}
	
	get <- function() x
	
	setinv <- function(inv) inverse <<- inv
	getinv <- function() inverse

	list(set = set, get = get,
		setinv = setinv,
		getinv = getinv)
}


## Write a short comment describing this function

## This will return the inverse of a matrix stored in the Matrix Object
## created with makeCacheMatrix
## 
## This inverse will either be cached (so no computation is required)
## or it will be computed if no cached version is available

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
	if(!is.null(inv)) {
		return(inv)
	}

	data <- x$get()
	inv <- solve(data)
	x$setinv(inv)
	inv
}









