# cacheMatrix.R

# makeCacheMatrix returns a list of functions for use with cacheSolve
#
# $set			set the value of the matrix
# $get			get the value of the matrix
# $setInverse  	set the value of the inverse of the matrix
# $getInverse	get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(x1) {
		x <<- x1
		inv <<- NULL # Blow the cache
	}
	get <- function() { x }
	setInverse <- function(inv1) { inv <<- inv1 }
	getInverse <- function() { inv }
	list(set = set,
		 get = get,
		 setInverse = setInverse,
		 getInverse = getInverse)

}


# cacheSolve(aCacheMatrix) calculates and caches the mean of aCacheMatrix,
# which is constructed using makeCacheMatrix.
# If there is a cached inverse (aCacheMatri$getInverse() not NULL) returns it.
# If there is a cache miss,
# 	gets the matrix to be solved using aCacheMatrix$get(),
#	solves the matrix using solve { base },
#   caches the inverse using aCacheMatrix$setInverse(inv).

cacheSolve <- function(aCacheMatrix, ...) {
	i <- aCacheMatrix$getInverse()
	if(is.null(i)) {
		m <- aCacheMatrix$get()
		i <- solve(m, ...)
		aCacheMatrix$setInverse(i)
	} else {
		message("Using cached data")
	}
	i
}
