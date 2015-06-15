## Put comments here that give an overall description of what your
## functions do

## Create the data structure that can be used to cache the inverse
## of a given matrix
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(m) {
		x<<-m
		inv<<-NULL
	}
	get <- function() x
	
	#Cache the inverse matrix
	setInverse <- function(i) inv<<- i
	
	#Get the cached inverse matrix
	#Return null if the inverse matrix has not been cached yet!
	getInverse <- function() inv
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Calculate the inverse of a matrix faster by caching and returning the cached result.
cacheSolve <- function(x, ...) {
	inv <- x$getInverse()
	if (!is.null(inv)) {
		message("getting cached inversed matrix")
		return (inv)
	}
	m<-x$get()
	#actual calculation of the inverse
	inv<-pinv(m)
	x$setInverse(inv)
	inv
}
