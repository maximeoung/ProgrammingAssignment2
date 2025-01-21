makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x) {
	## Return a matrix that is the inverse of 'x'
	i <- x$getinverse()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	matrix <- x$get()
	i <- solve(matrix)
	x$setinverse(i)
	i
}

## verifying functions
## initialize square matrix  of random integers
random_matrix <- matrix(round(runif(n=100, min=0, max=9),0), nrow=10)

## store matrix in cache
cache_matrix<-makeCacheMatrix(random_matrix)

## call inverse function on cache matrix
cacheSolve(cache_matrix)

## Calling twice to see if message is prompted
cacheSolve(cache_matrix)