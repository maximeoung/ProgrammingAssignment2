## function makeCacheMatrix will store a matrix in cache
makeCacheMatrix <- function(x = matrix()) {
	## initialize inverse matrix to NULL
	i <- NULL
	
	## set matrix function definition
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	
	## get matrix function definition
	get <- function() x

	## set inverse matrix function definition
	setinverse <- function(inverse) i <<- inverse
	
	## get inverse matrix function definition
	getinverse <- function() i

	## returns list of 'set', 'get', 'setinverse', 'getinverse'
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}

## function 'cacheSolve', when provided a cached matrix created with function 'makeCacheMatrix', will calculate the inverse of the matrix, if it hasn't been calculated already.
## Else, it will get the inverse matrix that has already been calculated.
## We assume the matrix is always invertible
cacheSolve <- function(x) {
	## get the 'getinverse' method of cached matrix
	i <- x$getinverse()
	
	## if already calculated, prompt a message 'getting cached data' and returns cached inverse matrix
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	
	## else we calculate the inverse matrix
	matrix <- x$get()
	i <- solve(matrix)
	x$setinverse(i)
	i
}


#### verifying functions :
#### initialize square matrix  of random integers
## random_matrix <- matrix(round(runif(n=100, min=0, max=9),0), nrow=10)

#### store matrix in cache
## cache_matrix<-makeCacheMatrix(random_matrix)

#### call inverse function on cache matrix
## cacheSolve(cache_matrix)

#### Calling twice to see if message is prompted
## cacheSolve(cache_matrix)
