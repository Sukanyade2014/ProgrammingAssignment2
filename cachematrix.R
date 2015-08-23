## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## MakeCacheMatrix function initializes an empty matrix.
##The Set function within the "makeCacheMatrix" function saves a matrix entered by the user
##The "setinverse" function inverts the matrix
## The "getinverse" function returns the valus of the matrix.


makeCacheMatrix <- function(x = matrix()) {
m <-NULL
	set<-function(y){
		x<<- y
		m<<-NULL
	}
	get <-function() x
	setinverse <-function(solve) m<<-solve
	getinverse <-function() m
	list(set=set , get = get, setinverse = setinverse, getinverse=getinverse)
	
}


## Write a short comment describing this function
##The cache solve function checks to see if we already have an inverse of the function, if yes, it returns the inverse from the cache or else it computes the inverse of the input matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

m <-x$getinverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	matrix <-x$get()
	m <-solve(matrix, ...)
	x$setinverse(m)
	m
}
