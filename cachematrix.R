## The functions are used to save and retrive inverse of matrix so that the inverse is not calculated everytime.
## 

## The function makeCacheMatrix is used to store and retrive values (matrix-x and its inverse-inv in this case).
## The function inturn has functions set and get for matrix, setInverse and getInverse for inverse. These 4 functions are returned as list.
## The first step is to assign a matrix and store the object. eg. mymatrix = makeCacheMatrix(matrix)

makeCacheMatrix <- function(x = matrix()) {

	inv <<- NULL

	set <- function(y){
		x <<- y
		inv <<- NULL
	}

	get <- function() x
	setInverse <- function(inverse) inv <<- inverse
	getInverse <- function() inv

	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## The function cacheSolve checks if inverse is already calculated. If yes then the function returns the inverse value 
## else, it calculates inverse, stores and then returns the value 
## We need to pass the object we obtained above (makeCacheMatrix-mymatrix) as an argument to this function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getInverse()

	if(!is.null(inv)){
		message("Getting cached Data")
		return(inv)
	}

	m <- x$get()
	inv <- solve(m)
	x$setInverse(inv)
	inv
}
