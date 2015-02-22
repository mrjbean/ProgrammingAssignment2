## Creates a matrix that can cache its inverse

## Function takes a matrix as an argument
makeCacheMatrix <- function(x = matrix()) {
        # initializes an empty variable, i
        i <- NULL 
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        # Creates a subfunction, get, that returns the value of x
        get <- function() x
        # Creates a subfunction, setinverse, which takes the inverse of the
        # matrix as an argument and caches the inverse of the matrix to 
        # i
        setinverse <- function(inverse) i <<- inverse 
        # Creates a subfunction, getinverse, which returns the value of i
        getinverse <- function() i
        # Creates a list that stores the values of set, get, setinverse,
        # and getinverse along with a name for retrieval
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function caclulates the inverse of the matrix from
## makeCacheMatrix. If the inverse has already been calculated,
## then cacheSolve will retrieve the inverse matrix from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # Calls the getinverse sub-function above and assigns to variable i
        i <- x$getinverse()
        # Initiates an if/else loop that checks if the value of i is NULL
        # If i != NULL, prints message and returns the value of i from cache
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        # Calls the get() sub-function above and assigns the value to 
        # the variable data
        data <- x$get()
        # Uses the built in solve function to create the inverse of data
        # and assigns the value to the variable i
        i <- solve(data, ...)
        # Calls setinverse sub-function above to cache the value of i
        x$setinverse(i)
        # Returns the value of the variable i
        i
}
