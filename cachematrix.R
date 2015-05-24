## R Programming Week 3 Homework: 

## Creates a special 'matrix' that can cache its inverse, per the assignment
## In reality, it actually returns a list of the results of some functions that have been called on a matrix.

makeCacheMatrix <- function(x = matrix()) {
    c <- NULL
    set <- function(y) {
        x <<- y
        c <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) c <<- inverse
    getInverse <- function() c
    # All of the above commands are typical setters and getters
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
    # This returns a list of the results of the above setter and getter functions
}


## Works with the special matrix created by makeCacheMatrix, 
## returning a cached value of the inverse matrix if a cached value is available.
## Otherwise, calculates the inverse and returns it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    c <- x$getInverse()
    if(!is.null(c)) { #Checks to see if the inverse exists
        message("getting cached inverse")
        return(c) #Returns the cached value
    }
    #This code only gets executed if the cached value did not previously exist
    data <- x$get()
    c <- solve(data, ...)
    x$setInverse(c)
    c
}
