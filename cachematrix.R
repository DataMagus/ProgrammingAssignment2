## Programming Assignment 2 for R Programming Class

## Write a short comment describing this function
## Creates a CacheMatrix object
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL # no inverse    
    set <- function(y) { 
        x <<- y
        inv <<- NULL
    }
    get <- function() x  # gets the matrix stored in the matrix object
    setinv <- function (inverse) inv<<- inverse  # function to store inverse
    getinv <- function() inv  # function to get inverse
    list(set = set, get = get, 
         setinv = setinv,
         getinv = getinv)    # list of functions
}



## Calculates and returns the inverse matrix if not cached
## Returns the cashed inverse if available to reduce computation
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()  # gets the inverse
    if (!is.null(inv)) {  # checks if it is cached
        message("getting cached data")
        return(inv)
    }
    data <- x$get()  # get the matrix
    inv<- solve(data,...)  # calculate the inverse
    x$setinv(inv)  # store it in the corresponding matrix object
    inv  # returned value
}
