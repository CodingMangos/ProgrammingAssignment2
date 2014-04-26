## These two functions are almost identical to the example given in the
## assignment details. The few differences are accepting a matrix for
## the argument rather than a vector, and using the solve() function
## to get the inverse of the matrix rather than finding the mean. I also
## added an if statement to check if the matrix being set in makeCacheMatrix
## is equal to what is already there, so as to avoid having to compute its
## inverse again.

## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    ## the "i" variable will hold the inverted matrix, but set to NULL as default
    i <- NULL
    ## the "set" function accepts a matrix and stores it in the x variable and resets the "i" variable to NULL (both of which are stored outside of this function's environment)
    set <- function(y) {
        ## if the user tries to pass a matrix that is identical to the one already stored in this list, then the inverse is the same and thus should not need to be recalculated
        if (!identical(x, y)) {
            x <<- y
            i <<- NULL
        }
    }
    # the "get" function returns the value of x
    get <- function() x
    # the "setInverse" function stores the variable passed to it (called "inverse") to the variable "i". Since "i" is not in this function's environemnt, it uses the << operator to assign its value
    setInverse <- function(inverse) i <<- inverse
    # the "getInverse" function returns the value stored in the "i" variable
    getInverse <- function() i
    # the function returns a list containing the four previously defined functions, with their names assigned to each one so they can be accesse with "$get"
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Computes the inverse of the special matrix (returned by makeCacheMatrix)
## if inverse has already been calculated (and matrix has not changed) then
## it will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    # the function treats x (a list created using the makeCacheMatrix) to retrieve the value of matrix's inverse and store it in the "i" variable
    i <- x$getInverse()
    # if the inverse of the matrix has not been computed yet, "i" will contain NULL and thus skip to the next step. Otherwise, it will return the matrix's inverse
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    # the "data" variable retrieves the matrix stored in x
    data <- x$get()
    # the solve() function is passed the matrix stored in data as the only argument, causing it to compute its inverse and store it in "i"
    i <- solve(data, ...)
    # "i" is passed to the "setInverse" function stored in x, which will set the value of x's inverted matrix to "i"
    x$setInverse(i)
    # the function returns "i" (the inverted matrix)
    i
}
