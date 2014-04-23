## Functions in this R file provide a mechanism to cache matrix inverse.
## Please read the comments before each function to know its purpose.
##
## USAGE1:
##   special <- makeCacheMatrix()
##   special$set(myMatrix)
##   inv <- cacheSolve(special)
##
## USAGE2:
##   special <- makeCacheMatrix(myMatrix)
##   inv <- cacheSolve(special)


## Purpose:
##   Creates a 'special' vector to help caching the matix inverse
## Input:
##   x = optional matrix to store inside for evaluations
##       Can also be 'set' later using the output 'special' vector
## Output:
##   A 'special' vector containing following functions:
##    set        -  set the value of the matrix
##    get        - get the matrix
##    setInverse - set the matrix inverse
##    getInverse - get the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
    cachedInv <- NULL
    set <- function(y) {
        x <<- y
        cachedInv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) cachedInv <<- inverse
    getInverse <- function() cachedInv
    list(set        = set,
         get        = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Purpose:
##   This function takes the 'special' vector created by 'makeCacheMatrix'.
##   It will check if its inverse is already evaluated and if yes returns
##   that cached value, else evaluates the same, caches it and returns the
##   value
## Input:
##   x = the 'special' vector returned by 'makeCacheMatrix'
##   ... = other arguments to be passed to the 'solve' function
## Output:
##   matrix inverse
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}
