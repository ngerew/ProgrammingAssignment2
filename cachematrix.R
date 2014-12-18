## These two functions allow for matrix inverses to be cached and then retreived;
## this potentially saves on time-intensive computations.  The functions assume the
## provided matrix is in fact invertible.

## This first function creates a matrix object that stores a matrix and a cache of
## its inverse.  There are getter and setter methods for both the matrix and its
## inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL        ## Cached value of inverse, initially set to NULL when object is created
        set <- function(y) {                      ## Set the value of the matrix 
                x <<- y
                m <<- NULL
        }
        get <- function() { x }                   ## Get the value of the matrix
        setinverse <- function(inv) { m <<- inv } ## Set value of inverse
        getinverse <- function() { m }            ## Get value of inverse
        ## Returns the object, with the 4 methods defined above
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}

## This second function calcuates the inverse of a provided matrix object.  
## If the inverse has already been calculated and cached, the function returns
## that cached value.  Otherwise, the function calcuates the inverse and then
## caches that result for future use.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()   ## m is the (possibly) cached inverse
        if (!is.null(m)) {    ## if m not null, return that cached value
                message("Getting cached inverse")
                return(m)
        }
        data <- x$get()       ## if the cache is null, then get the matrix
        m <- solve(data, ...) ## calculate the inverse
        x$setinverse(m)       ## and cache that calculated inverse
        m                     ## return that calculated inverse
}

