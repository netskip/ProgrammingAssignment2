## The makeCacheMatrix and cacheSolve functions work together to provide an efficient
## means to access a matrix's inverse.  The first access incurs the cost of computing
## the inverse, but subsequent accesses quickly retrieve the inverse from the cache.
##
## Example usage for accessing the inverse of matrix q:
##    cm <- makeCacheMatrix(q)
##    minv <- cacheSolve(cm)
##    ... other operations
##    minv2 <- cacheSolve(cm)
## In this example, the matrix is only calculated for minv.  Retrieving the value for
## minv2 is comparatively "free".  
##
## It might be noted that the "cache matrix" returned from makeCacheMatrix could be
## used for caching the results of any matrix calculation.  Nothing in the makeCacheMatrix
## function limits its use to matrix inversions.
##

## The makeCacheMatrix function initializes a matrix cache.  The function parameter x
## is the "source" value -- the matrix on which a calculation (e.g. inversion) is
## being performed.  The member functions get and set can be used to operate on the
## source value.  The member functions setinv and getinv can be used to operate on
## the cached results.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setcache <- function(inv) m <<- inv
    getcache <- function() m
    list(set = set, get = get,
         setcache = setcache,
         getcache = getcache)    
}


## The cacheSolve function uses the cacheResults function (defined below) to perform
## R's built-in solve function and store the results in a cache for later access.
## [[ NOTE TO PEER REVIEWERS:  What you might expect to see in the cacheSolve function
## is in the cacheResults function, which is called by this one.  A little different
## organization, but yields benefits in easily providing cache functionality for other
## matrix operations.  ]]

cacheSolve <- function(x, ...) {
	cacheResults(x, solve, ...)
}


## The cacheResults function performs a matrix operation in a cached manner. First, it
## attempts to retrieve the results of the operation from the cache.  If the cache
## is empty, it performs the calculation, caches the result, and returns it.

cacheResults <- function(x, fun, ...) {
    m <- x$getcache()
	if(!is.null(m)) {
	    message("getting cached data")
	    return(m)
	}
	data <- x$get()
	m <- fun(data, ...)
	x$setcache(m)
	m
}