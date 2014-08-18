## These functions can be used to store a matrix, and to get its inverse matrix
## either from a cache (if it has already been computed) or by computing it
## using the solve() function

## makeCacheMatrix() will take a matrix parameter x, and return a "CacheMatrix"
## object (basically this object is a list of functions), that can be used to
## read/write the matrix and to store/retrieve a cached value (in this exercise
## used to store the inverse matrix of x)

makeCacheMatrix <- function(x = matrix()) {
    ## initialize chache
    cachedInverse <- NULL

    ## create functions to get/set the matrix value and the cache
    get <- function () {
        x
    }
    set <- function (newMatrix) {
        x <<- newMatrix
        ## make sure cache is empty, to force recalculation of the inverse matrix
        cachedInverse <<- NULL
    }
    readCache <- function () {
        cachedInverse
    }
    writeCache <- function (value) {
        cachedInverse <<- value
    }
    
    ## return the matrix
    list(get = get, set = set, readCache = readCache, writeCache = writeCache)
}


## cacheSolve() will compute and return the inverse matrix of a given "CacheMatrix"
## object that has been produced by the makeCacheMatrix() function. It will store the
## result in the cache provided by the "CacheMatrix" object, and subsequent
## evaluations will just return the cached value, as long as the matrix has not been
## altered.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    ## try to read the cache
    cache <- x$readCache()
    
    ## if the cache is empty, calculate the inverse matrix and write it to the cache
    if (is.null(cache)) {
        cache <- solve(x$get())
        x$writeCache(cache)
    }
    
    ## return the inverse matrix
    cache
}
