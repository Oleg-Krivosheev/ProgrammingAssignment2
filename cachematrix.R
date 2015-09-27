## Matrix inversion might be a costly operation, therefore we might consider
## memoization so that we store and return already computed inverse value

# `makeCacheMatrix` initializes the cache and
# creates list of functions/methods for cache
# manipulations.
# Methods are returned as a list
makeCacheMatrix <- function(mtx = matrix()) {
    # inverse is set to null on init
    inv <- NULL

    # setter - set the matrix
    set <- function(m) {
        mtx <<- m
        inv <<- NULL
    }

    # getter - get matrix back
    get <- function() {
      mtx
    }
    
    # set cached inverse
    set_inv <- function(i) {
        inv <<- i
    }
    
    # get back cached inverse
    get_inv <- function() {
        inv
    }
    
    list(set = set,
         get = get,
         set_inv = set_inv,
         get_inv = get_inv)
}

# Function `cacheSolve` returns the inverse of the matrix, computed
# for a cache object, created by `makeCacheMatrix`. If cache object already
# containes the computed inverse, it will be retrieved and returned
# without the recalculation.

# Function takes cache object as a parameter,
# matrix from cache is assumed to be inversible
cacheSolve <- function(x, ...) {
    # check the matrix if it is good and inversible
    mtx_check <- function(x) {
        # check if it is a matrix
        if ( !is( x, "matrix") )
            FALSE
    
        # check incoming matrix number of rows,
        # bail out if there is an error
        if ( is.null( nrow(x) ) )
            FALSE
    
        # check incoming matrix number of columns,
        # bail out if there is an error
        if ( is.null( ncol(x) ) )
            FALSE
    
        # is it a square matrix
        if ( nrow(x) != ncol(x) )
            FALSE
    
        TRUE
    }

    # if cache object is NULL,
    # nothing we could do, just return NULL
    if ( is.null( x ) )
        return( NULL )
  
    # check if value is aready computed and cached
    inv <- x$get_inv()
    if ( !is.null(inv) )
        return( inv )

    # get matrix from cache
    mtx <- x$get()
    # check it
    if ( !mtx_check(mtx) )
        return( NULL )
    
    # pased checks, compute inverse and store it into cache
    inv <- solve(mtx)
    x$set_inv( inv )
    
    inv
}
