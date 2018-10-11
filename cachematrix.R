##
## Function computes the inverse of the matrix. In case the inverse has 
## already been calculated then the function return it from the cache.
##

makeCacheMatrix <- function(m = matrix()) {

    ## Initialization of object for inverse of matrix
    i <- NULL
    
    ## Function to set up matrix and to clear inverse of matrix
    set <- function( x ){
        m <<- x
        i <<- NULL
    }
 
    ## Function returns matrix  
    get <- function() m
    
    ## Function caches the inverse of matrix
    setInverse <- function( x ) i <<- x
    
    ## Function returns the inverse of matrix
    getInverse <- function() i

    ## Function assigns each functions from above to a list() and returns this list 
    list( set = set, get = get, setInverse = setInverse, getInverse = getInverse )
}


cacheSolve <- function(m, ...) {

    ## Return a matrix that is the inverse of 'x'
    x <- m$getInverse()
    
    ## Check the inverse of matrix and return it in case it was alredy cached
    if( !is.null( x ) ){
        message("getting cached data")
        return( x )
    }

    ## Inverse of matrix by use of function solve() in case it is not done yet.
    m$setInverse( solve( m$get() ) )

    ## Return the inverse of matrix. 
    m$getInverse()
}
