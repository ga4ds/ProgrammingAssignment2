
## The functions in this file create and cache a matrix and its inverse.


## makeCacheMatrix creates the matrix to be stored.
#2345678901234567890123456789012345678901234567890123456789012345678901234567890
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    get <- function() { x }
    getinverse <- function() { inv }
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    setinverse <- function(new_inv) { inv <<- new_inv }

    list(set = set, get = get, 
         setinverse = setinverse,
         getinverse = getinverse) 
}


## cacheSolve returns the inverse of the cached matrix.

cacheSolve <- function(x, ...) {
    ## This function returns a matrix that is the inverse of 'x'
    
  message("entering cacheSolve")
    inv_matrix <- x$getinverse()
    ## Check to see if the inverse has already been calculated.
    if (!is.null(inv_matrix)) {
        ## Return the pre-calculated result.
        message("getting cached data")
        return(inv_matrix)
    }
    orig_mat <- x$get()
    inv_matrix <-solve(orig_mat)
    x$setinverse(inv_matrix)
    inv_matrix
    ## Check to see if the matrix has been changed.
    ## The inverse is computed and cached and the result returned.
}
