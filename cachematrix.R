
## The functions in this file create and cache a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    ## makeCacheMatrix creates the matrix to be stored.
    get <- function() { x }
    getinverse <- function() { inv }
    is_changed <- function() { changed }
    set <- function(y) {
        x <<- y
        m <<- NULL
        changed <<- TRUE
    }
    setinverse <- function(new_inv) { 
        inv <<- new_inv
        changed <<- FALSE
    }

    inv <- NULL
    changed <- FALSE
    list(set = set, get = get, 
         setinverse = setinverse,
         getinverse = getinverse,
         is_changed = is_changed) 
}


cacheSolve <- function(x, ...) {
    ## cacheSolve returns the inverse of the matrix x.  If the inverse is already
    ## cached, that value is returned; otherwise, the inverse is computed and cached.

    inv_matrix <- x$getinverse()
    ## Check to see if the matrix has been changed.
    mat_changed <- x$is_changed()
    ## Find out if 
    use_cached <- !mat_changed & !is.null(inv_matrix)
    if (use_cached) {
        ## Return the pre-calculated result.
        message("getting cached data")
        return(inv_matrix)
    }
    ## The inverse is computed and cached and the result returned.
    orig_mat <- x$get()
    inv_matrix <- solve(orig_mat)
    x$setinverse(inv_matrix)
    inv_matrix
}
