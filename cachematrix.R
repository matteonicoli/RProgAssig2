## The function makeCacheMatrix returns a list of four functions 
## that operate on a local matrix A and its cached inverse inv_A.
## When A is created, inv_A is NULL and it should be set through
## the ...$setinverse() function in order to cache it.
## --------------------------------------------------------------
## A$set(B) sets the value of the matrix, A <- B
## A$get()  returns the matrix A
## A$setinverse(iA) sets the cached inverse inv_A <- iA
## A$getinverse() returns the cached inverse inv_A of the matrix A

makeCacheMatrix <- function(A = matrix()) {
    inv_A <- NULL
    set <- function(B) {
        A <<- B
        inv_A <<- NULL 
    }
    get <- function() A
    setinverse <- function(iA) inv_A <<- iA
    getinverse <- function() inv_A
    
    list( set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The function cacheSolve returns the inverse of a matrix A created with
## the makeCacheMatrix function.
## If the cached inverse is available, cacheSolve retrieves it, while if
## not, it computes, caches, and returns it.

cacheSolve <- function(A, ...) {
    ## Return a matrix that is the inverse of 'A'
    
    inv_A <- A$getinverse()
    if (!is.null(inv_A)) {
        message("getting cached inverse matrix")
        return(inv_A)
    } else {
        inv_A <- solve(A$get())
        x$setinverse(inv_A)
        return(inv_A)
    }
}
