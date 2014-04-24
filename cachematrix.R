## Creates a special matrix based on a given input matrix. 
## This special matrix will have accompanying functions to set and get the matrix,
## as well as, set and get the inverse of the matrix. This makes use of special
## out of scope assignment rules.

makeCacheMatrix <- function(x_matrix = matrix()) {
    inverse_matrix <- NULL
    
    set <- function(y_matrix) {
      x_matrix <<- y_matrix
      inverse_matrix <<- NULL
    }
    
    get <- function() {
      x_matrix
    } 
    
    setinverse <- function(inverse) {
      inverse_matrix <<- inverse
    } 
    
    getinverse <- function() {
      inverse_matrix 
    }
    
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## This is function taking a special matrix as input and computes the matrix
## inverse. The function checks to see if the inverse matrix has been already
## computed and cached. If cached then merely returns the already computed 
## inverse matrix. Otherwise, the inverse matrix is computed, stored, and 
## then returned as the function output.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse_matrix <- x$getinverse()
    
    if(!is.null(inverse_matrix)) {
      message("getting cached inverse matrix")
      return(inverse_matrix)
    }
    
    original_matrix <- x$get()
    inverse_matrix <- solve(original_matrix, ...)
    x$setinverse(inverse_matrix)
    inverse_matrix
}
