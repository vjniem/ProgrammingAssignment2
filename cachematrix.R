## First function: makeCacheMatrix modifies matrix so that it can store
## its own inverse matric to memory as variable inv

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL # This sets the stored inverse as NULL meaning it hasn't been calculated
  }
  
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function checks if matrix inversion has already been performed and if not
## it solves the inverse for the matrix given.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data") # If inverse as been solved (inv != NULL) get stored value
    return(inv)
  }
  my_matrix <- x$get()
  inv <- solve(my_matrix, ...) # If inverse hasn't been solved, solve it now
  x$setinverse(inv)
  inv
}
