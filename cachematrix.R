## Two functions that work together to calculate the mean & store in a cache

## This function creates a special matrix: a list of functions that set the 
## elements, get the elements, set the inverse value, & get the inverse 
## value of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function calculates the inverse value of the matrix created with the 
## function above. If the inverse has been calculated, it gets the value from
## the cache and skips the calculation (displays the message "getting cached
## data"). If not, it calculates the inverse value of the matrix & places in
## the cache for the next run.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix_to_invert <- x$get()
  inv <- solve(matrix_to_invert, ...)
  x$setinverse(inv)
  inv
}

  smatrix <- makeCacheMatrix(matrix(1:4, 2, 2))
  smatrix$get()
  smatrix$getinverse()
  cacheSolve(smatrix)
