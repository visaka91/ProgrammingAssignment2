## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly
##The below pair of functions will cache the inverse
## of a matrix.



## makeCacheMatrix function creates a special "matrix" object that can cache its
## inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse_matrix <- function(inverse_matrix) m <<- inverse_matrix
  getinverse_matrix <- function() m
  list(set = set, get = get,
       setinverse_matrix = setinverse_matrix,
       getinverse_matrix = getinverse_matrix)
  
}


##This function computes the inverse of the special "matrix" returned by
##makeCacheMatrix above. If the inverse has already been calculated (and the
##matrix has not changed), then the cachesolve should retrieve the inverse from
##the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse_matrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse_matrix(m)
  m
  
}
