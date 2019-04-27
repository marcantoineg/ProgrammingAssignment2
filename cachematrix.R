## Create a simple object to cache matrix inversion to reduce cpu load on big computation.

## Create a simple object that caches a matrix and an inverted one.

makeCacheMatrix <- function(x = matrix()) {
  i_m <- NULL
  set <- function(y) {
    x <<- y
    i_m <<- NULL
  }
  get <- function() x
  set_inverted_matrix <- function(inverted_matrix) i_m <<- inverted_matrix
  get_inverted_matrix <- function() i_m
  list(set = set, get = get,
       set_inverted_matrix = set_inverted_matrix,
       get_inverted_matrix = get_inverted_matrix)
}


## Returns the matrix cached if it exists else, calculates it, caches it
## and returns it.

cacheSolve <- function(x, ...) {
  i_m <- x$get_inverted_matrix()
  if(!is.null(i_m)) {
    message("getting cached data")
    return(i_m)
  }
  data <- x$get()
  i_m <- solve(data, ...)
  x$set_inverted_matrix(i_m)
  i_m
}
