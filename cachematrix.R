#makeCacheMatrix is a function that creates a special "matrix" object that can cache its inverse.



makeCacheMatrix <- function(x = matrix()) {
  sol <- NULL
  set <- function(y) {
    x <<- y
    sol <<- NULL
  }
  get <- function() x
  setsol <- function(solve) sol <<- solve
  getsol <- function() sol
  list(set = set, get = get, setsol = setsol, getsol = getsol)
}


#cacheSolve is a function that computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  sol <- x$getsol()
  if(!is.null(sol)) {
    message("getting cached result")
    return(sol)
  }
  data <- x$get()
  sol <- solve(data, ...)
  x$setsol(sol)
  sol
}

#Example
f<-matrix(rnorm(4),2,2)
f1<-makeCacheMatrix(f)        
cacheSolve(f1)

