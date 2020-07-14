## A matrix object is created which is able to cache inverse. The 
## second function then retrives the inverse from the cache if it has been 
## already created, if not then it calculates and creates the inverse

## makeCacheMatrix function creates a special "matrix" object that can cache 
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() {x}
    setinverse <- function(inverse) {m <<- inverse}
    getinverse <- function() {m}
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



## cacheSolve function computes the inverse of the special "matrix"  returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve retrieves the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m

}
