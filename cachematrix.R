## Constructs matrix with cacheable inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Return inverse from cache if exsits, caluclates inverse otherwise

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)){
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
