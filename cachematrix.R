## makeCacheMatrix and cacheSolve are functions used to create
## special CacheMatrix, for which the inverse can be cached

## makeCacheMatrix creates a special CacheMatrix that can cache
## its inverse. It's really a list of 4 functions that:
## 1) sets the matrix, 2) gets the matrix
## 3) sets the inverse, 3) gets the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve(x) gets the inverse of x, first 
## checking if it hass already been cached. If not,
## it evaluates the inverse using solve()

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
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

## Example syntax:
##
#x<-rbind(c(1,-1/4),c(-1/4,1))   # create an invertible matrix
#cm<-makeCacheMatrix(x)          # creates a CacheMatrix
#cm$getinverse()                 # check: no inverse exists yet
#cacheSolve(cm)                  # calcaultes and caches the inverse
#cm$getinverse()                 # check: now the inverse exists
