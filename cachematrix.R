## makeCacheMatrix creates a special "matrix" object that can cache its inverse
## while, cacheSolve computes the inverse of the special "matrix" returned by 
## makeCaceMatrix. But if the inverse has already been calculated, then the 
## cacheSolve retrives the inverse from the cache


##makeCacheMatrix creates a special "matrix" that contains a function to:
  ##set the value of the matrix
  ##get the value of the matrix
  ##set the value of the inverse
  ##get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
          x <<- y
          m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  matrix(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

##cacheSolve checks if the inverse of the matrix has beed calculated. 
##If so, this function gets the matrix inversion from the cache and skips the computation
##Otherwise, it calculates the inverse of the matrix and sets the value of the
##inversion in cache via setmean function

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
        message("getting cached matrix inversion")
        return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmean(m)
  m
  ## Return a matrix that is the inverse of 'x'
}
