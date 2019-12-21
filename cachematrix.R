## The two functions makeCacheMatrix() and cacheSolve() work together to cache and
##calculate the inverse of a matrix respectively.The cacheSolve() function calculates
## the inverse only if a cached inverse does not exist for the given matrix.


##The makeCacheMatrix() function takes a invertible matrix as argument
##1)It initially sets the inverse(inv) to NULL
##2)Returns a list containing four functions-
##  Setters: set() and setInverse() to cache the matrix and it's inverse respectively
##  Getters: get() and getInverse() to read the matric and it's inverse respectively
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv<<-inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##1)The cacheSolve() function takes a makeCacheMatrix() object as argument.
##2)On receiving the argument it checks the cached inverse value of the matrix.
##3)It returns the cached inverse value if exists. Otherwise it calculates the
##  inverse using the get() function to obtain the matrix, solve() function to
##  calculate the inverse and finally the setInverse() function to cache the
##  calculated inverse.
cachesolve <- function(x, ...) {
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