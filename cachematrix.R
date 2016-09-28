## It can be costly and time-consuming to compute the inverse of a
## matrix. These two functions cache, or save, the inverse of a
## matrix if it has already been computed once. Then they allow you
## to retrieve the cached computation.

## The first function creates a special "matrix" object that
## can cache the inverse of a matrix. The function returns a list
## containing a function that sets and gets the value of the matrix,
## and sets and gets the value of its inverse.

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) i <<- solve
      getinverse <- function() i
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

## The second function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already
## been calculated for the same matrix, then cacheSolve should
## retrieve the inverse from the cache. (You can tell it's working
## because the message "getting cached data" pops up.)

## If the inverse has not been computed, cacheSolve will return
## the calculated inverse of the matrix.

cacheSolve <- function(x, ...) {
      i <- x$getinverse()
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setinverse(i)
      
      i
      
}
