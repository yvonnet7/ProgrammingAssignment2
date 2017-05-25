## cache the inverse of a matrix

## creates a matrix that can cache its inverse

makeCacheMatrix <- function(a = matrix()) {
    inverse <- NULL
    set <- function(b){
      a <<- b
      inverse <<- NULL
    }
    get <- function() a
    setInverse <- function(solution) inverse <<- solution
    getInverse <- function() inverse
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## calculates the inverse of a matrix returned by makeCacheMatrix.
## if the inverse was calculated before, it should just retrieve the inverse from the cache

cacheSolve <- function(a, ...) {
    ## Return a matrix that is the inverse of 'a'
    inverse<- a$getInverse()
    if (!is.null(inverse)){
      message("getting cached data")
      return(inverse)
    }
    data <- a$get()
    inverse <- solve(data)
    a$setInverse(inverse)
    inverse
}

