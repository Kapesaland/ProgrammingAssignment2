## Pair of functions meant to calculate, cache, and 
## retrieve the inverse of a matrix

## Creates a "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
                    invm <- NULL
                    set <- function(y) {
                      x <<- y
                      invm <<- NULL
                    }

                    get <- function()x
                    
                    setinverse <- function(inverse) invm <<- inverse
          
                    getinverse <- function() invm
                    
                    list(set=set, get=get, setinverse=setinverse, getinverse=
                           getinverse)
                  }


## Computes the inverse of makCacheMatrix output. If already calculated and 
## matrix is unchanged inverse is retrieved from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                invm <- x$getinverse()
                if(!is.null(inv)){
                  message("Getting cached data...")
                  return(invm)
                }
                data <- x$get()
                invm <- solve(data)
                x$setinverse(invm)
                invm
              }