## Pair of functions meant to calculate, cache, and 
## retrieve the inverse of a matrix

## Creates a "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
                    invm <- NULL          ##variable to store inverse value
                    set <- function(y) {
                      x <<- y
                      invm <<- NULL
                    } ## sets value of matrix in parent environment and resets 
                      ## the value of invm to NULL

                    get <- function()x ## return value of matrix
                    
                    setinverse <- function(inverse) invm <<- inverse  
                    ##sets value of invm in parent environment
          
                    getinverse <- function() invm #returns value of invm
                    
                    list(set=set, get=get, setinverse=setinverse, getinverse=
                           getinverse) 
                    ## prints list that permits access to functions with the $
                  }


## Computes the inverse of makCacheMatrix output. If already calculated and 
## matrix is unchanged inverse is retrieved from cache

cacheSolve <- function(x, ...) {
                invm <- x$getinverse()
                if(!is.null(inv)){
                  message("Getting cached data...")
                  return(invm)
                } 
                ## checks if invm has been calculated and returns its value if 
                ## has after notifying the user
                data <- x$get() ## retrieves matrix values
                invm <- solve(data) ## returns the inverse of matrix
                x$setinverse(invm) ## stores the result of the calculation 
                invm
              } ## Return a matrix that is the inverse of 'x'