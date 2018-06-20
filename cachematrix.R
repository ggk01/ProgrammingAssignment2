# Following the example for the Vectors we can customize the given functions.
# By making the appropriate changes, we are able to calculate the inverse of a matrix in a similar way. 
# Below are shown the reformed functions.
#
#


## Make Matrix ##
makeCacheMatrix <- function(x = matrix())      # This function help us transform the matrix in a form that is useful for caching
{
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Cache Solve ##
cacheSolve <- function(x, ...)  
{
  inv <- x$getinverse()           #first our algorithm looks in cache
  if(!is.null(inv)){              #if found there, it will be retrieved
    message("getting cached data")
    return(inv)
  }
  data <- x$get()             #else will be solved using the solve method 
  inv <- solve(data, ...)
  x$setinverse(inv)         #then the result will be stored in cache before returned
  inv                   # lastly the inversed matrix is returned
}
