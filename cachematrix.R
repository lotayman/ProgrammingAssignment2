## These functions are used to solve matracies and to return the cache that contains the inverse

## Creates a special vector which is a list containing a function to:
## 1. set value of the vector
## 2. get value of the vector
## 3. set value of the inverse
## 4. get value of the inverse

makeCacheMatrix <- function(x = matrix()) {

  i <- NULL
  
    set <- function(y) {
      
      x <<- y
      i <<- NULL
      
    }
  
  get <- function() x
  
  setinverse <- function(inverse) i <<- inverse
  
  getinverse <- function() i
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Calculates the inverse of the special vector from the previous function, however it first checks if the inverse has already been calculated.
## If the inverse already exists then it retreives the inverse from the cache and skips the computation.
## Otherwise it calculates the inverse of the matrix and sets the value of the inverse in the chache to the calculated value

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
