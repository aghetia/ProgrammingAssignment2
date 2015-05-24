## makeChacheMarix function calculate the inverse of matrix and save it to cache 
## so once again it can be used without doing repeating calculation

## This function creates below four functions
## Set value of the matrix
## get value of the matrix
## set value of the inverse
## get value of the inverse


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL    ## define the cache m
  set <- function(y) {
    x <<- y    ## assign the input matrix y to the variable x in the
    m <<- NULL ## re-initialize m to null
  }
  get <- function() x                           ## return the matrix x
  setinverse <- function(inverse) m <<- inverse ## set the cache m equal
                                                ## to the inverse of the matrix x
  getinverse <- function() m                    ## return the cached inverse of x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve function calculates the inverse of the special "matrix" created
## with the above function. However, it first checks to see if the inverse
## has already been calculated. If so, it's get's the inverse from the cache
## and skips the computation. Otherwise, it calculates the matrix inverse
## and sets the value of the inverse in the cache via the 'setinverse' function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)         ## Function return Value 
  }
  data <- x$get()     
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
