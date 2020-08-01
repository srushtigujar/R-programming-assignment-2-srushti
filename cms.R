makeCacheMatrix <- function(y = matrix()){
  inv <- NULL
  set <- function(z)
  {
    y <<- z
    inv <<- NULL
  } 
  get <- function() {y}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function(){inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}
cacheSolve <- function(y, ...){
  inv <- y$getInverse()
  if(!is.null(inv)){message("Cached Data")
    return(inv)
    } 
   mat <- y$get()
    inv <-solve(mat, ...)
    y$setInverse(inv)
    inv
}