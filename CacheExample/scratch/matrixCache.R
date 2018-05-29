# https://stackoverflow.com/questions/33738820/r-programming-cache-the-inverse-of-a-matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function() inv <<- solve(x) #calculate the inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

funs <- makeCacheMatrix()
funs$set(matrix(1:4, 2))
funs$get()
funs$setInverse()
funs$getInverse()

environment(funs$get)$inv
