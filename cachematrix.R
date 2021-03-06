
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set<-function(y){
  x<<-y
 inv<<-NULL
}
get<-function() x
setInverse<-function(solve) inv<<- solve
getInverse<-function() inv
list(set=set, get=get,
   setInverse=setInverse,
   getInverse=getInverse)
}

cacheSolve <- function(x=matrix(), ...) {
    inv<-x$getInverse()
    if(!is.null(inv)){
      message("getting cached data")
      return(inv)
    }
    data<-x$get()
    inv<-solve(data, ...)
    x$setInverse(inv)
    inv
}

my_matrix <- makeCacheMatrix(matrix(1:4, 2,2))
my_matrix$get()
my_matrix$getInverse()
cacheSolve(my_matrix)
