
makeCacheMatrix <- function(x = matrix()) {
  Inv <- NULL
  set<-function(y){
  x<<-y
 Inv<<-NULL
}
get<-function() x
setInverse<-function(solve) Inv<<- solve
getInverse<-function() Inv
list(set=set, get=get,
   setInverse=setInverse,
   getInverse=getInverse)
}

cacheSolve <- function(x=matrix(), ...) {
    Inv<-x$getInverse()
    if(!is.null(Inv)){
      message("getting cached data")
      return(Inv)
    }
    data<-x$get()
    Inv<-solve(data, ...)
    x$setmatrix(Inv)
    Inv
}

my_matrix <- makeCacheMatrix(matrix(1:9, 3, 3))
my_matrix$get()
my_matrix$getInverse()
cacheSolve(my_matrix)
