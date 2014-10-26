## R Programming Assignment 2: Lexical Scoping
## Assignment: Caching the Inverse of a matrix 

## The first function makeCachematrix creates a special "matrix", which is really a list containing a function to get and set value of matrix 

makeCachematrix <- function(x = matrix()) {
  inv<-NULL
  ## set the value of the matrix
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  ## get the value of the matrix   
  get<-function() x
  ## set the value of the inverse of the matrix
  setmatrix<-function(solve) inv<<- solve
  ## get the the inverse of the matrix
  getmatrix<-function() inv
  ## return list 
  list(set=set, get=get,setmatrix=setmatrix,getmatrix=getmatrix)
}


## The following function computes the inverse of the special "matrix" returned by makeCachematrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then 
## cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x=matrix(), ...) {
## Return a matrix that is the inverse of 'x'

  inv<-x$getmatrix()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  matrix<-x$get()
  ## To calculate the inverse matrix  
  inv<-solve(matrix, ...)
  x$setmatrix(inv)
  inv
}




