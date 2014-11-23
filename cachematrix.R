## Jian Liang completed on 22nd Nov 2014 for R Programming Assignment 2

## This function creates a special "matrix" object that can cache its inverse
## It has following 4 functions:
## getm() (get the initial matrix, return a matrix), 
## setm() (set the origional matrix, return a boolean value),
## getinvm() (get the inverse matrix, return the inverse matrix), 
## setinvm() (assign the inverse matrix, cache inverse matrix)

makeCacheMatrix <- function(x = matrix()) {
  ## Initialized inverse matrix
  Inverse <- NULL
  
  ## This function is to initialized a matrix, if passed
  ## in matrix is NULL or not Matrix class object, a 1 value
  ## matrix will be store , and a warning message will
  ## be given. other
  setm <- function(mat){
    
    if(length(mat) == 0){
      
      warning("Matrix passed in is in length of 0")
      return(FALSE)
    }
    if(class(mat) == "matrix" & mat != NULL){
      x <<- mat
      Inverse <<- NULL
      return(TRUE)
    }
    else{
      warning("Please ensure you pass a matrix class valuable as parameter.")
      return(FALSE)
    }
  }
  
  getm <- function(){
    return(x)
  }
  
  ## This function is to store inverse matrix when origional
  ## matrix and passed in matrix are both not empty. It doesn't check
  ## if a matrix is inversible. When assign successfully, it
  ## will return TRUE, otherwise FALSE
  setinvm <- function(inv){
    if(class(inv)!="matrix"| is.null(inv)){
      warning("The object type passed in is not matrix")
      return(FALSE)
    }
    else if(is.na(x[[1]])){
      warning("There is no matrix initialized yet, please provid a matrix
              before given a inverse matrix")
      return(FALSE)
    }
    else if (is.na(inv[[1]])){
      warning("Empty matrix is not acceptable as inverse matrix")
      return(FALSE)
    }
    Inverse<<-inv
    return(TRUE)
  } 
  
  getinvm <- function(){
    Inverse
  }
  
  list(setm = setm, getm = getm,
        setinvm = setinvm,
        getinvm = getinvm)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache. otherwise it calculates the 
## inverse matrix assign to cache and return

cacheSolve <- function(x, ...) {

  ## Valuated if X is a valid matrix, if not, return NULL as result
  if(class(x)!="matrix" | is.null(x)){
    warning("Passed variable is not a valid matrix class object")
    return(NULL)
  }
  
  
  invm <- x$getinvm()
  mat <- x$getm()
  
  ## Check if the inverse is not null or matrix has no changed
  ##if (!is.null(invm) & identical(x,mat)){
  
  if (!is.null(invm)){
    message("cache exist")
    return (invm)
  }
  
  ## When inverse matrix is not in cache, calculate and return
  
  ## message("Cache not exist")
  
  invm <- solve(x)
  if(!x$setinvm(invm)){
    warning("Either matrix is empty matrix")
  }
  ## message("Inverse Matrix cached")
  return (invm)
}
