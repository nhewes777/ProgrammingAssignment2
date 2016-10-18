## makeCacheMatrix

## The function makeCacheMatrix creates a special matrix object that consists
## of a set of functions that can store and retrieve a matrix and store and
## retrieve the inverse of the matrix.

## The function makeCacheMatrix requires a matrix as an argument.
## It is assumed that the matrix argument is a square matrix.

makeCacheMatrix<-function(x=matrix()){
  ## The variable minv will hold the inverse matrix.
  ## Initialise the value of minv to be null.
  minv<-NULL
  
  ## The setmatrix function allows us to change the matrix stored in the
  ## special matrix object without having to create a new instance. 
  ## Note that the <<- assignment operator is used so that values of x
  ## and minv that contain the matrix and inverse matrix respectively
  ## are available in the makeCacheMatrix function environment.
  ## If the setmatrix function is used and the stored matrix is changed
  ## it is important that the inverse matrix (minv) is reset to a NULL value
  ## so that the inverse matrix is re-calculated. Otherwise we might have 
  ## the situation where the inverse matrix being stored does not correspond
  ## to the new matrix passed as an argument to the setmatrix function.
  setmatrix <- function(y) {
    x <<- y
    minv <<- NULL
  }
  
  ## The getmatrix function returns the stored matrix.
  getmatrix <- function() x
  
  ## The setinverse function assigns the inverse matrix to the 
  ## variable minv. Note that the <<- assignment operator is
  ## used so that the inverse matrix is available in the makeCacheMatrix
  ## environment and can therefore be returned by the getinverse function.
  setinverse <- function(inv) minv <<- inv
  
  ## The getinverse function returns the stored inverse matrix. 
  getinverse <- function() minv
  
  ## A list is returned to the global environment the members of 
  ## which are the setmatrix, getmatrix, setinverse and getinverse
  ## functions.
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## cacheSolve

## The cacheSolve function takes an instance x of a special matrix (or list)
## created by the makeCacheMatrix as an argument and determines whether or not 
## the inverse of the matrix has already been calculated. If the inverse of the 
## matrix has been calculated previously then the cached  inverse matrix is 
## returned otherwise the inverse of the matrix is calculated using the solve 
## function and it's value is stored in the makeCacheMatrix list x using the 
## setinverse function.

cacheSolve<-function(x,...){
  ## First determine if the inverse matrix has already been
  ## calculated for the instance x of the special matrix (or list) 
  ## defined by the makeCacheMatrix function.
  
  ## Assign the value returned by the getinverse function to the 
  ## variable minv. The value of minv will either be NULL if the
  ## inverse matrix has not been previously calculated and cached
  ## or it will contain the inverse of the matrix.
  minv <- x$getinverse()
  
  ## If the value of minv is not NULL then output the value of the
  ## cached inverse matrix and inform the user that the cached inverse
  ## matrix is returned.
  if(!is.null(minv)) {
    message("The inverse matrix has been previously cached.")
    message("Getting the cached inverse matrix.")
    return(minv)
  }
  
  ## If the value of minv is NULL then we need to calculate the inverse
  ## matrix and cache the result.
  else {
    ## Inform the user that the inverse matrix has not previously
    ## calculated and cached.
    message("The inverse matrix has not been previously cached.")
    message("Calculating the inverse matrix.")
    
    ## Assign the matrix that is stored in the makeCacheMatrix list
    ## x to the variable matrix.
    matrix <- x$getmatrix()
    
    ## Use the solve function to calculate the inverse matrix. 
    ## Note that the argument b in the solve function has
    ## not been supplied with a value so that the inverse 
    ## of the matrix is returned since if nothing is specified
    ## for b it is taken to be the identity matrix.
    minv <- solve(a=matrix)
    
    ## Once the inverse matrix has calculated store it in the
    ## makeCacheMatrix list x.
    x$setinverse(minv)
    
    ## Output the inverse matrix.
    minv
  }
}
