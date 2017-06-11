## Put comments here that give an overall description of what your
## functions do
##  This pair of functions cache the inverse of a matrix.



#Make a function which contains the set and get functions and the calculation for inverse
makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  # function for initializing the values
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  #function to get the matrix
  get <- function() x
  
  #function to set the cache
  setinverse <- function(inverse) i <<- inverse
  
  #function to retrive the cache
  getinverse <- function() i
  
  #create list of functions to call using $
  list(set = set, get = get, getinverse = getinverse, setinverse = setinverse)
}

#function to calculate the inverse and cache it using the functions created in makeCacheMatrix
cacheMatrixInverse <- function(xx,...) {
  
  # retrive the matix which is in cache
  mat_inverse <- xx$getinverse()
  
  #check if the matrix is NULL or it is the same as the inverse of the matrix being input into the function
  #if it is the same matrix, then return the retrieved cache, if not do the rest of the steps
  if (identical(solve(mat),mat_inverse) & !is.null(mat_inverse)) {
    message("Getting cached inverse...")
    return(mat_inverse)
  }   
  
  #get the matrix input
  m <- xx$get()
  
  #find the inverse of the matrix
  inverse <- solve(m)
  
  #insert inverse into cache
  xx$setinverse(inverse)
  
  print("Returned new inverse...")
  inverse
}

#How to test
#Create a new invertible matrix
#call the makeCacheMatrix function and pass the matrix as argument and store in a variable
#Call the cacheMatrixInverse function and pass the variable as the argument
#The first time a matrix is processes, it should return the message "Returned new inverse..." showing that the
#inverse was calculated
#The second time the same matrix is passed, it should return message "Getting cached inverse..." showing that
# the cached inverse was retrieved.
