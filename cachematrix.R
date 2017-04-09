## This is an assignment submission for Coursera R-Programming Module.
##Under this assignment a caching solution is to be constructed. The sample function is provided with 
#mean calculation as example.


## The function makeCacheMatrix created for Setting and getting the Inverse vector

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## second part of function will Cache solve the multiple iterations
## I have also tested this function with Sys.time (this code is commented)

# Assumption that the matrix is always invertable
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    return(inv) 
    message("This function run is on Cache data")
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
        
## Function test
# x = rbind(c(2, 4), c(4, 2))
#m = makeCacheMatrix(x)
#m$get()


#cacheSolve(m)
#first_time=Sys.time()


#cacheSolve(m)
#second_time=Sys.time()
#cacheSolve(m)
#third_time=Sys.time()
#elapse_time <-second_time- first_time
#elapse_time_next <-third_time- second_time
#print(elapse_time)
#print(elapse_time_next)

#result

#print(elapse_time)
#Time difference of 0.0009999275 secs
# print(elapse_time_next)
#Time difference of 0 secs