### John Hopkins University Coursera R programming assignment 2, week 3.
# by M Djela, my GitHum name is Gilmage

###  Caching the Inverse of a Matrix

## To save the inefficient computation of the matrix inversion we will cache the inverse of a matrix 
## rather than compute it repeatedly, by creating a pair of functions that work together: 
## makeCacheMatrix() and cacheSolve().

## This is possible due to lexical scoping and also demonstrates how this scoping works


### makeCacheMatrix()

## This function creates a special "matrix" object that can cache its inverse.(L Peng)
## It does this by creating a special object, which is really a list containing a function to

## 1 set the value of the special matrix
## 2 get the value of the special matrix,   i.e  retrieve (access) data within the set matrix
## 3 set the value of the inverse matrix
## 4 get the value of the inverse matrix,  i.e. retrieve data within the inverse matrix

## The elements of the list are named after the four functions above, so they can be retreaved by lexical scoping
## This function is incomplete without cacheSolve() because the computation of the inverse matrix only happens there


makeCacheMatrix <- function(x = matrix()) {   # initialising x as the functions argument x as an empty matrix by default
  inv <- NULL                                 # initialisinf inv within the makeCacheMatrix() environment to be used by later code in the function
  set <- function(y) {    # defines the set() function (name of arg y does not matter)that set the value of the special matrix
    x <<- y   # assigns the input argument y to the x object in the parent environment, i.e. resets the x object
    inv <<- NULL # assign the value of NULL to the inv object in the parent environment. 
    # This clears any value of inv that had been cached by a prior execution of cacheSolve()
    # I.e. whenever x is reset, the value of inv cached is cleared, forcing subsequent calls to cacheSolve() to recalculate
  }
  get <- function() x  # defines the getter for the vector x due to lexical scoping
  #Since the symbol x is not defined within get(), R retrieves it from the parent environment of makeCacheMatrix()
  setinv <- function(solve) inv <<- solve # defines the setter for the inverse matrix inv assigning inv the value of input "solve".
  # Since inv is defined in the parent environment and we need to access it after setinv() completes, the code uses the <<- form 
  # of the assignment operator to assign the input argument solve to the value of inv in the parent environment.
  getinv <- function() inv # defines the getter for the inv. 
  # Similar to the getter for x, R lexical scoping is used to find the correct symbol inv 
  # to retrieve its value from the parent environment of makeCacheMatrix().
  list(set = set, get = get, # Creates the special object by returning a list() 
       # with elements named after the four functions above, so they can be retreaved by lexical scoping
       setinv = setinv,
       getinv = getinv)
       # i.e. gives the name 'set' to the set() function defined above
       # gives the name 'get' to the get() function defined above
       # gives the name 'setinv' to the setinv() function defined above
       # gives the name 'getinv' to the getinv() function defined above
       # what allows us to use the $ form of the extract operator to access the functions by name 
}


### cacheSolve()

# This function computes the inverse of the "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then this function should retrieve the inverse from the cache.(L. Peng)

# Computing the inverse of a square invertible matrix is done via the solve() function. 

# cacheSolve() function REQUIRES an input argument of type makeCacheMatrix()
# The call to solve() function only happens here, 
# so cacheSolve() is required to populate or retrieve the inverse from an object of type makeCacheMatrix()

cacheSolve <- function(x, ...) { # starts with a single argument, x, and an ellipsis that allows the caller to pass additional arguments
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()  # retrieve the inverse from the object passed in as the argument. First, it calls the getinv() function on the input object x.
  if(!is.null(inv)) { # checks if there is valid inv is in the cache 
    message("getting cached data") 
    # Since makeCacheMatrix() sets the cached inv to NULL when a new vector is set into the object, 
    # if the value here is not NULL, we have a valid, cached inv and can return it to the parent environment
    return(inv)
  }
  # If the result of !is.null(inv) is FALSE, 
  data <- x$get()  # gets the matrix from the input object x, 
  inv <- solve(data, ...) # calculates the inverse by solve()
  x$setinv(inv)  # uses the setinv() function on the input object x to set the inverse for x in the input object
  inv  # returns the value of the inv to the parent environment by printing the inverse object.
}


# I tested this and it works:
# m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
#> m1
#[,1]  [,2]
#[1,]  0.50 -1.00
#[2,] -0.25  0.75
#> myMatrix_object <- makeCacheMatrix(m1)
#> cacheSolve(myMatrix_object)
#[,1] [,2]
#[1,]    6    8
#[2,]    2    4
#> cacheSolve(myMatrix_object)
#getting cached data
#[,1] [,2]
#[1,]    6    8
#[2,]    2    4
#> 

