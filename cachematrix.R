## these two functions combined have the objective of calculating
#and storing the inverse of a matrix 'x' in cache in order to
#provide easier access to it rather than recalculating the inverse
#repeatedly

## makeCacheMatrix creates an object that stores x (the matrix we
# wish  to invert), and inv (its inverse) as to store it in the 
# GlobalEnvt for easier access, rather than computing inv over and
# over every time we need it for further calculations

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL #creates initially =NULL object that will later store
              #the inverse matrix
  set <- function(y){
          x <<- y
          inv <<- NULL
  }           #creates set() function, can be used to later set
              #a new matrix w/o entering it as an input to
              #makeCacheMatrix and "erase" the previous inv matrix
  
  get <- function(){
          x
  }           #creates the get() function which prints x (the
              #latest set matrix)

  setinv <- function(inverse){
          inv <<- inverse
  }
  
  getinv <- function(){
          inv
  }           #prints the inverse of the earlier set x matrix
  
  
  list(set = set, get =  get,
       setinv = setinv,
       getinv = getinv) 
              #this is the output of makeCacheMatrix, a list
              #containing the 4 functions created inside the
              #makeCacheMatrix envt, all named so to enable
              #access to them via the $ operator
                        
}


## cacheSolve calculates de inverse of the desired matrix x created
# through the previous function and stored in the objct created by
# it

cacheSolve <- function(x, ...) {
  inv <- x$getinv() #attempts to retrieve inverse if its already
                    #stored
  if(!is.null(inv)){
        message("getting cached data")
        return(inv) #case in which inv has alrd been calc 
    
  } else {
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv             ## Returns a matrix that is the inverse of 'x'
                    #stores it in object created by makeCacheMatrix
  }
        
}


######TESTING THE FUNCTIONS######
#for the following test, I used the matrixes provided in the "Simple test
#matrices for the lexical scoping programming assignment" article.

#let's create the matrix I2 
I2 <- matrix(c(1,0,0,1), nrow = 2, ncol = 2)
I2

#inputing it into makeCacheMatrix, creating matrix object
matrixobject <- makeCacheMatrix(I2)
matrixobject

#calculating inverse of I2 using cacheSolve (execute twice to retrieve it from
#cache, w/o calculating it again)
cacheSolve(matrixobject)

#here you can see that the inverse has been stored in matrixobject:
matrixobject$getinv()

#now, if we want to calculate the inverse of another matrix "n1" w/o creating
#another matrix object
  #setting the new matrix into matrixobject:
  n1 <- matrix(c(6,2,8,4), nrow = 2, ncol = 2)
  matrixobject$set(n1)
  
  #here you can see that it has indeed been set and that the previous inverse
  #has been erased from the object
  matrixobject$get()
  matrixobject$getinv() #NULL since the previous has been deleted an the inverse
                        #of n1 has not yet been calculated
  
  #calculating the inverse of n1 (execute twice to retrieve from cache)
  cacheSolve(matrixobject)

  #the inverse has been stored in matrixobject:
  matrixobject$getinv()

  
  
#####THANK YOU FOR REVIEWING! IF I HAVEN'T YET, I WILL REVIEW YOUR ASSIGNMENT ASAP!


