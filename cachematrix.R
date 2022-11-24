
##put comments here that give an overall description of what your
##functions do

##there are 2 functions makeCacheMatrix,makeCacheMatrix
##makeCacheMatrix consists of set,get,setinv,getinv
##library(MASS) is used to calculate inverse for non squared as well as square matrices
library(MASS)
makeCacheMatrix <- function(x= matrix()){
  inv<-NULL #intializing inverse as NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function()x
  setinv<-function(inverse)inv<<-inverse
  getinv<-function(){
    inver<-ginv(x)
    inver%%x   #function to obtain inverse of the matrix
  }
  list(set=set,get=get,
       setinv=setinv,
       getinv=getinv)
}
cacheSolve<-function(x,...)##gets cache data
{
  inv<-x$getinv()
  if(!isnull(inv)){   #checking whether inverse is NULL
            message("getting cached data")
            return(inv)            ##returns inverse value
  }
  data<-x$get()
  inv<-solve(data,...)  #calculates inverse value
  x$setinv(inv)
  inv ##return a matrix thst is the inverse of 'x'
}
