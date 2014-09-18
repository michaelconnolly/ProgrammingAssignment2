makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  message("no cache, calculating return value")
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

## The way these methods should be used:
## myDataStructure <- makeVector(c(10, 20))
## y <- cachemean(myDataStructure)
testcase <- function(vectorInput){
  
  myVector <- vectorInput
  myDataStructure <- makeVector(myVector)
  
  mean1 <- cachemean(myDataStructure)
  print(mean1)
  mean2 <- cachemean(myDataStructure)
  print(mean2)
  
}

testcase1 <- function(){
  testcase(c(10, 20))
}
