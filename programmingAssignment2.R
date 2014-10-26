
1.	makecachcematrix <- function(x=matrix()) {
	i <- NULL
	set <- function(matrix) {
	m<<- matrix
	i <<-NULL
	}
	get <-function(){
	m
	}
	setinverse<- function() {
	i<<-inverse
	}
	getinverse<-function() {
	i
	}

	list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
	}


2.	cachesolve <- function(x,...) {
	m <-x$getinverse()

	if(!is.null(m)) {
	message("getting cached data")
	return(m)
	}
	data <- x$get()

	m<- solve(data) %*% data
	x$setinverse(m)
	m
	}