mydata <- read.table("q15.txt")

myPLA <- function () {
	w <- c(rep(0,5))
	loop_cnt <- 0
	for (j in 1:1000) {
		halt<-FALSE
		for (i in 1:400) {
			x <- c(1,mydata[i,1],mydata[i,2],mydata[i,3],mydata[i,4])
			y <- mydata[i,5]
			wx = sign( w %*% x)
			if ( wx == 0 ) wx <- -1
			if ( wx != y ) {
				print("update w")
				w <- w + y * x
				break
			} else if ( i == 400 ) halt<-TRUE
		}
		if (halt) {
			print("halt")
			loop_cnt <- j
			break
		}
	}
	loop_cnt
}

