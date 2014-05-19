complete <- function(directory, id = 1:332) {
	        ## 'directory' is a character vector of length 1 indicating
	        ## the location of the CSV files

	        ## 'id' is an integer vector indicating the monitor ID numbers
	        ## to be used
	        
	files <- lapply(list.files(directory, '*.csv'), function(f) sprintf("%s/%s", directory, f))
	mydata <- do.call("rbind", lapply(files, read.csv))
	        ## Return a data frame of the form:
	        ## id nobs
	        ## 1  117
	        ## 2  1041
	        ## ...
	        ## where 'id' is the monitor ID number and 'nobs' is the
	        ## number of complete cases

	cna <- function(id) {
		d <- subset(mydata, ID %in% id)
		completes <- complete.cases(d)
		s <- sum(completes)
		data.frame(id=id, nobs=s)
	}
	do.call(rbind, Map(cna, id))

}
