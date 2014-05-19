pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used

        #Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
		#directory <- "specdata/"
		#id <- 23
		#pollutant <- 'sulfate'

		files <- lapply(list.files(directory, '*.csv'), function(f) sprintf("%s/%s", directory, f))
		mydata <- do.call("rbind", lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))
		#mydata[1:5,]
		#nrow(mydata)
			
		values <- subset(mydata, ID %in% id)
		values <- values[,pollutant]
		values <- values[!is.na(values)]
		#length(values)

		mean(values)

}
