## First Mini Project
## Name: John A. Honeyman
## Course: BS in Computer Science IV
## Subject: CMSC 197 - 2

## ============================================================================

## Item 1
pollutantMean <- function(directory, pollutant, id = 1:332){
  # 'list.files' returns the files located in directory, full.names returns the full name of the directory
  # it then gets stored in 'files_null'. This will be the same for other items.
  files_full <- list.files(directory, full.names = TRUE)

  # Declaring empty data.frame and storing it in 'dat'
  # 'for loop' to get the values stored in 'files_full'
  # 'rbind' gets current value of 'dat' and appends the contents of 'i'
  # 'na.omit' to remove all rows that have missing data
  dat <- data.frame()
  for(i in files_full[id]){
    dat <- rbind(dat, na.omit(read.csv(i)))
  }

  # 'mean' to get the mean of 'dat', given the factor 'pollutant'
  return(mean(dat[[pollutant]]))
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

## Item 2
complete <- function(directory, id = 1:332){
  files_full <- list.files(directory, full.names = TRUE)

  # 'nobs' will be the column that will contain the vectors of the complete cases
  # 'for loop' is the same with item1, 'nobs' will get its previous data and append
  # 'nrow(na.omit(read.csv(i)))'. 'nrow' returns the number of rows found in csv file
  # 'na.omit' removes all rows that contains a missing data
  nobs <- c()
  for(i in files_full[id]){
    nobs <- c(nobs, nrow(na.omit(read.csv(i))))
  }

  # returns a data frame with columns id and nobs, and their contents
  return(data.frame(id, nobs))
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

## Item 3
corr <- function(directory, threshold = 0){
  files_full <- list.files(directory, full.names = TRUE)

  # initialize an empty vector 'dat'
  dat <- c()

  # 'for loop' where 'i' goes through the files in 'files_full'
  # 'read.csv(i)' to read the content of 'i'. 'na.omit' to remove all rows with missing data
  # if 'nrow' of 'file' returns a number of complete cases greater than 'threshold'
  # then append the result of correlation of 'file$sulfate' & 'file$nitrate' in dat
  for(i in files_full){
    file <- na.omit(read.csv(i))
    if(nrow(file) > threshold){
      dat <- c(dat, cor(file$sulfate, file$nitrate))
    }
  }

  #return dat
  return(dat)
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

## Item 4
outcome <- read.csv("hospdata/outcome-of-care-measures.csv", colClasses = "character")
outcome[,11] <- as.numeric(outcome[,11])

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Item 1 Tests
pollutantMean("specdata", "sulfate", 1:10)
pollutantMean("specdata", "nitrate", 70:72)
pollutantMean("specdata", "nitrate", 23)

# Item 2 Tests
complete("specdata", 1)
complete("specdata", c(2,4,8,10,12))
complete("specdata", 30:25)
complete("specdata", 3)

# Item 3 Tests
cr <- corr("specdata", 150)
head(cr); summary(cr)

cr <- corr("specdata", 400)
head(cr); summary(cr)

cr <- corr("specdata", 5000)
head(cr); summary(cr); length(cr)

cr <- corr("specdata")
head(cr); summary(cr); length(cr)

# Item 4 Output
# 'main' displays the caption of the histogram
# 'xlab' displays the name of the x-values, 'col' changes the color of the bars
hist(outcome[,11], main="Hospital 30-Day Death (Mortality) Rates from Heart Attack", xlab="Deaths", col="#add8e6")

