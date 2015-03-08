# plot3.R

library(dplyr)

##----------------------------------------------------------------
## Step 1 : Read Data from CSV File
##
## Linux and MacOSX systems: 
## 
## The program takes benefits og unix tool for pre-filtering 
## Fastest file reading (alternative authorized by the assignment)
## Search the start and end lines for the 1/2/2007 and 2/2/2007 dates.
## For this purpose, use the power of unix system commands (grep, cut, head)
## Then only read the necessary lines
##
## This fast file reading takes 4.15 seconds
## By comparison a standard read.csv() of the full file 
## takes 19.29 secs on a MacBook Pro Retina 13" Late 2013
## So this fast file reading method is 4.65 times faster
##
## Note the endline search for the first 3/2/2007 occurence
## then takes the previous row. 
## This method allows managing incompleted dataset (for instance
## if the last time of 2/2/2007 is not at 23:59:00), so here 
## it takes the real last row of 2/2/2007
##
## Windows systems: 
##
## The program uses the classical method (5 times slower)
## And the locale stays in default language, so the day of week 
## will not necessarilly looks like the reference .png
##----------------------------------------------------------------

# First check is expected size of the full data is reasonable
# Get 100 rows, then calculate the size in Mb
# If it represents more than 2GB, it could be too much for 
# Small computers 
hundredrows <- read.csv2("./data/household_power_consumption.txt",na.strings="?" ,stringsAsFactors=FALSE,skip=1,nrow=100,header=FALSE)
hundredrowssize <- as.numeric(object.size(hundredrows))
mbsize <- round((2075259/100) * hundredrowssize/(1024^2),2)

if (mbsize > 2048){
    warning("The total size of object after import is ",mbsize," Mb, it exceed 2Gb\n",
            "Make sure your computer as enough RAM to support it or manually reduce the file size")
    warning("If you are under Linux or MacOSX, this is not a problem\n",
            "Only 2880 rows or the file are read, so roughly ", round(hundredrowssize*28.8/(1024^2),2), " Mb"
    )
}

# Save curent locale LC_TIME (to be restored at the end of the program)
loc <- Sys.getlocale(category="LC_TIME")

sysname <- Sys.info()["sysname"]

if (sysname %in% c("Darwin","Linux")) {
    # Set locale LC_TIME to en_US.UTF-8
    Sys.setlocale(category="LC_TIME", locale="en_US.UTF-8")
    
    startline <- as.numeric(system("grep -n \"^1/2/2007\" ./data/household_power_consumption.txt | cut -f1 -d: | head -n 1",intern=TRUE)) - 1
    endline <- as.numeric(system("grep -n \"^3/2/2007\" ./data/household_power_consumption.txt | cut -f1 -d: | head -n 1",intern=TRUE)) - 1
    header <- read.csv2("./data/household_power_consumption.txt",na.strings="?",stringsAsFactors=FALSE,nrow=1,header=FALSE)
    hpc <- read.csv2("./data/household_power_consumption.txt",na.strings="?",stringsAsFactors=FALSE,skip=startline,nrow=endline-startline,header=FALSE)
    names(hpc) <- header[1,]
} else {
    hpc <- read.csv2("./data/household_power_consumption.txt",na.strings="?",stringsAsFactors=FALSE)    
} 

##----------------------------------------------------------------
## Step 2 : prepare data with POSIXlt column and convert
## some columns into numeric
##
## The POSIXlt will enable extracting the Day of Week for the plots
##----------------------------------------------------------------

# Add a DateTime column as string first (a two step process 
# makes the code more readable)
hpc <- mutate(hpc,DateTime=paste(Date,Time))

# Precising the tz is necessary to avoid the daylight saving time
# generating NA on timezone offset during the DST period
hpc$DateTime <- strptime(hpc$DateTime, "%d/%m/%Y %H:%M:%S", tz="UTC")

# Conversion to numeric
hpc[,3:9] <- lapply(hpc[,3:9], as.numeric)

##----------------------------------------------------------------
## Now the data are ready to be plotted
##----------------------------------------------------------------

# Open graphic device for png, the width and hight are
# given even if equal to default values, just for precision
png(filename="plot3.png", width = 480, height = 480)
# Draw the chart

with(hpc,{
    plot(DateTime,Sub_metering_1,type="l",xlab = "",ylab="Energy sub metering")
    lines(DateTime,Sub_metering_2,col="red")
    lines(DateTime,Sub_metering_3,col="blue")
    legend("topright",lty=c(1,1),col=c("black","red","blue"),legend=grep("Sub",names(hpc),value=TRUE))
})

# shut down the current device (png file)
dev.off()

# Restore the LC_TIME locale
Sys.setlocale(category="LC_TIME", loc)












