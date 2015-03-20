require(RJDBC)

# this line will create the driver object to allow R to talk with MySQL
drv <- JDBC("com.mysql.jdbc.Driver","mysql-connector-java-5.1.34 bin.jar",
            identifier.quote="`")

# this line will connect to your database using the driver.
# Replace DB_NAME with the name of your database (i.e., group name/login),
# the LOGIN name of your group, and your group PASSWORD.

conn <- dbConnect(drv, "jdbc:mysql://mydb.itap.purdue.edu/DB_NAME",
                 "LOGIN", "PASSWORD")

# run your QUERY (i.e., SELECT statement), the result is a data.frame
getRetailBar <- dbGetQuery(conn, "QUERY")
getRegionBar <- dbGetQuery(conn, "QUERY")
getLinReg <- dbGetQuery(conn, "QUERY")


source(histmaker)
source(plotmaker)
histmaker(getRetailBar, "INSERT RECEIPT COST")
histmaker(getRegionBar, "INSERT RECEIPT COST")
plotmakerMonth(getLinRed, "")


histmaker <- function(directory, x){

        id = nrow(getRetailBar)
        s = split(data, data$x)
        rev = sapply(s$cost, sum)
        barplot(rev, beside=T)
}
plotmaker <- function(directory){
        #splits the data by date
        short.date = strftime(directory$date, "%Y/%m")
        #create revenue by date
        aggr.stat = aggregate(directory$amount ~ short.date, FUN = sum)
        
        
        #plot the original data points with larger dots for more freq pts
        y <- aggr.stat
        x <- short.date
        freqData <- as.data.frame(table(aggr.stat, short.date))
        names(freqData) <- c("revenue", "date", "freq")
        plot(as.numeric(as.vector(freqData$date)), 
             as.numeric(as.vector(freqData$revenue)), 
             pch = 21, col = "black", bg = "lightblue",
             cex = .07 * freqData$freq, xlab = "month", ylab = "revenue")
        
        #original regression line, revenue as outcome, month as predictor
        abline(mean(y) - mean(x) * cor(y, x) * sd(y) / sd(x), #intercept
               sd(y) / sd(x) * cor(y, x),  #slope
               lwd = 3, col = "red")
        
        #new regression line, month as outcome, revenue as predictor
        abline(mean(y) - mean(x) * sd(y) / sd(x) / cor(y, x), #intercept
               sd(y) / cor(y, x) / sd(x), #slope
               lwd = 3, col = "blue")
        
        #assume correlation is 1 so slope is ratio of std deviations
        abline(mean(y) - mean(x) * sd(y) / sd(x), #intercept
               sd(y) / sd(x),  #slope
               lwd = 2)
        points(mean(x), mean(y), cex = 2, pch = 19) #big point of intersection
}