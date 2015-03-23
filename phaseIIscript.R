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
getProductReg <- dbGetQuery(conn, "QUERY")


histmakerRetail(getRetailBar, "INSERT retailer ID")
histmakerRegion(getRegionBar, "INSERT region ID")
plotmakerMonth(getLinRed)
productplot(getProductReg)

#Receives a table with 2 columns, retailer and amount
#Creates a bar plot for revenue by Retailer ID
histmakerRetail <- function(directory, x){  
        png(filename="barRetail.png", width=500, height=500, type="cairo")
        aggdata = aggregate(directory, by=list(x), FUN=sum)
        aggfix = cbind(aggdata[1], aggdata[3])
        rowid = aggfix[,1]
        rev = aggfix[,2]
        colnames(aggfix) <- c("retailer ID", "revenue")
        barplot(rev, 
                col = c("lightblue", "mistyrose",
                                        "lightcyan", "lavender"),
                
                xlab = colnames(aggfix[1]),
                ylab = colnames(aggfix[2]),
                names.arg = rowid)
        dev.off()        
}

#Receives a table with 2 columns, region and amount
#Creates a bar plot for revenue by Region
histmakerRegion <- function(directory, x){   
        png(filename="barRegion.png", width=500, height=500, type="cairo")
        
        #combines the amount based on retailer ID
        aggdata = aggregate(directory, by=list(x), FUN=sum)
        
        #Takes away unnecssary column
        aggfix = cbind(aggdata[1], aggdata[3])
        rowid = aggfix[,1]
        rev = aggfix[,2]
        colnames(aggfix) <- c("region ID", "revenue")
        barplot(rev, 
                col = c("lightblue", "mistyrose",
                        "lightcyan", "lavender"),
                
                xlab = colnames(aggfix[1]),
                ylab = colnames(aggfix[2]),
                names.arg = rowid)
        dev.off()
}

#Creates a plot for revenue vs time
plotmaker <- function(directory){
        png(filename="predRev.png", width=500, height=500, type="cairo")
        
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
        dev.off()
}

#Creates a plot for product vs time
productplot <- function(directory){
        png(filename="predProd.png", width=500, height=500, type="cairo")
        
        #splits the data by date
        short.date = strftime(directory$date, "%Y/%m")

        #plot the original data points with larger dots for more freq pts
        y <- short.date$productID
        x <- short.date
        freqData <- as.data.frame(table(aggr.stat, short.date))
        names(freqData) <- c("productID", "date", "freq")
        plot(as.numeric(as.vector(freqData$date)), 
             as.numeric(as.vector(freqData$productID)), 
             pch = 21, col = "black", bg = "lightblue",
             cex = .07 * freqData$freq, xlab = "month", ylab = "productID")
        
        #original regression line, productID as outcome, month as predictor
        abline(mean(y) - mean(x) * cor(y, x) * sd(y) / sd(x), #intercept
               sd(y) / sd(x) * cor(y, x),  #slope
               lwd = 3, col = "red")
        
        #new regression line, month as outcome, productID as predictor
        abline(mean(y) - mean(x) * sd(y) / sd(x) / cor(y, x), #intercept
               sd(y) / cor(y, x) / sd(x), #slope
               lwd = 3, col = "blue")
        
        #assume correlation is 1 so slope is ratio of std deviations
        abline(mean(y) - mean(x) * sd(y) / sd(x), #intercept
               sd(y) / sd(x),  #slope
               lwd = 2)
        points(mean(x), mean(y), cex = 2, pch = 19) #big point of intersection
        dev.off()
}