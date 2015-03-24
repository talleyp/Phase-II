require(RJDBC)
require(plyr)

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
        
        #Takes away unnecssary column and fixes format
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
        
        #adds month and year column
        directory$year <- strftime(directory$date, format="%Y")
        directory$month <- strftime(directory$date, format="%m")
        
        #create revenue by date
        aggr.stat = ddply(directory, .(year, month), function(x) sum(x$amount))
        
        
        #plot the original data points with larger dots for more freq pts
        y <- aggr.stat$V1
        x <- as.numeric(aggr.stat$month)
        
        plot(x, y, type = "p",
             pch = 21, col = "black", bg = "lightblue",
             xlab = "month", ylab = "revenue [USD]")
        
        #original regression line, revenue as outcome, month as predictor
        abline(lm(y~x), #regression line 
               lwd = 3, col = "red")
        
        
        
        dev.off()
}


# #Creates a plot for product vs time
# productplot <- function(directory){
#         png(filename="predProd.png", width=500, height=500, type="cairo")
#         
#         #adds month and year column
#         directory$year <- strftime(directory$date, format="%Y")
#         directory$month <- strftime(directory$date, format="%m")
# 
#         #plot the original data points with larger dots for more freq pts
#         y <- directory$productID
#         x <- directory$month
#         freqData <- as.data.frame(table(directory$productID, directory$month))
#         names(freqData) <- c("productID", "date", "freq")
#         #plot(as.numeric(as.vector(freqData$date)), 
#         #    as.numeric(as.vector(freqData$productID)), 
#         #     pch = 21, col = "black", bg = "lightblue",
#         #     cex = .07 * freqData$freq, xlab = "month", ylab = "productID")
#         plot(as.numeric(as.vector(freqData$date)), 
#              as.numeric(as.vector(freqData$productID)), type="n",
#              xlim=range(as.numeric(simpledf$month)), 
#              ylim=range(simpledf$productID), xlab="Month", ylab="productID")
#         
#         
#         #original regression line, revenue as outcome, month as predictor
#         abline(lm(y~x), #regression line 
#                lwd = 3, col = "red")
# }