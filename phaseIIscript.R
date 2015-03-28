require(RJDBC)
require(plyr)
require(ggplot2)
require(data.table)
require(splitstackshape)

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

#Creates plot png files
png("plot-%d.png")
#png(filename="barRetail.png", width=500, height=500, type="cairo")
histmakerRetail(getRetailBar, "INSERT retailer ID")
#dev.off()

#png(filename="barRegion.png", width=500, height=500, type="cairo")
histmakerRegion(getRegionBar, "INSERT region ID")
#dev.off()

#png(filename="predRev.png", width=500, height=500, type="cairo")
plotmakerMonth(getLinRed)
#dev.off()

#png(filename="predProd.png", width=500, height=500, type="cairo")
print(productplot(getProductReg))
#dev.off()

#png(filename="barRetprof.png", width=500, height=500, type="cairo")
histmakerRetail(getRetailBar, "INSERT retailer ID")
#dev.off()

#png(filename="barRegprof.png", width=500, height=500, type="cairo")
histmakerRegion(getRegionBar, "INSERT region ID")
#dev.off()

#png(filename="predProf.png", width=500, height=500, type="cairo")
plotmakerMonth(getLinRed)
dev.off()


#Receives a table with 2 columns, retailer and amount
#Creates a bar plot for revenue by Retailer ID
histmakerRetail <- function(directory, x){  
        
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
}

#Receives a table with 2 columns, region and amount
#Creates a bar plot for revenue by Region
histmakerRegion <- function(directory, x){   
        
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
}

#Creates a plot for revenue vs time
plotmaker <- function(directory){
        
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

}


#Creates a plot for product vs time
seasonProducts <- function(directory){
                
        #creates a shortdate column year/month
        directory$shortdate <- strftime(directory$date, format="%Y/%m")
        
        tempdf <- as.data.table(directory)
        
        #counts product ID's for each unique shortdate
        setkey(tempdf, shortdate, productID)
        tempdt <- tempdf[CJ(unique(shortdate), unique(productID)), .N, by = .EACHI]
        as.data.frame(tempdt)
        
        #splits short date into year and month columns
        finaldf <- cSplit(tempdt, "shortdate", "/", stripWhite=FALSE, type.convert=FALSE)
        as.data.frame(finaldf)
        setnames(finaldf, names(finaldf), c("productID","N","year","month"))
        finaldf <- sapply(finaldf, as.numeric)
        finaldf<- as.data.frame(finaldf)
        
        #Creates histogram for each month showing product ID amounts each year
        hp <- (ggplot(finaldf, aes(x=productID, y=N)) + 
                       geom_bar(aes(fill=year),   # fill depends on cond2
                                stat="identity",
                                colour="black") )    # Black outline for all
        
        hp + facet_wrap( ~ month, ncol=3)
        
}