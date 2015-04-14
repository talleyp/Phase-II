require(RMySQL)
require(plyr)
require(ggplot2)
require(data.table)
require(splitstackshape)


# this line will create the driver object to allow R to talk with MySQL
drv <-  dbDriver("MySQL")

# this line will connect to your database using the driver.
# Replace DB_NAME with the name of your database (i.e., group name/login),
# the LOGIN name of your group, and your group PASSWORD.

conn <- dbConnect(drv, user="g1046729",dbname='g1046729', pass="mapftk20", host ="mydb.ics.purdue.edu")



avgbasket <- function(data){
        dt <- count(tt, vars=c("RetailerID","RetailerID"))
        df <- aggregate(freq ~ RetailerID, dt, FUN = mean)
        barplot(df$freq, 
                col = c("lightblue", "mistyrose",
                        "lightcyan", "lavender"),
                
                xlab = colnames(df[1]),
                ylab = colnames(df[2]),
                names.arg = rowid)
}

avgtick <- function(data){
        dt <- aggregate(Price~RetailerID+ReceiptID, data, sum)
        df <- aggregate(Price~RetailerID, dt, mean)
        barplot(df$Price, 
                col = c("lightblue", "mistyrose",
                        "lightcyan", "lavender"),
                
                xlab = colnames(df[1]),
                ylab = colnames(df[2]),
                names.arg = rowid)
}

histmakerRegion <- function(directory){  
        
        aggdata = aggregate(directory, by=list(RegionID), FUN=mean)
        aggfix = cbind(aggdata[1], aggdata[3])
        rowid = aggfix[,1]
        rev = aggfix[,2]
        colnames(aggfix) <- c("Region ID", "Profit")
        barplot(rev, 
                col = c("lightblue", "mistyrose",
                        "lightcyan", "lavender"),
                
                xlab = colnames(aggfix[1]),
                ylab = colnames(aggfix[2]),
                names.arg = rowid)
}

histmakerRetail <- function(directory){  
        
        aggdata = aggregate(directory, by=list(RetailerID), FUN=mean)
        aggfix = cbind(aggdata[1], aggdata[3])
        rowid = aggfix[,1]
        rev = aggfix[,2]
        colnames(aggfix) <- c("retailer ID", "Profit")
        barplot(rev, 
                col = c("lightblue", "mistyrose",
                        "lightcyan", "lavender"),
                
                xlab = colnames(aggfix[1]),
                ylab = colnames(aggfix[2]),
                names.arg = rowid)
}


plotmaker <- function(directory){
        directory$daten <- as.Date(directory$Date, format = "%d%m%Y")
        
        #adds month and year column
        directory$year <- strftime(directory$daten, format="%Y")
        directory$month <- strftime(directory$daten, format="%m")
        
        #create revenue by date
        aggr.stat = ddply(directory, .(year, month), function(x) mean(x$Price))
        
        
        #plot the original data points with larger dots for more freq pts
        y <- aggr.stat$V1
        x <- as.numeric(aggr.stat$month)
        
        plot(x, y, type = "p",
             pch = 21, col = "black", bg = "lightblue",
             xlab = "month", ylab = "revenue [USD]")
        
        #original regression line, revenue as outcome, month as predictor
        lmfit <- lm()
        
        plot(lmfit)
        
}

#Creates a plot for product vs time
seasonProducts <- function(directory){
        #directory$daten <- as.Date(directory$Date, format = "%d/%m/%Y")
        
        #creates a shortdate column year/month
        directory$shortdate <- strftime(directory$date, format="%Y/%m")
        directory$month <- strftime(directory$date, format="%m")
        
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
        hp <- (ggplot(finaldf, aes(x=month, y=N)) + 
                       geom_bar(aes(fill=as.factor(year)),   # fill depends on cond2
                                stat="identity",
                                colour="black") )    # Black outline for all
        hp + facet_wrap( ~ productID, ncol=5)  
        
}

# run your QUERY (i.e., SELECT statement), the result is a data.frame

#Retailer Profit
getRR <- dbGetQuery(conn,"SELECT Retailers.RetailerID, [Price]-[Cost] AS Profit
FROM Products, Retailers;")

#Region Profit
getGR <- dbGetQuery(conn,"SELECT Retailers.Region, [Price]-[Cost] AS Profit
FROM Retailers, Products;")

#Average Ticket Size by Retailer
getAT <- dbGetQuery(conn,"SELECT Retailers.RetailerID, Products.ProductID, Products.Price, ReceiptProducts.ReceiptID
FROM Retailers, ReceiptProducts INNER JOIN Products ON ReceiptProducts.ProductID = Products.ProductID
GROUP BY Retailers.RetailerID, Products.ProductID, Products.Price, ReceiptProducts.ReceiptID;")

#Average basket size by retailer
getAB <- dbGetQuery(conn,"SELECT Retailers.RetailerID, ReceiptProducts.ReceiptID
FROM Retailers, ReceiptProducts
GROUP BY Retailers.RetailerID, ReceiptProducts.ReceiptID;")

#Profit by month
getRM <- dbGetQuery(conn,"SELECT Receipts.Date, [Price]-[Cost] AS Profit
FROM Products, Receipts;")

#Product histograms
getPH <- dbGetQuery(conn,"SELECT Products.ProductID, Receipts.Date
FROM Products, Receipts;")



png("plotAB.png", width=500, height=500, type="cairo")
avgbasket(getAB)
dev.off()

png("plotAT.png", width=500, height=500, type="cairo")
avgbasket(getAT)
dev.off()

png("plotRM.png", width=500, height=500, type="cairo")
avgbasket(getRM)
dev.off()

png("plotRR.png", width=500, height=500, type="cairo")
avgbasket(getRR)
dev.off()

png("plotGR.png", width=500, height=500, type="cairo")
avgbasket(getGR)
dev.off()

png("plotPH.png", width=1900, height=1080, type="cairo")
avgbasket(getPH)
dev.off()

