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

# run your QUERY (i.e., SELECT statement), the result is a data.frame
#Retail and revenue
        getRetailBar <- dbGetQuery(conn, "SELECT Receipts.RetailerID, Products.Price
                                FROM Receipts, Products;")
 #Region and revenue 
        getRegionBar <- dbGetQuery(conn, "SELECT Distributors.Region, Products.Price
                           FROM Distributors, Products;")
 #Revenue by date
        getLinReg <- dbGetQuery(conn, "SELECT Receipts.Date, Products.Price
                        FROM Receipts, Products
                        ORDER BY Receipts.Date;")
 #Retail and profit
        #getRetailprof <- dbGetQuery(conn, "SELECT Receipts.RetailerID, [Price]-[Cost] AS amount
        #                 FROM Receipts, Products;")
 #Region and profit
        #getRegionprof <- dbGetQuery(conn, "SELECT Distributors.Region , [Price]-[Cost] AS amount
         #                FROM Distributors, Products;")

        getProductReg <- dbGetQuery(conn, "SELECT Receipts.Date, Products.ProductID
                        FROM Receipts, Products
                        ORDER BY Receipts.Date;")






#Receives a table with 2 columns, retailer and amount
#Creates a bar plot for revenue by Retailer ID
histmakerRetail <- function(directory){  
  
  aggdata = aggregate(directory$Price, list(directory$RetailerID), FUN=sum)
  #aggfix = cbind(aggdata[1], aggdata[2])
  rowid = aggdata[,1]
  rev = aggdata[,2]
  colnames(aggdata) <- c("retailer ID", "revenue")
  barplot(aggdata[,2], 
          col = c("lightblue", "mistyrose",
                  "lightcyan", "lavender"),
          
          xlab = colnames(aggdata[1]),
          ylab = colnames(aggdata[2]),
          names.arg = rowid)
}

#Receives a table with 2 columns, region and amount
#Creates a bar plot for revenue by Region
histmakerRegion <- function(directory, x){   
  
  
  #combines the amount based on retailer ID
  aggdata = aggregate(directory$Price, list(directory$Region), FUN=sum)
  
  #Takes away unnecssary column and fixes format
  aggfix = cbind(aggdata[1], aggdata[2])
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
  directory$daten <- as.Date(directory$Date, format = "%d/%m/%Y")
  
  #adds month and year column
  directory$year <- strftime(directory$daten, format="%Y")
  directory$month <- strftime(directory$daten, format="%m")
  
  #create revenue by date
  aggr.stat = ddply(directory, .(year, month), function(x) sum(x$Price))
  
  
  #plot the original data points with larger dots for more freq pts
  y <- aggr.stat$V1
  x <- as.numeric(aggr.stat$month)
  
  plot(x, y, type = "p",
       pch = 21, col = "black", bg = "lightblue",
       xlab = "month", ylab = "revenue [USD]")
  
  #original regression line, revenue as outcome, month as predictor
  abline(lm(y~x), #regression line 
         lwd = 3, col = "red")
  Data <- data.frame(x, y)
  loess_fit <- loess(y ~ x, Data)
  lines(Data$x, predict(loess_fit),lwd = 3, col = "blue")
  
}


#Creates a plot for product vs time
seasonProducts <- function(directory){
        directory$daten <- as.Date(directory$Date, format = "%d/%m/%Y")
  
  #creates a shortdate column year/month
  directory$shortdate <- strftime(directory$daten, format="%Y/%m")
  
  tempdf <- as.data.table(directory)
  
  #counts product ID's for each unique shortdate
  setkey(tempdf, shortdate, ProductID)
  tempdt <- tempdf[CJ(unique(shortdate), unique(ProductID)), .N, by = .EACHI]
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

#Creates plot png files
png("plot-1.png", width=500, height=500, type="cairo")
histmakerRetail(getRetailBar)
dev.off()

png("plot-2.png", width=500, height=500, type="cairo")
histmakerRegion(getRegionBar)
dev.off()

png("plot-3.png", width=500, height=500, type="cairo")
plotmaker(getLinReg)
dev.off()

png("plot-4.png", width=500, height=500, type="cairo")
print(seasonProducts(getProductReg))
dev.off()

#png("plot-5.png", width=500, height=500, type="cairo")
#histmakerRetail(getRetailprof)
#dev.off()

#png("plot-6.png", width=500, height=500, type="cairo")
#histmakerRegion(getRegionprof)
