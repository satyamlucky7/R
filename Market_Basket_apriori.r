library(arules)
library(arulesViz)
library(datasets)
library(dplyr)
library(tidyr)
# Load the libraries for the session


# Read data from you saved location. Please change the address as per you file's saved path
Basket <- read.csv('C:/Users/Satyam.Anand/Desktop/Market Basket/Basket_All.csv')

# Remove SalesTerritoryKey variable as it is redundant and needs to be dropped
Basket = subset(Basket, select = -c(1))

# Install "plyr" only if it is not installed. Also, Make sure that you do not have package ‘dplyr’ attached to the session.
install.packages ("plyr", dependencies =  TRUE)
if(sessionInfo()['basePkgs']=="dplyr" | sessionInfo()['otherPkgs']=="dplyr"){
  detach(package:dplyr, unload=TRUE)
}

# load plyr
library(plyr)

# This is the actual method where we are bringing the data in desired format using SalesOrderNumber. Here, all the products sold with
# same order number will be concatenated in one row with , as seperator
df_itemList <- ddply(Basket,c("SalesOrderNumber"), 
                     function(df1)paste(df1$Product.Name, 
                                        collapse = ","))

# Now since we dont need SalesOrderNumber variable we can drop it using below command
Basket2 = subset(df_itemList, select = -c(1))

# We need to save the file in csv format as by default it adds a row number for each row that we can use as transation IDs as they would be unique
write.csv(Basket2,'C:/Users/Satyam.Anand/Desktop/Market Basket/Basket_All_2.csv', quote = FALSE, row.names = TRUE)

# Using read.transations we can read the csv file and convert it into transations
txn = read.transactions(file="C:/Users/Satyam.Anand/Desktop/Market Basket/Basket_All_2.csv", rm.duplicates= TRUE, format="basket",sep=",",cols=1);

# Run the apriori algorithm on the transations by specifying confidence and spport minimum value
basket_rules <- apriori(txn,parameter = list(sup = 0.01, conf = 0.8,target="rules"));


# Inspect rules with confidence higher than 80% and support higher than 0.01
inspect(basket_rules)


# Some visualizations to play with
library(arulesViz)
plot(basket_rules)
plot(basket_rules, method = "grouped", control = list(k = 5))
plot(basket_rules, method="graph", control=list(type="items"))
plot(basket_rules, method="paracoord",  control=list(alpha=.5, reorder=TRUE))
plot(basket_rules,measure=c("support","lift"),shading="confidence",interactive=T,jitter = 0)


itemFrequencyPlot(txn, topN = 5)

