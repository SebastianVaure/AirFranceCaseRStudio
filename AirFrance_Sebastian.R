#####################################################
# Air France Business Case#
# Sebastian Valenzuela #
# Data Science - R #
#####################################################
#calling the library to read Excel files
library(readxl)

#Importing the data from the Air France Case, specifically the second sheet of the document
airfrance <- read_excel("Desktop/MsBA/6 - Data Science - R/Datasets/Air France Case Spreadsheet Supplement.xls", 
                        sheet = "DoubleClick")
View(airfrance)
print(airfrance)
summary(airfrance)

#####################################################
###########DATA EXPLORATION & UNDERSTANDING##########
#####################################################
#### What are we trying to solve? ####
#### Optimize performance and ROA dollars spent for SEM campaigns. 


###########BID STRATEGY##############
#Before moving forward, we'll analyze the data and see if there are missing values within
na_values <- colSums(is.na(airfrance))
airfrance$`Bid Strategy`[is.na(airfrance$`Bid Strategy`)] = 'None'
sum(is.na(airfrance))

unique(airfrance$`Bid Strategy`)
airfrance$`Bid Strategy` <- gsub("Position 1-2 Target", " Position 1 -2 Target",
                                 airfrance$`Bid Strategy`)
airfrance$`Bid Strategy` <- gsub("Postiion 1-4 Bid Strategy", "Position 1-4 Bid Strategy",
                                 airfrance$`Bid Strategy`)



###########MATCH TYPE#################


# Looking at the data we realize that for Match Type,Yahoo/Overture uses "Advanced" and "Standard"
# while Google/MSN uses "exact" and "Broad". So that the information is easier to read and understand
#we will only keep two of them: Exact and Broad. 

airfrance$`Match Type` <- gsub('Advanced', 'Exact', airfrance$`Match Type`)
airfrance$`Match Type` <- gsub('Standard', 'Broad', airfrance$`Match Type`)
airfrance$`Match Type` <- gsub('N/A', 'Unknown', airfrance$`Match Type`)
table(airfrance$`Match Type`)

###########PUBLISHER NAME##############

# Looking at our Publishers data, we have Google, MSN, Yahoo and Overture. These last two are the same 
#company, so for clarity we will also join them using gsub. 
airfrance$`Publisher Name` <- gsub('Overture - Global', 'Yahoo - Global', airfrance$`Publisher Name`)
airfrance$`Publisher Name` <- gsub('Overture - US', 'Yahoo - US', airfrance$`Publisher Name`)
table(airfrance$`Publisher Name`)



#####################################################
###########DATA MASSAGING & CLEANING ###############
#####################################################

## Variables that can help us "solve" the case, but are characters:
#Publisher Name, Match Type, Campaign, Category, Bid Strategy, Status
# Converting these into data type: factors

airfrance$'Publisher Name' <- as.factor(airfrance$'Publisher Name')
airfrance$'Match Type' <- as.factor(airfrance$'Match Type')
airfrance$'Campaign' <- as.factor(airfrance$'Campaign')
airfrance$'Category' <- as.factor(airfrance$'Category')
airfrance$`Bid Strategy` <- as.factor(airfrance$`Bid Strategy`)
airfrance$'Status' <- as.factor(airfrance$'Status')

#using the table() function to categorized the columns names after converting into a factor.
table(airfrance$'Publisher Name')
table(airfrance$'Match Type')
table(airfrance$'Campaign')
table(airfrance$'Category')
table(airfrance$`Bid Strategy`)
table(airfrance$'Status')



## creating new columns/variables for the data frame, that will help us have a deeper analysis
#We will exclude 0 from total Cost, to avoid errors
airfrance <- airfrance[which(airfrance$`Total Cost` !=0),]
airfrance$revenue <- airfrance$Amount - airfrance$`Total Cost`


###########CREATING CUSTOM VARIABLES ##############
airfrance$Roa <- airfrance$revenue/airfrance$`Total Cost`#Return on Advertisement
airfrance$rev_bookings <- airfrance$Amount/airfrance$`Total Volume of Bookings`
airfrance$ads_cost_booking <- (airfrance$`Total Cost`/airfrance$`Total Volume of Bookings`)
airfrance$booking_prob <- (airfrance$`Trans. Conv. %` * airfrance$`Engine Click Thru %`)/10000
airfrance$bookings <- ifelse(airfrance$`Total Volume of Bookings`>0, 1, 0)
airfrance[airfrance==""] <- 0
airfrance[airfrance =="Inf"]<- 0
summary(airfrance) #CHECKING OF RESULTS



###########SUBSETTING PUBLISHER NAME ##############
###########US VERSUS GLOBAL ######################
# Subsetting Google
google <- airfrance[grepl('Google',airfrance$'Publisher Name'),]
googleUS <- google[grepl("US", google$'Publisher Name'),]
googleglob <- google[!grepl("US", google$'Publisher Name'),]

#subsetting MSN
msn <- airfrance[grepl('MSN',airfrance$`Publisher Name`),]
msnus <- msn[grepl("US", msn$`Publisher Name`),]
msnglob <- msn[!grepl("US", msn$`Publisher Name`),]

# subsetting Yahoo
yahoo <- airfrance[grepl('Yahoo', airfrance$`Publisher Name`),]
yahoous <- yahoo[grepl("US", yahoo$`Publisher Name`),]
yahooglob <- yahoo[!grepl("US", yahoo$`Publisher Name`),]


###########PUBLISHER NAME & BOOKING PROBABILITIES##############
###########Impressions, Total Cost, and Booking ##############

#Impressions of each one 
msn_impressions <- sum(msn$Impressions, msnus$Impressions, msnglob$Impressions)
google_impressions <- sum(google$Impressions, googleUS$Impressions, googleglob$Impressions)
yahoo_impressions <- sum(yahoo$Impressions, yahoous$Impressions, yahooglob$Impressions)

#Total cost
msn_totalcost <- sum(msn$`Total Cost`, msnus$`Total Cost`, msnglob$`Total Cost`)
google_totalcost <- sum(google$`Total Cost`, googleUS$`Total Cost`, googleglob$`Total Cost`)
yahoo_totalcost <- sum(yahoo$`Total Cost`, yahoous$`Total Cost`, yahooglob$`Total Cost`)

#Total Booking 
msn_totalbooking <- sum(msn$`Total Volume of Bookings`, msnus$`Total Volume of Bookings`, msnglob$`Total Volume of Bookings`)
google_totalbooking <- sum(google$`Total Volume of Bookings`, googleUS$`Total Volume of Bookings`, googleglob$`Total Volume of Bookings`)
yahoo_totalbooking <- sum(yahoo$`Total Volume of Bookings`, yahoous$`Total Volume of Bookings`, yahooglob$`Total Volume of Bookings`)

###########CAMPAIGNS##############

#Subgrouping Geo Targeted Campaigns
airfrance$`Campaign` <- gsub('Geo Targeted Atlanta', 'Geo US', airfrance$`Campaign`)
airfrance$`Campaign` <- gsub('Geo Targeted Boston', 'Geo US', airfrance$`Campaign`)
airfrance$`Campaign` <- gsub('Geo Targeted Chicago', 'Geo US', airfrance$`Campaign`)
airfrance$`Campaign` <- gsub('Geo Targeted DC', 'Geo US', airfrance$`Campaign`)
airfrance$`Campaign` <- gsub('Geo Targeted Detroit', 'Geo US', airfrance$`Campaign`)
airfrance$`Campaign` <- gsub('Geo Targeted Houston', 'Geo US', airfrance$`Campaign`)
airfrance$`Campaign` <- gsub('Geo Targeted Los Angeles', 'Geo US', airfrance$`Campaign`)
airfrance$`Campaign` <- gsub('Geo Targeted Miami', 'Geo US', airfrance$`Campaign`)
airfrance$`Campaign` <- gsub('Geo Targeted New York', 'Geo US', airfrance$`Campaign`)
airfrance$`Campaign` <- gsub('Geo Targeted Philadelphia', 'Geo US', airfrance$`Campaign`)
airfrance$`Campaign` <- gsub('Geo Targeted San Francisco', 'Geo US', airfrance$`Campaign`)
airfrance$`Campaign` <- gsub('Geo Targeted Seattle', 'Geo US', airfrance$`Campaign`)

#Campaigns Total Revenue
total_revenue_GeoUS <- with(airfrance, sum(revenue[Campaign == 'Geo US']))
total_revenue_AirFrance <- with(airfrance, sum(revenue[Campaign == 'Air France Brand & French Destinations']))
total_revenue_AirFranceBranded <- with(airfrance, sum(revenue[Campaign == 'Air France Branded']))
total_revenue_AirFranceGlobal <- with(airfrance, sum(revenue[Campaign == 'Air France Global Campaign']))
total_revenue_BusinessClass <- with(airfrance, sum(revenue[Campaign == 'Business Class']))
total_revenue_FrenchDestinations <- with(airfrance, sum(revenue[Campaign == 'French Destinations']))
total_revenue_GeneralTerms <- with(airfrance, sum(revenue[Campaign == 'General Terms']))
total_revenue_Google2006 <- with(airfrance, sum(revenue[Campaign == 'Google_Yearlong 2006']))
total_revenue_ParisFrance <- with(airfrance, sum(revenue[Campaign == 'Paris & France Terms']))
total_revenue_OutsideWestern <- with(airfrance, sum(revenue[Campaign == 'Outside Western Europe']))
total_revenue_Unassigned <- with(airfrance, sum(revenue[Campaign == 'Unassigned']))
total_revenue_WesternEurope <- with(airfrance, sum(revenue[Campaign == 'Western Europe Destinations']))

airfrance$TotalBookings <- airfrance$`Total Volume of Bookings`

#Campaigns Total Bookings
total_bookings_GeoUS <- with(airfrance, sum(TotalBookings[Campaign == 'Geo US']))
total_bookings_AirFrance <- with(airfrance, sum(TotalBookings[Campaign == 'Air France Brand & French Destinations']))
total_bookings_AirFranceBranded <- with(airfrance, sum(TotalBookings[Campaign == 'Air France Branded']))
total_bookings_AirFranceGlobal <- with(airfrance, sum(TotalBookings[Campaign == 'Air France Global Campaign']))
total_bookings_BusinessClass <- with(airfrance, sum(TotalBookings[Campaign == 'Business Class']))
total_bookings_FrenchDestinations <- with(airfrance, sum(TotalBookings[Campaign == 'French Destinations']))
total_bookings_GeneralTerms <- with(airfrance, sum(TotalBookings[Campaign == 'General Terms']))
total_bookings_Google2006 <- with(airfrance, sum(TotalBookings[Campaign == 'Google_Yearlong 2006']))
total_bookings_ParisFrance <- with(airfrance, sum(TotalBookings[Campaign == 'Paris & France Terms']))
total_bookings_OutsideWestern <- with(airfrance, sum(TotalBookings[Campaign == 'Outside Western Europe']))
total_bookings_Unassigned <- with(airfrance, sum(TotalBookings[Campaign == 'Unassigned']))
total_bookings_WesternEurope <- with(airfrance, sum(TotalBookings[Campaign == 'Western Europe Destinations']))





## Creating a new data frame with only the variables that would be important (according to me)
# the ones changed into factors + clicks, Avg. Cost per Click, Impressions, Total Cost & Total Volume of Bookings

new_airfrance <- airfrance[, c("Publisher Name", "Match Type", "Campaign", "Category", "Bid Strategy",
                               "Status", "Clicks", "Avg. Cost per Click", "Impressions", "Engine Click Thru %",
                               "Total Cost/ Trans." ,"Amount","Total Cost", "Total Volume of Bookings", "revenue",
                               "Roa", "rev_bookings", "ads_cost_booking", "booking_prob", 'bookings')]

new_airfrance <- as.data.frame(new_airfrance)

#####################################################
###########    DESCRIPTIVE & PREDICTIVE ANALYSIS   ###############
#####################################################

#1.) How much revenue generated by each publisher name based on campaign match type? 

#### Revenue per publisher
googleus_revenue <- c()
googleglob_revenue <- c()
msnus_revenue <- c()
msnglob_revenue <- c()
yahoous_revenue <- c()
yahooglob_revenue <- c()

matching_type <- c("Exact", "Broad")

i <- 1


while (i<= length(matching_type)){
  googleus_revenue <- c(googleus_revenue, mean(new_airfrance$revenue[which(new_airfrance$`Publisher Name` == "Google - US" &
                                                                             new_airfrance$`Match Type`== matching_type[i])],na.rm=TRUE))
  googleglob_revenue <- c(googleglob_revenue, mean(new_airfrance$revenue[which(new_airfrance$`Publisher Name` == "Google - Global" &
                                                                                 new_airfrance$`Match Type`== matching_type[i])],na.rm=TRUE))
  msnus_revenue <- c(msnus_revenue, mean(new_airfrance$revenue[which(new_airfrance$`Publisher Name` == "MSN - US" &
                                                                       new_airfrance$`Match Type`== matching_type[i])],na.rm=TRUE))
  msnglob_revenue <- c(msnglob_revenue, mean(new_airfrance$revenue[which(new_airfrance$`Publisher Name` == "MSN - Global" &
                                                                           new_airfrance$`Match Type`== matching_type[i])],na.rm=TRUE))
  yahoous_revenue <- c(yahoous_revenue, mean(new_airfrance$revenue[which(new_airfrance$`Publisher Name` == "Yahoo - US" &
                                                                           new_airfrance$`Match Type`== matching_type[i])],na.rm=TRUE))
  yahooglob_revenue <- c(yahooglob_revenue, mean(new_airfrance$revenue[which(new_airfrance$`Publisher Name` == "Yahoo - Global" &
                                                                               new_airfrance$`Match Type`== matching_type[i])],na.rm=TRUE))
  i <- i+1
  
}#closing while loop for revenue

revenue_per_SE <- cbind( googleus_revenue, googleglob_revenue, msnus_revenue, msnglob_revenue, 
                         yahoous_revenue, yahooglob_revenue)

rownames(revenue_per_SE) <- matching_type
print(revenue_per_SE)

##In this case, Google in general generates the most revenue for exact campaigns 
##among the rest of the publishers. 
## This says a lot of about its brand identity being present in the U.S and globally 



#2.) What is the ROA per publisher based on match type? 

googleus_Roa <- c()
googleglob_Roa <- c()
msnus_Roa <- c()
msnglob_Roa <- c()
yahoous_Roa <- c()
yahooglob_Roa <- c()

i <- 1

while (i<= length(matching_type)){
  googleus_Roa <- c(googleus_Roa, mean(new_airfrance$Roa[which(new_airfrance$`Publisher Name` == "Google - US" &
                                                                 new_airfrance$`Match Type`== matching_type[i])]))
  googleglob_Roa <- c(googleglob_Roa, mean(new_airfrance$Roa[which(new_airfrance$`Publisher Name` == "Google - Global" &
                                                                     new_airfrance$`Match Type`== matching_type[i])]))
  msnus_Roa <- c(msnus_Roa, mean(new_airfrance$Roa[which(new_airfrance$`Publisher Name` == "MSN - US" &
                                                           new_airfrance$`Match Type`== matching_type[i])]))
  msnglob_Roa <- c(msnglob_Roa, mean(new_airfrance$Roa[which(new_airfrance$`Publisher Name` == "MSN - Global" &
                                                               new_airfrance$`Match Type`== matching_type[i])]))
  yahoous_Roa <- c(yahoous_Roa, mean(new_airfrance$Roa[which(new_airfrance$`Publisher Name` == "Yahoo - US" &
                                                               new_airfrance$`Match Type`== matching_type[i])]))
  yahooglob_Roa <- c(yahooglob_Roa, mean(new_airfrance$Roa[which(new_airfrance$`Publisher Name` == "Yahoo - Global" &
                                                                   new_airfrance$`Match Type`== matching_type[i])]))
  i <- i+1
  
}

ROA_SEM <- cbind( googleus_Roa, googleglob_Roa, msnus_Roa, msnglob_Roa, 
                  yahoous_Roa, yahooglob_Roa)

rownames(ROA_SEM) <- matching_type
print(ROA_SEM)


#3.) Which publisher name has the highest generated revenue per booking?

googleus_rev_book <- c()
googleglob_rev_book <- c()
msnus_rev_book <- c()
msnglob_rev_book <- c()
yahoous_rev_book <- c()
yahooglob_rev_book <- c()

i <- 1

while (i<= length(matching_type)){
  googleus_rev_book <- c(googleus_rev_book, mean(new_airfrance$rev_bookings[which(new_airfrance$`Publisher Name` == "Google - US" &
                                                                                    new_airfrance$`Match Type`== matching_type[i])]))
  googleglob_rev_book <- c(googleglob_rev_book, mean(new_airfrance$rev_bookings[which(new_airfrance$`Publisher Name` == "Google - Global" &
                                                                                        new_airfrance$`Match Type`== matching_type[i])]))
  msnus_rev_book <- c(msnus_rev_book, mean(new_airfrance$rev_bookings[which(new_airfrance$`Publisher Name` == "MSN - US" &
                                                                              new_airfrance$`Match Type`== matching_type[i])]))
  msnglob_rev_book <- c(msnglob_rev_book, mean(new_airfrance$rev_bookings[which(new_airfrance$`Publisher Name` == "MSN - Global" &
                                                                                  new_airfrance$`Match Type`== matching_type[i])]))
  yahoous_rev_book <- c(yahoous_rev_book, mean(new_airfrance$rev_bookings[which(new_airfrance$`Publisher Name` == "Yahoo - US" &
                                                                                  new_airfrance$`Match Type`== matching_type[i])]))
  yahooglob_rev_book <- c(yahooglob_rev_book, mean(new_airfrance$rev_bookings[which(new_airfrance$`Publisher Name` == "Yahoo - Global" &
                                                                                      new_airfrance$`Match Type`== matching_type[i])]))
  i <- i+1
  
}

Rev_booking_SEM <- cbind( googleus_rev_book, googleglob_rev_book, msnus_rev_book, msnglob_rev_book, 
                          yahoous_rev_book, yahooglob_rev_book)

rownames(Rev_booking_SEM) <- matching_type
print(Rev_booking_SEM)

colSums(is.na(airfrance))

#4.) What is our business success? Creating dummy variables for ROA_Average for logistic regression.

Roa_average <- sum(new_airfrance$Amount)/sum(new_airfrance$`Total Cost`)
for (i in 1:nrow(new_airfrance)){
  
  if(new_airfrance[i, "Roa"] > Roa_average){
    
    new_airfrance$binary_success_Roa[i] <- 1
    
  }
  else if (new_airfrance[i,"Roa"] < Roa_average){
    new_airfrance$binary_success_Roa[i] <- 0
  }
}#end loop



#5.) How much is the total revenue generated per campaign?
#Observing Total Revenue of each campaign 
airfrance <- airfrance[which(airfrance$`Total Cost` !=0),]
airfrance$revenue <- airfrance$Amount - airfrance$`Total Cost`
airfrance$Roa <- airfrance$revenue/airfrance$`Total Cost`
airfrance$rev_bookings <- airfrance$Amount/airfrance$`Total Volume of Bookings`
airfrance$ads_cost_booking <- airfrance$`Total Cost`/airfrance$`Total Volume of Bookings`
airfrance$booking_prob <- (airfrance$`Trans. Conv. %` * airfrance$`Engine Click Thru %`)/100000


#6.) Which publisher generated more total bookings and revenue through click through rates?
## creating a loop to get total values for each Publisher

publishers <- unique(new_airfrance$`Publisher Name`)
publishers

publisher_total_revenue <- c()
publisher_total_clicks <- c()
publisher_total_costs <- c()
publisher_total_bookings <- c()


for (i in 1:length(publishers)) {
  publisher_total_revenue <- c(publisher_total_revenue,sum(new_airfrance$revenue[which(new_airfrance[,1] == publishers[i])]))
  print(publisher_total_revenue)
  publisher_total_clicks <- c(publisher_total_clicks,sum(new_airfrance$Clicks[which(new_airfrance[,1]==publishers[i])]))
  publisher_total_costs<- c(publisher_total_costs,sum(new_airfrance$`Total Cost`[which(new_airfrance[,1]==publishers[i])]))
  publisher_total_bookings <- c(publisher_total_bookings,sum(new_airfrance$`Total Volume of Bookings`[which(new_airfrance[,1]==publishers[i])]))
  
}#ending loop


airfrance_matrix <- matrix( c(publishers, publisher_total_revenue, publisher_total_clicks, 
                              publisher_total_costs, publisher_total_bookings), ncol= 5, nrow= 6)

overview_publishers <- as.data.frame(airfrance_matrix)

colnames(x= overview_publishers) <-c("Publishers", "Total Revenue", "Total Clicks",
                                     "Total Costs", "Total bookings")


#7.) What are the factors that affects the success of campaign?

#create a new target variable "Ads_success" to classify whether our campaign is profitable or lossing money
airfrance$Ads_success <- c()
for (i in 1:nrow(airfrance)){
  if(airfrance$revenue[i]>0){airfrance$Ads_success[i] <- "1"}
  else{airfrance$Ads_success[i] <- "0"}
}
airfrance$Ads_success <- as.numeric(airfrance$Ads_success)

#create dummy variables for "publishers name" and "campaign" 

airfrance$Google_US <- ifelse(airfrance$`Publisher Name` == 'Google - US', 1, 0)
airfrance$MSN_Global <- ifelse(airfrance$`Publisher Name` == 'MSN - Global', 1, 0)
airfrance$MSN_US <- ifelse(airfrance$`Publisher Name` == 'MSN - US', 1, 0)
airfrance$Yahoo_Global <- ifelse(airfrance$`Publisher Name` == 'Yahoo - Global', 1, 0)
airfrance$Yahoo_US <- ifelse(airfrance$`Publisher Name` == 'Yahoo - US', 1, 0)

airfrance <- as.data.frame(airfrance)

airfrance$CTR_norm <- normalize(airfrance$`Engine Click Thru %`)
airfrance$CR_norm <- normalize(airfrance$`Trans. Conv. %`)
airfrance$booking_prob_norm <- normalize(airfrance$booking_prob)

# Sampling for train and test
training_idx <- sample(1:nrow(airfrance), size=0.8*nrow(airfrance))
AF_train <- airfrance[training_idx,]
AF_test <- airfrance[-training_idx,]



#using logistic regression model to analyze that how much impact each variables can effect on the target
#Unit
unit_logit <- glm(Ads_success ~ Google_US+MSN_Global+MSN_US+Yahoo_Global+Yahoo_US+
                    `Engine Click Thru %`+`Trans. Conv. %`+booking_prob, 
                  data= AF_train, family= "binomial")
summary(unit_logit)

#Unitless
unitless_logit <- glm(Ads_success ~ Google_US+MSN_Global+MSN_US+Yahoo_Global+Yahoo_US+
                        CTR_norm+CR_norm+booking_prob_norm, 
                      data= AF_train, family= "binomial")
summary(unitless_logit)


#####################################################
###########    VISUALIZATION AND INSIGHTS  ###############
#####################################################

## creating plots to see correlation between variables
library(ggplot2)
library(plotly)

publisher_booking_prob <- ggplot(new_airfrance, aes(x=`Publisher Name`, y=`booking_prob`)) +
  stat_sum(geom="bar", fill="blue")
ggplotly(publisher_booking_prob)


ggplot()+geom_bar(data=new_airfrance, aes(x=`Publisher Name`))
## we can observe that the Publisher that has the more booking prob is Google US

# ROA
Roa_plot <- ggplot(data=new_airfrance, aes(Roa)) + geom_histogram(fill='red', color='white', na.rm=TRUE, bins=30)+
  xlim(0,500) + ylim(0,70)
Roa_plot

# clicks
clicks_airfrance <- ggplot(new_airfrance, aes(x=`Clicks`, y=`Total Volume of Bookings`, color=`Publisher Name`))+geom_point()


ggplot(data=new_airfrance, aes(x=Roa, y='Match Type'))+
  geom_point()

ggplot(data=new_airfrance, aes(x='Avg. Cost per Click', y=Roa, color='Publisher Name'))+
  geom_jitter()

my_scatter_airfrance <- ggplot(data=new_airfrance, aes(x='Avg. Cost per Click', y=Roa, color='Publisher Name'))+
  geom_point()+ geom_smooth()+scale_color_manual(values=c("#B81CEE", "#0Af0D4"))
my_scatter_airfrance


ggplotly(my_scatter_airfrance)

publishers <- ggplot(new_airfrance, aes(x=`Publisher Name`, y= `Total Cost`)) + 
  stat_summary(geom="bar", fill="green")
ggplotly(publishers)

cost_booking_publisher <- ggplot(new_airfrance, aes(x=`Publisher Name`, y=`ads_cost_booking`)) +
  stat_sum(geom="bar", fill="light blue")
ggplotly(cost_booking_publisher)

campaign <- ggplot(new_airfrance, aes(x=`Campaign`, y=`revenue`, fill=`Publisher Name`)) +
  geom_bar(position="Dodge", stat="identity", size=10)+coord_flip()
ggplotly(campaign)

impressions_publisher <- ggplot(new_airfrance, aes(x=`Publisher Name`, y=`Impressions`))+
  stat_sum(geom="bar", fill="red")
ggplotly(impressions_publisher)



###########NORMALIZING##############
## Normalizing the columns in new_airfrance with a UDF

normalize <- function(var){
  my_norm <- (var-min(var))/(max(var)-min(var))
  return(my_norm)
} #closing the UDF

new_airfrance$Clicks_norm <- normalize(var=new_airfrance$Clicks)
new_airfrance$avg_cost_click_norm <- normalize(var=new_airfrance$`Avg. Cost per Click`)
new_airfrance$Impressions_norm <- normalize(var=new_airfrance$Impressions)
new_airfrance$Amount_norm <-normalize(var=new_airfrance$Amount)
new_airfrance$total_cost_norm <- normalize(var=new_airfrance$`Total Cost`)
new_airfrance$total_volume_bookings_norm <- normalize(var=new_airfrance$`Total Volume of Bookings`)
new_airfrance$revenue_norm <- normalize(var=new_airfrance$revenue)
new_airfrance$Roa_norm <- normalize(var=new_airfrance$Roa)
new_airfrance$rev_bookings_norm <- normalize(var=new_airfrance$rev_bookings)
new_airfrance$ads_cost_booking_norm <- normalize(var=new_airfrance$ads_cost_booking)
new_airfrance$booking_prob_norm <- normalize(var=new_airfrance$booking_prob)


# Sampling for train and test
training_idx_AF <- sample(1:nrow(new_airfrance), size=0.8*nrow(new_airfrance))
airfrance_train <- new_airfrance[training_idx_AF,]
airfrance_test <- new_airfrance[-training_idx_AF,]


###########LINEAR REGRESSIONS##############

## LINEAR REGRESSION
AF_linear <- lm(revenue ~ Clicks+Impressions+booking_prob, data=airfrance_train)
summary(AF_linear)

#Logistic regression 
AF_logit <- glm(binary_success_Roa ~ `Bid Strategy` + `Total Volume of Bookings`+ `Publisher Name`,
                data=airfrance_train, family="binomial")

summary(AF_logit)
exp(-1.74688)-1


AF_logit2 <- glm(bookings ~ `Clicks`+ `Avg. Cost per Click` + `Impressions`,
                 data = airfrance_train, family = "binomial")

summary(AF_logit2)

AF_logit3 <- glm(binary_success_Roa ~ `Bid Strategy`+Clicks+Impressions+`Engine Click Thru %`, 
                 `Total Cost/ Trans.`, data=airfrance_train, family="binomial")

summary(AF_logit3)
# Logistic regression with normalized data
AF_logit_norm <- glm(bookings ~ Clicks_norm +avg_cost_click_norm + Impressions_norm,
                     data=airfrance_train, family="binomial")

###########CONFUSION MATRIX##############

#creating confusion matrix for training(3607 observations):
library(caret)

my_prediction_training <- predict(unit_logit, AF_train,
                                  type="response")
confusionMatrix(data= as.factor(as.numeric(my_prediction_training>0.5)),
                reference= as.factor(as.numeric(AF_train$Ads_success)))


#creating confusion matrix for testing(902 observations):
my_prediction_testing <- predict(unitless_logit, AF_test,
                                 type="response")
confusionMatrix(data= as.factor(as.numeric(my_prediction_testing>0.5)),
                reference= as.factor(as.numeric(AF_test$Ads_success)))

# creating an AUC ROC for unit_logit
library(ROCR)

pred_val_logit <- prediction(my_prediction_testing, AF_test$Ads_success)

perf_logit <- performance(pred_val_logit, "tpr", "fpr")

plot(perf_logit)

###########GINI TREES##############

# creating gini trees for Airfrance case
library(rpart)
library(rpart.plot)

my_tree <- rpart(Ads_success ~ `Publisher Name`+Campaign+`Engine Click Thru %`+`Trans. Conv. %`+booking_prob, 
                 data= AF_train, method="class",
                 cp=0.001)
rpart.plot(my_tree, type=1, extra=1)

#using this tree to predict on testing customers
AF_tree_predict <- predict(my_tree, AF_test,
                           type="prob")
AF_tree_prediction <- prediction(AF_tree_predict[,2],
                                 AF_test$Ads_success)
my_tree_perf <- performance(AF_tree_prediction,
                            "tpr","fpr")
plot(perf_logit, col="blue")
plot(my_tree_perf, col="green4", add=TRUE)








#######################################
## KAYAK COMPARISON ##
#######################################
library(readxl)
Kayak <- read_excel("Desktop/MsBA/6 - Data Science - R/Datasets/Air France Case Spreadsheet Supplement.xls", 
                                                     sheet = "Kayak")
View(Kayak)




# Changing variable names of Kayak df with colnames
colnames(Kayak) <- c("publisher_name", "clicks", "media_cost","total_bookings", 
                     "avg_ticket", "total_revenue", "net_revenue","total_cost")

#for loops 
for(i in 2:7){
  Kayak[,i] <- as.numeric(Kayak[,i])
} # closing for loop

summary(overview_publishers)
sum(overview_publishers$`Total Costs`) #Ad Cost Budget Allocation



### Graphing for kayak ###

plot_ly(data=Kayak, x=~overview_publishers, y=~overview_publishers$`Total Revenue`, name="Total Revenue

        ",
        marker = list(color = 'rgb(200,86,87)'),type="bar") %>%
  add_trace(y = ~overview_publishers$`Total Costs`, name = 'Total Cost
            ',
            marker = list(color = 'rgb(242,217,187)'),type="bar") %>%
  add_trace(y = ~overview_publishers$`Total Clicks`, name = 'Ad Spend per Click
            ',
            marker = list(color = 'rgb(99,143,169)'),type="bar") %>%
  layout(xaxis = list(title = "Kayak vs. Publishers", tickangle = -45),
         yaxis = list(title = ""),
         margin = list(b = 100),
         barmode = 'group',
         legend = list(x = 1.1, y = 1))












