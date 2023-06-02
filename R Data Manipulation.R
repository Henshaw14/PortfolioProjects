##############################

install.packages("tidyverse")  
install.packages("lubridate")     # for Month and Data manipulation and extraction
install.packages("GGally")
install.packages("dplyr")         # for data mannipulation
install.packages('ggplot2')
install.packages('corrplot')
install.packages("gridExtra")      #  for converting the y scale to percentage
install.packages("factoextra")    
install.packages("vis_miss")
install.packages("mclust")
install.packages("caret")         # For data partitioning and other machine learning tools
install.packages("rpart")
install.packages("rpart.plot")
install.packages("rattle")
install.packages("visdat")
install.packages("skimr")
install.packages("devtools")
devtools::install_github("ropensci/visdat")
install.packages("class")
install.packages("tree")
install.packages("e1071")
install.packages("ggstatsplot")
install.packages("randomForest")    # for building random forest model
library(randomForest)
library(e1071)
library(tree)
library(rpart.plot)
library(class)
library(visdat)
library(tidyverse)
library(devtools)
library(mclust)
library(caret)
library(rpart)
library(rattle)
library(skimr)
library(lubridate)
library(dplyr)
library(ggplot2)
library(GGally)
library(corrplot)
library(scales)
library(factoextra)
library(visdat)
library(ggstatsplot)


getwd()

setwd('C:/Users/s4110499/Desktop/HMG')
#setwd("N:/HenshawData")

#setwd("C:/Users/hensh/OneDrive/Desktop/Documents/Assignment Data")
#setwd("./Assignment Data")


####################################################
# DATA IMPORTATION AND INTEGRATION
####################################################
file_names <- list.files()
file_number <- length(file_names)

# creating a empty matrix of 0X0 to hold our data
empty_matrix <- matrix(ncol = 0, nrow = 0)
# convert the empty matrix to data frame
df <- data.frame(empty_matrix)

for (i in 1 : file_number){
  files <- read.csv(file_names[i])
  files$date <- rep(file_names[i], nrow(files))
  files <- data.frame(lapply(files, as.character))
  df <- bind_rows(df, files)
}

# creating another empty matrix because
# we wish to extract month and year variable from the filenames
empty_matrix2 <- matrix(ncol = 0, nrow = 0)
df1 <- data.frame(empty_matrix2)

for (i in 1 : nrow(df)){
  lfn <- df$date[i]
  date_v <- as.data.frame(substr(lfn, 28, nchar(lfn)))
  df1 <- bind_rows(df1, date_v)
}

df1$`substr(lfn, 28, nchar(lfn))`

df$date <- my(df1$`substr(lfn, 28, nchar(lfn))`)
final_df <- df

# cleaning the data (removing "%", "-", " ")
clean<- function(x){
  gsub("%","",x)
}
final_df1 <- lapply(final_df, clean)
final_df1 <- as.data.frame(final_df1)

clean1 <- function(x){
  gsub(",","",x)
}
final_df1 <- lapply(final_df1, clean1)
final_df1 <- as.data.frame(final_df1)

clean2 <- function(x){
  gsub("-","0.0",x)
}
final_df1 <- lapply(final_df1, clean2)
final_df2 <- as.data.frame(final_df1)

final_df2$date <- df$date

final_df3 <- final_df2[-c(1,52)]
class(final_df3)

# Make data numeric
POC <-as.data.frame(lapply(final_df3, as.numeric))
POC$County <- df$X
POC$date <- df$date



# Add month and year variable
POC$month <- month.abb[month(as.POSIXlt(POC$date, format="Y-%m-%d"))]
POC$year <- year(as.POSIXlt(POC$date, format="Y-%m-%d"))



POC
dim(POC)
head(POC)
colnames(POC)








######################################################################
####### 2.  DATA CLEANING
######################################################################



##  removing the percentage column
POC_df <- POC %>%
  select(County,date,month,year,starts_with("Number"),)



colnames(POC_df)



######################################################################
####### Renaming the variable names since they are too long
######################################################################
names(POC_df)[2:29] <- c("Date", "Month", "Year", "Homicide", "Homicide_Unsuccessful", "Offences_Against_The_Person", "Offences_Against_The_Person_Unsuccessful", "Sexual_Offences",
                         "Sexual_Offence_Unsuccessful", "Burglary", "Burglary_Unsuccessful", "Robbery", "Robbery_Unsuccessful", "Theft_And_Handling","Theft_And_Handling_Unsuccessful",
                         "Fraud_And_Forgery", "Fraud_And_Forgery_Unsuccessful", "Criminal_Damage", "Criminal_Damage_Unsuccessful", "Drugs","Drugs_Unsuccessful","Public_Order", "Public_Order_Unsuccessful",
                         "All_Other_Offences","All_Other_Offences_Unsuccessful","Motoring_Offences", "Motoring_Offences_Unsuccessful", "Admin_Finalised_Unsuccessful")



str(POC_df)   #Viewing the structure of the data set
dim(POC_df)   # See the dimension of the entire data
skim(POC_df)  # An overview of the data
glimpse(POC_df)
colnames(POC_df)
summary(POC_df)



#A graph to visualize the number of missing data in the data set
vis_miss(POC_df)



#A plot to visualize the class of value in the data set
vis_dat(POC_df)



# Mutating the column in my data set to create a new column using the dplyr function "MUTATE"
#Removing all the row with "National", since it is total and will affect our analysis
POC_Ndf <- POC_df %>%
  filter(County != "National") %>%
  mutate(Total_Number_of_Crime = Homicide + Offences_Against_The_Person +Sexual_Offences + Burglary +
           Robbery + Theft_And_Handling + Fraud_And_Forgery + Criminal_Damage + Drugs + Public_Order +
           All_Other_Offences + Motoring_Offences + Admin_Finalised_Unsuccessful)

####################################
# County with the highest crime 2014
####################################
POC_Ndf %>%
  group_by(County) %>%
  filter(Year == 2014) %>%
  ggplot(aes(x = reorder(County,+Total_Number_of_Crime), y=Total_Number_of_Crime, fill=Month)) + geom_col() +
  ggtitle("Highest Crime by County in 2014") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("County") +
  ylab("Total Crime") +
  coord_flip()


#####################################
# County with the highest crime 2015
#####################################
POC_Ndf %>%
  group_by(County) %>%
  filter(Year == 2015) %>%
  ggplot(aes(x = reorder(County,+Total_Number_of_Crime), y=Total_Number_of_Crime, fill=Month)) + geom_col() +
  ggtitle("Highest Crime by County in 2015") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("County") +
  ylab("Total Crime") +
  coord_flip()


#####################################
# County with the highest crime 2016
#####################################
POC_Ndf %>%
  group_by(County) %>%
  filter(Year == 2016) %>%
  ggplot(aes(x = reorder(County,+Total_Number_of_Crime), y=Total_Number_of_Crime, fill=Month)) + geom_col() +
  ggtitle("Highest Crime by County in 2016") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("County") +
  ylab("Total Crime") +
  coord_flip()

#####################################
# County with the highest crime 2017
#####################################
POC_Ndf %>%
  group_by(County) %>%
  filter(Year == 2017) %>%
  ggplot(aes(x = reorder(County,+Total_Number_of_Crime), y=Total_Number_of_Crime, fill=Month)) + geom_col() +
  ggtitle("Highest Crime by County in 2017") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("County") +
  ylab("Total Crime") +
  coord_flip()

View(POC_Ndf)



######################################################################
# 2.  Descriptive Analysis (Data Exploration)
######################################################################



summary(POC_Ndf)  # Performing Basic Statistics



# Average crime rate distributed monthly per 2014
colnames(POC_Ndf)

WORD <- POC_Ndf %>%
  filter(Year==2014) %>%
  group_by(Month) %>%
  summarise(Avg_Homicide = mean(Homicide),
            Avg_Criminal_Damage = mean(Criminal_Damage),
            Avg_Fraud_And_Forgery = mean(Fraud_And_Forgery),
            Avg_Offense_against_person = mean(Offences_Against_The_Person),
            Avg_Drugs = mean(Drugs),
            Avg_Sexual_Offence = mean(Sexual_Offences),
            Avg_Public_Order = mean(Public_Order),
            Avg_Burglary = mean(Burglary),
            Avg_Motoring_Offences = mean(Motoring_Offences),
            Avg_Robbery = mean(Robbery),
            Avg_Theft_And_Handling = mean(Theft_And_Handling),
            Avg_All_Other_Offences = mean(All_Other_Offences))

WORD

DAD <- WORD %>%
  select(Avg_Homicide, Avg_Sexual_Offence, Avg_Robbery, Avg_Theft_And_Handling, Avg_Drugs, Avg_Burglary, Month) %>%
  gather(key="crimetype", value = "Averagecrime", -Month)

ggplot(DAD, aes(fill=crimetype, y=Averagecrime, x=Month)) +
  geom_bar(position="dodge", stat="identity") +
  ggtitle("Average Crime vs Crimetype for 2014")


ggbetweenstats(data = DAD, x = crimetype, y = Averagecrime) +
  labs(title = "Distribution of Average crime type")


barplot(WORD$Average_Homicide, main = 'Average Homicide in 2014',
        xlab = 'Months', horiz = FALSE,col="blue")


boxplot(WORD[, 2:3], main ='Box Plots for Air Quality Parameters', col = "orange", border = "brown", horizontal = TRUE, notch = FALSE)


POC_Ndf %>%
  filter(Year==2014) %>%
  group_by(Month) %>%
  summarise(Avg_Homicide_Unsuc = mean(Homicide_Unsuccessful),
            Avg_Fraud_And_Forgery_Unsuc = mean(Fraud_And_Forgery_Unsuccessful),
            Avg_Offense_against_person_unsuc = mean(Offences_Against_The_Person_Unsuccessful),
            Avg_Criminal_Damage_Unsuc = mean(Criminal_Damage_Unsuccessful),
            Avg_Sexual_Offence_Unsuc = mean(Sexual_Offence_Unsuccessful),
            Avg_Drugs_Unsuc = mean(Drugs_Unsuccessful),
            Avg_Burglary_Unsuc = mean(Burglary_Unsuccessful),
            Avg_Public_Order_Unsuc = mean(Public_Order_Unsuccessful),
            Avg_All_Other_Offences_Unsuc = mean(All_Other_Offences_Unsuccessful),
            Avg_Robbery_Unsuc = mean(Robbery_Unsuccessful),
            Avg_Theft_And_Handling_Unsuc=mean(Theft_And_Handling_Unsuccessful),
            Avg_Admin_Finalised_Unsuc = mean(Admin_Finalised_Unsuccessful))


#################################################
# Average crime rate distributed monthly for 2015
#################################################
WORD2 <- POC_Ndf %>%
  filter(Year==2015) %>%
  group_by(Month) %>%
  summarise(Avg_Homicide = mean(Homicide),
            Avg_Criminal_Damage = mean(Criminal_Damage),
            Avg_Fraud_And_Forgery = mean(Fraud_And_Forgery),
            Avg_Offense_against_person = mean(Offences_Against_The_Person),
            Avg_Drugs = mean(Drugs),
            Avg_Sexual_Offence = mean(Sexual_Offences),
            Avg_Public_Order = mean(Public_Order),
            Avg_Burglary = mean(Burglary),
            Avg_Motoring_Offences = mean(Motoring_Offences),
            Avg_Robbery = mean(Robbery),
            Avg_Theft_And_Handling = mean(Theft_And_Handling),
            Avg_All_Other_Offences = mean(All_Other_Offences))
WORD2

DAD2 <- WORD2 %>%
  select(Avg_Homicide, Avg_Sexual_Offence, Avg_Robbery, Avg_Theft_And_Handling, Avg_Drugs, Avg_Burglary, Month) %>%
  gather(key="crimetype", value = "Averagecrime", -Month)

ggplot(DAD2, aes(fill=crimetype, y=Averagecrime, x=Month)) +
  geom_bar(position="dodge", stat="identity") +
  ggtitle("Average Crime vs Crimetype for 2015")

POC_Ndf %>%
  filter(Year==2015) %>%
  group_by(Month) %>%
  summarise(Avg_Homicide_Unsuc = mean(Homicide_Unsuccessful),
            Avg_Fraud_And_Forgery_Unsuc = mean(Fraud_And_Forgery_Unsuccessful),
            Avg_Offense_against_person_unsuc = mean(Offences_Against_The_Person_Unsuccessful),
            Avg_Criminal_Damage_Unsuc = mean(Criminal_Damage_Unsuccessful),
            Avg_Sexual_Offence_Unsuc = mean(Sexual_Offence_Unsuccessful),
            Avg_Drugs_Unsuc = mean(Drugs_Unsuccessful),
            Avg_Burglary_Unsuc = mean(Burglary_Unsuccessful),
            Avg_Public_Order_Unsuc = mean(Public_Order_Unsuccessful),
            Avg_All_Other_Offences_Unsuc = mean(All_Other_Offences_Unsuccessful),
            Avg_Robbery_Unsuc = mean(Robbery_Unsuccessful),
            Avg_Theft_And_Handling_Unsuc=mean(Theft_And_Handling_Unsuccessful),
            Avg_Admin_Finalised_Unsuc = mean(Admin_Finalised_Unsuccessful))

#################################################
# Average crime rate distributed monthly for 2016
#################################################
WORD3 <- POC_Ndf %>%
  filter(Year==2016) %>%
  group_by(Month) %>%
  summarise(Avg_Homicide = mean(Homicide),
            Avg_Criminal_Damage = mean(Criminal_Damage),
            Avg_Fraud_And_Forgery = mean(Fraud_And_Forgery),
            Avg_Offense_against_person = mean(Offences_Against_The_Person),
            Avg_Drugs = mean(Drugs),
            Avg_Sexual_Offence = mean(Sexual_Offences),
            Avg_Public_Order = mean(Public_Order),
            Avg_Burglary = mean(Burglary),
            Avg_Motoring_Offences = mean(Motoring_Offences),
            Avg_Robbery = mean(Robbery),
            Avg_Theft_And_Handling = mean(Theft_And_Handling),
            Avg_All_Other_Offences = mean(All_Other_Offences))
WORD3

DAD3 <- WORD3 %>%
  select(Avg_Homicide, Avg_Sexual_Offence, Avg_Robbery,
         Avg_Theft_And_Handling, Avg_Drugs, Avg_Burglary, Month) %>%
  gather(key="crimetype", value = "Averagecrime", -Month)

ggplot(DAD3, aes(fill=crimetype, y=Averagecrime, x=Month)) +
  geom_bar(position="dodge", stat="identity") +
  ggtitle("Average Crime vs Crimetype for 2016")


POC_Ndf %>%
  filter(Year==2016) %>%
  group_by(Month) %>%
  summarise(Avg_Homicide_Unsuc = mean(Homicide_Unsuccessful),
            Avg_Fraud_And_Forgery_Unsuc = mean(Fraud_And_Forgery_Unsuccessful),
            Avg_Offense_against_person_unsuc = mean(Offences_Against_The_Person_Unsuccessful),
            Avg_Criminal_Damage_Unsuc = mean(Criminal_Damage_Unsuccessful),
            Avg_Sexual_Offence_Unsuc = mean(Sexual_Offence_Unsuccessful),
            Avg_Drugs_Unsuc = mean(Drugs_Unsuccessful),
            Avg_Burglary_Unsuc = mean(Burglary_Unsuccessful),
            Avg_Public_Order_Unsuc = mean(Public_Order_Unsuccessful),
            Avg_All_Other_Offences_Unsuc = mean(All_Other_Offences_Unsuccessful),
            Avg_Robbery_Unsuc = mean(Robbery_Unsuccessful),
            Avg_Theft_And_Handling_Unsuc=mean(Theft_And_Handling_Unsuccessful),
            Avg_Admin_Finalised_Unsuc = mean(Admin_Finalised_Unsuccessful))



#################################################
# Average crime rate distributed monthly for 2017
#################################################
WORD4 <- POC_Ndf %>%
  filter(Year==2017) %>%
  group_by(Month) %>%
  summarise(Avg_Homicide = mean(Homicide),
            Avg_Criminal_Damage = mean(Criminal_Damage),
            Avg_Fraud_And_Forgery = mean(Fraud_And_Forgery),
            Avg_Offense_against_person = mean(Offences_Against_The_Person),
            Avg_Drugs = mean(Drugs),
            Avg_Sexual_Offence = mean(Sexual_Offences),
            Avg_Public_Order = mean(Public_Order),
            Avg_Burglary = mean(Burglary),
            Avg_Motoring_Offences = mean(Motoring_Offences),
            Avg_Robbery = mean(Robbery),
            Avg_Theft_And_Handling = mean(Theft_And_Handling),
            Avg_All_Other_Offences = mean(All_Other_Offences))
WORD4

DAD4 <- WORD4 %>%
  select(Avg_Homicide, Avg_Sexual_Offence, Avg_Robbery,
         Avg_Theft_And_Handling, Avg_Drugs, Avg_Burglary, Month) %>%
  gather(key="crimetype", value = "Averagecrime", -Month)

ggplot(DAD4, aes(fill=crimetype, y=Averagecrime, x=Month)) +
  geom_bar(position="dodge", stat="identity") +
  ggtitle("Average Crime vs Crimetype for 2017")


POC_Ndf %>%
  filter(Year==2017) %>%
  group_by(Month) %>%
  summarise(Avg_Homicide_Unsuc = mean(Homicide_Unsuccessful),
            Avg_Fraud_And_Forgery_Unsuc = mean(Fraud_And_Forgery_Unsuccessful),
            Avg_Offense_against_person_unsuc = mean(Offences_Against_The_Person_Unsuccessful),
            Avg_Criminal_Damage_Unsuc = mean(Criminal_Damage_Unsuccessful),
            Avg_Sexual_Offence_Unsuc = mean(Sexual_Offence_Unsuccessful),
            Avg_Drugs_Unsuc = mean(Drugs_Unsuccessful),
            Avg_Burglary_Unsuc = mean(Burglary_Unsuccessful),
            Avg_Public_Order_Unsuc = mean(Public_Order_Unsuccessful),
            Avg_All_Other_Offences_Unsuc = mean(All_Other_Offences_Unsuccessful),
            Avg_Robbery_Unsuc = mean(Robbery_Unsuccessful),
            Avg_Theft_And_Handling_Unsuc=mean(Theft_And_Handling_Unsuccessful),
            Avg_Admin_Finalised_Unsuc = mean(Admin_Finalised_Unsuccessful))





############################################################
# 2.  Average Crime distibuted across various county per year
POC_Ndf %>%
  filter(Year==2014) %>%
  group_by(Month) %>%
  summarise(average_crime_2014 = mean(Homicide), percent_homocide_average = mean(Drugs)) %>%
  arrange(desc(average_crime_2014)) %>%
  ggplot(aes(x = reorder(Month,+average_crime_2014), y=average_crime_2014,fill=Month)) + geom_col() +
  ggtitle("Night price distribution of Airbnb appartements") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Night price") +
  ylab("Number of apartments") +
  coord_flip()


# Distribution of the various crime by month
POC_df %>%
  group_by(Year) %>%
  group_by(Month) %>%
  summarise(A = mean(Homicide), B=mean(Homicide_Unsuccessful),  C = mean(Offences_Against_The_Person), D=mean(Offences_Against_The_Person_Unsuccessful), E=mean(Sexual_Offences), F=mean(Sexual_Offence_Unsuccessful), G=mean(Burglary), H=mean(Burglary_Unsuccessful), I=mean(Robbery), J=mean(Robbery_Unsuccessful), K=mean(Theft_And_Handling),
            L=mean(Theft_And_Handling_Unsuccessful), M=mean(Fraud_And_Forgery), N=mean(Fraud_And_Forgery_Unsuccessful), O=mean(Criminal_Damage), P=mean(Criminal_Damage_Unsuccessful), Q=mean(Drugs), R=mean(Drugs_Unsuccessful),S=mean(Public_Order),T=mean(Public_Order_Unsuccessful),
            U=mean(All_Other_Offences),V=mean(All_Other_Offences_Unsuccessful), W=mean(Motoring_Offences), X=mean(Motoring_Offences_Unsuccessful), Y=mean(Admin_Finalised_Unsuccessful)) %>%
  ggplot(aes(x = reorder(Month,+A), y=A,fill=Month)) + geom_col() +
  ggtitle("Name Main") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Name X axis") +
  ylab("Name Y axis") +
  coord_flip()


ggplot(POC_df, aes(x=Homicide, y=Drugs)) +
  geom_point(size=6, color="#69b3a2") +
  theme_ipsum()

#################################################################
#   Most frequent crime in 2014 using National which gives the
#   total number of crime in all the county
##################################################################
national_df_14 <- POC_df %>%
  filter(County=="National", Year == "2014")

head(national_df_14)

num_df <- national_df_14[,-c(1:4)]
value_df<-apply(num_df,2,sum)
value_df<-as.data.frame(value_df)
value_df$value_df
col_nam <- as.data.frame(colnames(num_df))



offense_2014_df <- cbind(col_nam, value_df$value_df)
names(offense_2014_df)[1:2] <- c("Offense","Total")



offense_2014_df[-26,] %>%
  ggplot(aes(x =reorder(Offense, + Total), y =  Total, fill=Offense)) +
  theme_classic()+
  geom_col() +
  theme(legend.position = "none") +
  ggtitle("Most Reoccuring Crime in 2014") +
  coord_flip()


#################################################################
#   Most frequent crime in 2015 using National which gives the
#   total number of crime in all the county
##################################################################

national_df_15 <- POC_df %>%
  filter(County=="National", Year == "2015")

num_df <- national_df_15[,-c(1:4)]
value_df<-apply(num_df,2,sum)
value_df<-as.data.frame(value_df)
value_df$value_df
col_nam <- as.data.frame(colnames(num_df))

offense_2015_df <- cbind(col_nam, value_df$value_df)
names(offense_2015_df)[1:2] <- c("Offense","Total")

offense_2015_df[-26,] %>%
  ggplot(aes(x =reorder(Offense, + Total), y =  Total, fill=Offense)) +
  theme_classic()+
  geom_col() +
  theme(legend.position = "none") +
  ggtitle("Most Reoccuring Crime in 2015") +
  coord_flip()


#################################################################
#   Most frequent crime in 2016 using National which gives the
#   total number of crime in all the county
##################################################################
national_df_16 <- POC_df %>%
  filter(County=="National", Year == "2016")

num_df <- national_df_16[,-c(1:4)]
value_df<-apply(num_df,2,sum)
value_df<-as.data.frame(value_df)
value_df$value_df
col_nam <- as.data.frame(colnames(num_df))

offense_2016_df <- cbind(col_nam, value_df$value_df)
names(offense_2016_df)[1:2] <- c("Offense","Total")

offense_2016_df[-26,] %>%
  ggplot(aes(x =reorder(Offense, + Total), y =  Total, fill=Offense)) +
  theme_classic()+
  geom_col() +
  theme(legend.position = "none") +
  ggtitle("Most Reoccuring Crime in 2016") +
  coord_flip()


#################################################################
#   Most frequent crime in 2017 using National which gives the
#   total number of crime in all the county
##################################################################
national_df_17 <- POC_df %>%
  filter(County=="National", Year == "2017")

num_df <- national_df_17[,-c(1:4)]
value_df<-apply(num_df,2,sum)
value_df<-as.data.frame(value_df)
value_df$value_df
col_nam <- as.data.frame(colnames(num_df))

offense_2017_df <- cbind(col_nam, value_df$value_df)
names(offense_2017_df)[1:2] <- c("Offense","Total")

offense_2017_df[-26,] %>%
  ggplot(aes(x =reorder(Offense, + Total), y =  Total, fill=Offense)) +
  theme_classic()+
  geom_col() +
  theme(legend.position = "none") +
  ggtitle("Most Reoccuring Crime in 2017") +
  coord_flip()


##############################################
# Average occurance of various crime in years
##############################################
Year_2014 <- POC_Ndf %>%
  filter(Year==2014) %>%
  summarise(Homicide = mean(Homicide),
            Criminal_Damage = mean(Criminal_Damage),
            Fraud_And_Forgery = mean(Fraud_And_Forgery),
            Offense_against_person = mean(Offences_Against_The_Person),
            Drugs = mean(Drugs),
            Sexual_Offence = mean(Sexual_Offences),
            Public_Order = mean(Public_Order),
            Burglary = mean(Burglary),
            Motoring_Offences = mean(Motoring_Offences),
            Robbery = mean(Robbery),
            Theft_And_Handling = mean(Theft_And_Handling),
            All_Other_Offences = mean(All_Other_Offences),
            year = median(Year))

Year_2015 <- POC_Ndf %>%
  filter(Year==2015) %>%
  summarise(Homicide = mean(Homicide),
            Criminal_Damage = mean(Criminal_Damage),
            Fraud_And_Forgery = mean(Fraud_And_Forgery),
            Offense_against_person = mean(Offences_Against_The_Person),
            Drugs = mean(Drugs),
            Sexual_Offence = mean(Sexual_Offences),
            Public_Order = mean(Public_Order),
            Burglary = mean(Burglary),
            Motoring_Offences = mean(Motoring_Offences),
            Robbery = mean(Robbery),
            Theft_And_Handling = mean(Theft_And_Handling),
            All_Other_Offences = mean(All_Other_Offences),
            year = median(Year))


Year_2016 <- POC_Ndf %>%
  filter(Year==2016) %>%
  summarise(Homicide = mean(Homicide),
            Criminal_Damage = mean(Criminal_Damage),
            Fraud_And_Forgery = mean(Fraud_And_Forgery),
            Offense_against_person = mean(Offences_Against_The_Person),
            Drugs = mean(Drugs),
            Sexual_Offence = mean(Sexual_Offences),
            Public_Order = mean(Public_Order),
            Burglary = mean(Burglary),
            Motoring_Offences = mean(Motoring_Offences),
            Robbery = mean(Robbery),
            Theft_And_Handling = mean(Theft_And_Handling),
            All_Other_Offences = mean(All_Other_Offences),
            year = median(Year))

Year_2017 <- POC_Ndf %>%
  filter(Year==2017) %>%
  summarise(Homicide = mean(Homicide),
            Criminal_Damage = mean(Criminal_Damage),
            Fraud_And_Forgery = mean(Fraud_And_Forgery),
            Offense_against_person = mean(Offences_Against_The_Person),
            Drugs = mean(Drugs),
            Sexual_Offence = mean(Sexual_Offences),
            Public_Order = mean(Public_Order),
            Burglary = mean(Burglary),
            Motoring_Offences = mean(Motoring_Offences),
            Robbery = mean(Robbery),
            Theft_And_Handling = mean(Theft_And_Handling),
            All_Other_Offences = mean(All_Other_Offences),
            year = median(Year))

Avg_crime <- rbind(Year_2014, Year_2015, Year_2016, Year_2017)

############################
# Applying gather and spread
############################
Average_crime <- Avg_crime %>%
  select(Homicide,
         Criminal_Damage,
         Fraud_And_Forgery,
         Offense_against_person,
         Drugs,
         Sexual_Offence,
         Public_Order,
         Burglary,
         Motoring_Offences,
         Robbery,
         Theft_And_Handling,
         year) %>%
  gather(key="crime_type", value = "values", -year)

ggplot(Average_crime, aes(fill=crime_type, y=values, x=year)) +
  geom_bar(position="dodge", stat="identity") +
  ggtitle("Average crime type distribution in years")

########################################################
# Total Unsuccessful Convictions for 2014 - 2017
unsucc <- POC_Ndf %>% select(Year, contains("Unsuccessful"))

unsucc1 <- POC_Ndf %>% select(contains("Unsuccessful"))
total_unsucc <- unsucc1 %>% summarise(total_unsuccessful = apply(unsucc1, 1, sum))

Total_unsucc_df <- cbind(unsucc[1], total_unsucc)

# 2014
T_U_14 <- Total_unsucc_df %>% 
  filter(Year == "2014") %>%
  summarise(unsucc_14 = mean(total_unsuccessful))

# 2015
T_U_15 <- Total_unsucc_df %>% 
  filter(Year == "2015") %>%
  summarise(unsucc_15 = mean(total_unsuccessful))

# 2016
T_U_16 <- Total_unsucc_df %>% 
  filter(Year == "2016") %>%
  summarise(unsucc_16 = mean(total_unsuccessful))

# 2017
T_U_17 <- Total_unsucc_df %>% 
  filter(Year == "2017") %>%
  summarise(unsucc_17 = mean(total_unsuccessful))

T_U <- cbind(T_U_14, T_U_15, T_U_16, T_U_17)

T_U_df <- T_U %>%
  select(unsucc_14,
         unsucc_15,
         unsucc_16,
         unsucc_17) %>%
  gather(key="Average_Unsuccessful_Conviction", value = "values")

ggplot(T_U_df, aes(fill=Average_Unsuccessful_Conviction, y=values, x=Average_Unsuccessful_Conviction)) +
  geom_bar(position="dodge", stat="identity") +
  ggtitle("Average Unsuccessful Convitions for (2014 - 2017")  + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none")
#######################################################

#################################################################################################
# Exploring by Regions
North_County1 <- c("Cleveland", "Cumbria", "Durham", "GreaterManchester", "Lancashire",
                   "Merseyside", "Metropolitan and City", "Northumbria", "North Wales",
                   "North Yorshire")
North_df1 <- POC_Ndf %>%
  filter(County == North_County1) %>%
  mutate(Region = "North")

South_County1 <- c("Avon and Somerset", "Derbyshire", "Devon and Cornwall", "Dorset",
                   "Dyfed Powys", "Gloucestershire",
                   "Gwent", "Hampshire", "Kent", "South Wales", "South Yorkshire",
                   "Surrey", "Sussex", "Thames Valley", "Wiltshire")
South_df1 <- POC_Ndf %>%
  filter(County == South_County1) %>%
  mutate(Region = "South")

East_County1 <- c("Cambridgeshire", "Cheshire", "Essex", "Hertfordshire", "Humberside",
                  "Leicestershire", "Lincolnshire", "Norfolk",
                  "Northamptonshire", "Nottinghamshire", "Bedfordshire")

East_df1 <- POC_Ndf %>%
  filter(County == East_County1) %>%
  mutate(Region = "East")

West_County1 <- c("Staffordshire", "Warwickshire", "West Mercia", "West Midlands",
                  "West Yorkshire", "Suffolk")

West_df1 <- POC_Ndf %>%
  filter(County == West_County1) %>%
  mutate(Region = "West")

# Binding the regions using row bind
Regions <- rbind(North_df1, South_df1, East_df1, West_df1)
colnames(Regions)
############################################################################
# Average occurance of various crime by Region
###########################################################################

Region_2014 <- Regions %>%
  group_by(Region) %>%
  filter(Year==2014) %>%
  summarise(Homicide = mean(Homicide),
            Criminal_Damage = mean(Criminal_Damage),
            Fraud_And_Forgery = mean(Fraud_And_Forgery),
            Offense_against_person = mean(Offences_Against_The_Person),
            Drugs = mean(Drugs),
            Sexual_Offence = mean(Sexual_Offences),
            Public_Order = mean(Public_Order),
            Burglary = mean(Burglary),
            Motoring_Offences = mean(Motoring_Offences),
            Robbery = mean(Robbery),
            Theft_And_Handling = mean(Theft_And_Handling)) %>% 
  gather(key="crime_type", value = "values", -Region) 
ggplot(Region_2014, aes(fill=crime_type, y=values, x=Region)) +
  geom_bar(position="dodge", stat="identity") +
  ggtitle("Average crime type distributed by Region for 201")

Region_2015 <- Regions %>%
  group_by(Region) %>%
  filter(Year==2015) %>%
  summarise(Homicide = mean(Homicide),
            Criminal_Damage = mean(Criminal_Damage),
            Fraud_And_Forgery = mean(Fraud_And_Forgery),
            Offense_against_person = mean(Offences_Against_The_Person),
            Drugs = mean(Drugs),
            Sexual_Offence = mean(Sexual_Offences),
            Public_Order = mean(Public_Order),
            Burglary = mean(Burglary),
            Motoring_Offences = mean(Motoring_Offences),
            Robbery = mean(Robbery),
            Theft_And_Handling = mean(Theft_And_Handling)) %>% 
  gather(key="crime_type", value = "values", -Region) 
ggplot(Region_2015, aes(fill=crime_type, y=values, x=Region)) +
  geom_bar(position="dodge", stat="identity") +
  ggtitle("Average crime type distributed by Region for 2015")


Region_2016 <- Regions %>%
  group_by(Region) %>%
  filter(Year==2016                                                                                                       ) %>%
  summarise(Homicide = mean(Homicide),
            Criminal_Damage = mean(Criminal_Damage),
            Fraud_And_Forgery = mean(Fraud_And_Forgery),
            Offense_against_person = mean(Offences_Against_The_Person),
            Drugs = mean(Drugs),
            Sexual_Offence = mean(Sexual_Offences),
            Public_Order = mean(Public_Order),
            Burglary = mean(Burglary),
            Motoring_Offences = mean(Motoring_Offences),
            Robbery = mean(Robbery),
            Theft_And_Handling = mean(Theft_And_Handling)) %>% 
  gather(key="crime_type", value = "values", -Region) 
ggplot(Region_2016, aes(fill=crime_type, y=values, x=Region)) +
  geom_bar(position="dodge", stat="identity") +
  ggtitle("Average crime type distributed by Region for 2016")

Region_2017 <- Regions %>%
  group_by(Region) %>%
  filter(Year==2017) %>%
  summarise(Homicide = mean(Homicide),
            Criminal_Damage = mean(Criminal_Damage),
            Fraud_And_Forgery = mean(Fraud_And_Forgery),
            Offense_against_person = mean(Offences_Against_The_Person),
            Drugs = mean(Drugs),
            Sexual_Offence = mean(Sexual_Offences),
            Public_Order = mean(Public_Order),
            Burglary = mean(Burglary),
            Motoring_Offences = mean(Motoring_Offences),
            Robbery = mean(Robbery),
            Theft_And_Handling = mean(Theft_And_Handling)) %>% 
  gather(key="crime_type", value = "values", -Region) 
ggplot(Region_2017, aes(fill=crime_type, y=values, x=Region)) +
  geom_bar(position="dodge", stat="identity") 
######################################################################


######################################################################
# 2.  Regression Analysis
######################################################################
colnames(POC_Ndf)
Reg_df <- POC_Ndf[,c("Total_Number_of_Crime","Homicide","Offences_Against_The_Person","Sexual_Offences",
                     "Burglary","Robbery","Theft_And_Handling","Fraud_And_Forgery","Criminal_Damage",
                     "Drugs","Public_Order","All_Other_Offences")]
head(Reg_df)

plot(Reg_df) #scatter plot to see if there is a relationship in  the data under consideration

correlation = cor(Reg_df) # get correlations
round(correlation, 4)

corrplot(correlation, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45) #plot matrix or the heatmap

str(Reg_df)

#splitting the data into training and testing data set.seed()
set.seed(2)
install.packages("caTools")
library(caTools)
split<- sample.split(Reg_df, SplitRatio = 0.75)
split

train <- subset(Reg_df, split="TRUE")
test <- subset(Reg_df, split="FALSE")
head(train)

class(train)
#creating the model
Model <- lm(Total_Number_of_Crime ~.,data = train)
summary(Model)
anova(Model) # Analysis of Variance

plot(Model)

#prediction
pred <- predict((Model), test)
head(pred)



#comparing predicted vs actual values
plot(test$Total_Number_of_Crime,type = "l", lty = 1.8, col = "black")
lines(pred,type = "l", col = "red")

#Finding Accuracy of the model
rmse <- sqrt(mean(pred-Reg_df$Total_Number_of_Crime)^2)
rmse

#To identify which of the crime contributes more to the Total crime rate, i will rank the signqificant values that has been gotetn from the regression analysis

## Apply normalization
normalization <-function(x) { (x -min(x))/(max(x)-min(x)) }




######################################################################
# 2.  Clustering Analysis
######################################################################
colnames(POC_Ndf)
POC_ClUS_df <- POC_Ndf %>%    #filtering the data to use only data from year 2014 for the cluster analysis
  filter(Year==2014) %>%
  select(-c(Date, Month, Year, Homicide_Unsuccessful, Offences_Against_The_Person_Unsuccessful,
            Sexual_Offence_Unsuccessful, Burglary_Unsuccessful, Robbery_Unsuccessful,
            Theft_And_Handling_Unsuccessful, Fraud_And_Forgery_Unsuccessful, Criminal_Damage_Unsuccessful,
            Drugs_Unsuccessful, Public_Order_Unsuccessful, All_Other_Offences_Unsuccessful,
            Admin_Finalised_Unsuccessful, Motoring_Offences_Unsuccessful, Total_Number_of_Crime))

head(POC_ClUS_df)
dim(POC_ClUS_df)

colnames(POC_ClUS_df)


# Grouping our county into North, South, East and West

North_County <- c("Cleveland", "Cumbria", "Durham", "GreaterManchester", "Lancashire",
                  "Merseyside", "Metropolitan and City", "Northumbria", "North Wales",
                  "North Yorshire")
North_df <- POC_ClUS_df %>%
  filter(County == North_County) %>%
  mutate(Region = "North")

South_County <- c("Avon and Somerset", "Derbyshire", "Devon and Cornwall", "Dorset",
                  "Dyfed Powys", "Gloucestershire",
                  "Gwent", "Hampshire", "Kent", "South Wales", "South Yorkshire",
                  "Surrey", "Sussex", "Thames Valley", "Wiltshire")
South_df <- POC_ClUS_df %>%
  filter(County == South_County) %>%
  mutate(Region = "South")

East_County <- c("Cambridgeshire", "Cheshire", "Essex", "Hertfordshire", "Humberside",
                 "Leicestershire", "Lincolnshire", "Norfolk",
                 "Northamptonshire", "Nottinghamshire", "Bedfordshire")

East_df <- POC_ClUS_df %>%
  filter(County == East_County) %>%
  mutate(Region = "East")

West_County <- c("Staffordshire", "Warwickshire", "West Mercia", "West Midlands",
                 "West Yorkshire", "Suffolk")

West_df <- POC_ClUS_df %>%
  filter(County == West_County) %>%
  mutate(Region = "West")

# Binding the regions using row bind
Cluster_Region <- rbind(North_df, South_df, East_df, West_df)
colnames(Cluster_Region)

# Distribution of Drugs crime related by regions
ggbetweenstats(data = Cluster_Region, x = Region, y = Drugs) +
  labs(title = "Distribution of crime type by Region")

POC_Region_df <- Cluster_Region %>%
  select(Region, 2:13, -1)

dim(POC_Region_df)
colnames(POC_Region_df)

# Taking out the categorical variables in the data frame
POC_Region <- POC_Region_df %>%
  select(-Region)

head(POC_Region)
dim(POC_Region)

plot(POC_Region_df$Homicide ~ POC_Region_df$Robbery, data=POC_Region_df)
with(POC_Region_df, text(POC_Region_df$Homicide ~ POC_Region_df$Robbery,
                         labels=POC_Region_df$Region, pos=1))

#There is no need to scale or standardized the data for we are using for the clustering since its is assume there are all coming from the same population and was also measured using the same measurement scale
#Also the reason for wanting  to standardized the data is to avoid one particular variable  
z <- POC_Region
x_bar <- apply(z,2,mean)
std <- apply(z,2,sd)
POC_Region_norm <- scale(z,center = x_bar, scale = std)


head(round(POC_Region_norm, 2))
tail(round(POC_Region_norm, 2))
POC_Region_norm %>% summary()


#######################################
# Hierarchical agglomerative clustering
######################################
#calculating the distance matrix
distance = dist(POC_Region_norm)
round(distance, 2)
plot(distance)
fit.hclust <- hclust(distance, "ward.D2")

fit.hclust

require(grDevices)
require(scales)
fviz_dend(x = fit.hclust, cex =0.8, lwd = 0.8, k=4, k_colors = c("red", "green", "blue", "magenta"))
          
summary(fit.hclust)
plot(fit.hclust, labels = )  #plotting our Hierarchical structure

# Determining how many cluster to keep
rect.hclust(fit.hclust, k=6, border ="red")
cluster <- cutree(fit.hclust, 6)
cluster


# Evaluating the suitability of the cluster model
plot(POC_Region[,1:4], col = cluster)


#############################################################
# K-Means Clustering
############################################################
#Setting seed so we can achieved reproducibility of the result
dim(POC_Region)
plot(POC_Region[,1:4], col=cluster)


set.seed(100)
km.fit <- kmeans(POC_Region_norm,centers = 4, nstart = 100)
km.fit

plot(POC_Region[,1:4], col = km.fit$cluster)


#Choosing K, we create a for loop to iterate over the range of value for k to get the optimal number of k
k <- list()
for(i in 1:10){
  k[[i]] <- kmeans(POC_Region_norm, i)
}

k

head(k[[i]])

BSS_TSS <- list()
for(i in 1:10){
  BSS_TSS[[i]] <- k[[i]]$betweenss/k[[i]]$totss
}

BSS_TSS
# Optimal number of clusters
plot(1:10, BSS_TSS, type = "b",
     ylab = "Between Sums of Squares / Total SS", xlab = "Number of cluster(k)")


for(i in 1:4){
  plot(POC_Region[1:4], col = k[[i]]$cluster)
}

library(cluster)
sil <- silhouette(km.fit$cluster, dist(POC_Region_norm))
fviz_silhouette(sil)
#########################################################
# At K = 2 we get a good cluster grouping for our data set
#########################################################

# Visualization
km.clusters <- km.fit$cluster
km.clusters

fviz_cluster(list(data=POC_Region, cluster = km.clusters))
table(km.clusters, POC_Region_df$Region )


# D~~~#####
km.clusters
head(POC_Region_df)
derr = POC_Region_df
derr$Cluser <- km.clusters

derr %>%
  filter(Cluser == 4)

########################################################################################################################
# MODEL BASED CLUSTERING
# There are some other sophisticated clustering technique available like Model based and Density based clustering
# What Model basses does is to fit statistical model called mixture model to our data to look for different distribution
# And based on the distribution it identify whether our data comes from any of this identified distribution
########################################################################################################################
fit_M <- Mclust(POC_Region_norm)
fit_M
fit_M$classification
fit_M$modelName
fit_M$BIC

# Plotting the Model Based Clustering plot
plot(fit_M)


#######################################
# Density Based Clustering
#######################################

install.packages("dbscan")
library(dbscan)
kNNdistplot(POC_Region_norm, k=4)
abline(h=1.5, col = "red", lty=2)
fitD <- dbscan(POC_Region_norm, eps = 1.5, minPts = 14)
fitD

plot(POC_Region[,c(1:4)], col = fitD$cluster)

########################################


##################
# CLASSIFICATION
##################

#####################################################
# KNN (K-Nearest Neighbor) Classification Analysis
# randomizing my data frame
#set.seed(1000)
rd <- runif(nrow(POC_Region_df))
POC_Region_df1 <- POC_Region_df[order(rd), ]

# Rescaling the randomized data
POC_Region_df1_n <- as.data.frame(lapply(POC_Region_df1[,c(2:11)], normalization))
summary(POC_Region_df1_n)

train_set <- POC_Region_df1_n[1:60,]
test_set <- POC_Region_df1_n[61:77,]
train_target <- POC_Region_df1[1:60, 1]
test_target <- POC_Region_df1[61:77, 1]
tr_data <- cbind(train_target, train_set)
te_data <- cbind(test_target, test_set)
names(tr_data)[1] <- "Region"

tr_data$Region <- as.factor(tr_data$Region)
names(te_data)[1] <- "Region"


# creating the knn model
k <- sqrt(nrow(POC_Region_df1)) # using rule of thumb to choose value of K
k
knn_model <- knn(train = train_set, test = test_set, cl = train_target, k = 9)
dim(train_set)
dim(test_set)
class(train_target)

knn_model
summary(knn_model)
confusion  <- table(knn_model, test_target)
confusion

plot(knn_model)

# Accuracy of the model
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}

accuracy(confusion)
library(caret)
confusionMatrix(knn_model, test_target)

# using for loop to get the best value of k that will give the highest accuracy for the model
#EXCERCISE 1 changing the value of K

for(i in 1:50){
  knn_model1 <- knn(train = train_set, test = test_set, cl = train_target, k = i)
  prediction1 <- table(knn_model1, test_target)
  accuracy1 <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
  accuracy1(prediction1)
  print(paste(i, accuracy1(prediction1)))
}

plot_predictions <- data.frame(
  te_data$Homicide,
  te_data$Offences_Against_The_Person,
    te_data$Sexual_Offences,
    te_data$Burglary,
    te_data$Robbery,
    te_data$Theft_And_Handling,
    te_data$Fraud_And_Forgery,
    te_data$Criminal_Damage,
      te_data$Drugs,
      te_data$Public_Order,
  predict_class = knn_model)

# Renaming the column names
colnames(plot_predictions) <- c("Homicide",
                                "Offences_Against_The_Person",
                                "Sexual_Offences",
                               "Burglary",
                               "Robbery",
                               "Theft_And_Handling",
                               "Fraud_And_Forgery",
                               "Criminal_Damage",
                               "Drugs",
                               "Public_Order",
                               "Predicted")

p1 <- ggplot(plot_predictions, aes(Homicide, Offences_Against_The_Person, 
                             color = Predicted, fill = Predicted)) +
  geom_point(size = 4) +
  geom_text(aes(label = test_target), hjust = 1, vjust = 2) + 
  ggtitle("Predicted relationship btw Offence Against Person and Homicide") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.position = "none")


p2 <- ggplot(plot_predictions, aes(Sexual_Offences, Burglary, 
                                   color = Predicted, fill = Predicted)) +
  geom_point(size = 4) +
  geom_text(aes(label = test_target), hjust = 1, vjust = 2) + 
  ggtitle("Predicted relationship btw Sexual_Offences and Burglary") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.position = "none")


p3 <- ggplot(plot_predictions, aes(Robbery, Theft_And_Handling, 
                                   color = Predicted, fill = Predicted)) +
  geom_point(size = 4) +
  geom_text(aes(label = test_target), hjust = 1, vjust = 2) + 
  ggtitle("Predicted relationship btw Robbery and Theft_And_Handling") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.position = "none")


p4 <- ggplot(plot_predictions, aes(Fraud_And_Forgery, Criminal_Damage, 
                                   color = Predicted, fill = Predicted)) +
  geom_point(size = 4) +
  geom_text(aes(label = test_target), hjust = 1, vjust = 2) + 
  ggtitle("Predicted relationship btw Fraud_And_Forgery and Criminal_Damage") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.position = "none")


p5 <- ggplot(plot_predictions, aes(Drugs, Public_Order, 
                                   color = Predicted, fill = Predicted)) +
  geom_point(size =4 ) +
  geom_text(aes(label = test_target), hjust = 1, vjust = 2) + 
  ggtitle("Predicted relationship btw Drugs and Public_Order") + 
  theme(plot.title = element_text(hjust = 0.5)) 

library(gridExtra)
grid.arrange(p1, p2, p3, p4, p5, ncol=3)

grid.arrange(p1, p2, ncol =2)
grid.arrange(p3, p4, p5, ncol =2)

#####################################################


#########################################
# Random Forests Classifier
#########################################
set.seed(100)
RFM <- randomForest(Region ~ ., data = tr_data)
RFM
# Evaluating the model Accuracy
pred <- predict(RFM, te_data)
pred

# Confusion matrix
RF_cm <- table(te_data$Region, pred)
RF_cm

classification_Accuracy <- sum(diag(RF_cm)/sum(RF_cm))
classification_Accuracy

##########################################
###################################
# SPLIT AND TEST VARIABLE
###################################
# To achieve reproducible model: set the random seed number
normalization <-function(x) { (x -min(x))/(max(x)-min(x)) }
Nor_df <- as.data.frame(lapply(POC_Region_df[,-1], normalization))

# Performing stratified random split of the data to create index based on Region  
training_index <- createDataPartition(POC_Region_df$Region, p=0.8, list=FALSE)
target_variable <- POC_Region_df[training_index, 1]

# Subset training set with index
training_data <- POC_Region_df[training_index,]
str(training_data)

# Subset test set with index
testing_data <- POC_Region_df[-training_index,]

###########################################################
# Decision Tree Classification Model
###########################################################
set.seed(100)

#  converting Region from character to factor
POC_Region_df$Region <- as.factor(POC_Region_df$Region)
training_data$Region <- as.factor(training_data$Region)
testing_data$Region <- as.factor(testing_data$Region)


model <- Region ~ Homicide + Offences_Against_The_Person +
  Sexual_Offences + Burglary + Theft_And_Handling + Fraud_And_Forgery +
  Criminal_Damage + Drugs + Public_Order + All_Other_Offences + Motoring_Offences


distance = dist(POC_Region_norm)
round(distance, 2)
plot(distance)


tree(model, data = training_data)
tree <- rpart(model, data = training_data, method = "class")
tree
summary(tree)
round(predict(tree),2)
rpart.plot(tree)

pred <- predict(tree, testing_data, type = "class")

#Classifications error for train data
tab <- table(pred, testing_data$Region)
tab
tree_Accuracy <- sum(diag(tab)/sum(tab))
tree_Accuracy
##################################################################
# SUPPORT VECTOR MACHINE (SVM) MODEL Using the (Polynomial Kernel)
##################################################################
qplot(Sexual_Offences, Public_Order, data = POC_Region_df1))

# Building the Model; this model is based on the training data set
SW_model <- train(Region ~ ., data = training_data,
                  method = "svmPoly",
                  na.action = na.omit,
                  preProcess = c("scale", "center"),
                  trControl = trainControl(method = "none"),
                  tuneGrid = data.frame(degree=1,scale=1,C=1))

# Building Cross Validation Model
CV_model <- train(Region ~ ., data = training_data,
                  method = "svmPoly",
                  na.action = na.omit,
                  preProcess = c("scale", "center"),
                  trControl = trainControl(method = "cv", number = 42),
                  tuneGrid = data.frame(degree=1,scale=1,C=1))

# Apply Model for Prediction
Model_training <- predict(SW_model, training_data)
Model_testing <- predict(SW_model, testing_data)

# Performing cross validation
CV_model <- predict(CV_model, training_data)

# Model Performance and Confusion matrix and statistics
Model_training_confusion <- confusionMatrix(Model_training, training_data$Region)

Model_testing_confusion <- confusionMatrix(Model_testing, testing_data$Region)

CV_model_confusion <- confusionMatrix(CV_model, training_data$Region)

print(Model_training_confusion)
print(Model_testing_confusion)
print(CV_model_confusion)

# Feature Importance
importance <- varImp(SW_model)
plot(importance, col="red")


###############################################################################################
#   One Way Anova

# Null hypothesis Ho: there is no difference in the mean of the various crime type
# Alternative hypothesis Ho: there is difference in the mean of the various crime type

# One way Analysis of Variance to investigate if there is a statistical significant
#difference btw the various crime type in year 2014 for only the successfully convicted crime
###############################################################################################
one_way_Anova_df <- POC_Ndf %>%
  group_by(Year) %>%
  filter(Year == 2014) %>%
  select(Homicide, Offences_Against_The_Person, Sexual_Offences, Burglary, Robbery,
         Theft_And_Handling, Fraud_And_Forgery, Criminal_Damage, Drugs, Public_Order, Motoring_Offences, Year) %>%
  gather(key = "crime_type", value = "crime_value", -Year)


# Boxplot for the crime mean
ggplot(one_way_Anova_df, aes(x=crime_type, y = crime_value, fill= crime_type)) +
  geom_boxplot() +
  coord_flip()


# Distribution of the mean of the various crime type
one_way_Anova_df %>%
  group_by(crime_type) %>%
  summarise(Mean_Crime = mean(crime_value)) %>%
  arrange(Mean_Crime)


# Is the Mean of the various crime really different

# Difference in mean of the various crime type is observed in the sample data
#  but is this statistically significant at (alpha = 0.05)

# Creating the ANOVA Model

one_way_AOV_model <- aov(crime_value ~ crime_type, data = one_way_Anova_df)

summary(one_way_AOV_model)


# from the anova table we can see that there is a statistical significant diffences
# but we would like to know if the significance is driven by a p[articular crime type
one_way_AOV_model %>%
  TukeyHSD()


###############################################################################################
#   TWO WAY ANOVA

# Null hypothesis Ho: there is no difference in the mean of the various crime type
# Alternative hypothesis Ho: there is difference in the mean of the various crime type

# Two way Analysis of Variance to investigate if there is a statistical significant
#difference and possible interaction btw crime type and County in year 2014 also for only the successfully convicted crime
########################################################################################################

#############################################################################
#   Null Hypothesis

#   H1: The means of observations grouped by crime type are the same
#   H2: The mean of observations grouped by County are the same
#   H3: There is no interaction between the crime type and the various County


#   at Alpha = 0.05
##############################################################################

two_way_Anova_df <- POC_Ndf %>%
  group_by(County) %>%
  filter(Year == 2014) %>%
  select(Homicide, Sexual_Offences, Burglary, Robbery,
         Theft_And_Handling, Fraud_And_Forgery, Criminal_Damage, Drugs, Motoring_Offences, County, Year) %>%
  gather(key = "crime_type", value = "crime_value", -County, -Year)


two_way_Anova_df %>%
  group_by(crime_type, County) %>%
  summarise(Mean_Crime = mean(crime_value)) %>%
  arrange(Mean_Crime) %>%
  ggplot(aes(x=crime_type, y=Mean_Crime, fill=County)) +
  geom_col() +
  ggtitle("Mean Plot of crime type and Region")


# Building two way anova model
two_way_ANOVA_model <- aov(crime_value ~ crime_type * County, data = two_way_Anova_df)
summary(two_way_ANOVA_model)