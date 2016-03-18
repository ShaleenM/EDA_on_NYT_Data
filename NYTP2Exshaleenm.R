library(ggplot2)
#Import Data
for(i in 1:31)
{
  #Import Data
  data <- cbind(Day=i,read.csv(url(paste("http://stat.columbia.edu/~rachel/datasets/nyt",i,".csv", sep=""))))
  # categorize
  data$age_cat <-cut(data$Age,c(-Inf,0,18,24,34,44,54,64,Inf))
  
  data$scode[data$Impressions==0] <- "NoImps"
  data$scode[data$Impressions >0] <- "Imps"
  data$scode[data$Clicks >0] <- "Clicks"
  
  #Adding Click through Rate
  data$ClickThrRate <- data$Clicks/data$Impressions
  
  #Distribution of Users under 18 years
  data$isAdult[data$Age >=18] <- "Over 18"
  data$isAdult[data$Age <18] <- "Under 18"
  
  data$Gender[data$Gender==0] <- "Female"
  data$Gender[data$Gender==1] <- "Male"
  
  if(i==1){
    master_data <-data
  }else{
    master_data <- rbind(master_data, data)
  }
}

#Filter NotLogged in
loggedIn_mstr <- subset(master_data, master_data$Signed_In == 1)

# Categorise based on Click Behaviour
Clickers_data <-subset(master_data,ClickThrRate > 0, Impressions>0)
Clickmax <- max(Clickers_data$Clicks)
Clickers_data$clickActivity[Clickers_data$Clicks > (0.75*Clickmax)] <- "High"
Clickers_data$clickActivity[Clickers_data$Clicks > (0.25*Clickmax) & Clickers_data$Clicks <= (0.75*Clickmax)] <- "Mid"
Clickers_data$clickActivity[Clickers_data$Clicks <= (0.25*Clickmax)] <- "low"

# Logged IN cs Not Logged IN
qplot(Signed_In, data=master_data, geom="bar")+scale_x_discrete(breaks=c("-0.5","0","1","1.5"),labels=c("","Not_Logged_In", "Logged_In","")) 

#Distribution of Users under 18 years
loggedIn_mstr$isAdult[loggedIn_mstr$Age >=18] <- "Over 18"
loggedIn_mstr$isAdult[loggedIn_mstr$Age <18] <- "Under 18"

loggedIn_mstr$Gender[loggedIn_mstr$Gender==0] <- "Female"
loggedIn_mstr$Gender[loggedIn_mstr$Gender==1] <- "Male"

ggplot(loggedIn_mstr, aes(x=Gender, fill=isAdult))+geom_bar()

# Jitter Plot of Age vs Gender
ggplot(loggedIn_mstr, aes(Gender, Age))+geom_jitter(aes(colour = age_cat))
ggplot(subset(loggedIn_mstr, Clicks>0), aes(x=Clicks,colour=age_cat)) + geom_density()
# Male Clicked vs Female Clicked
ggplot(loggedIn_mstr, aes(Gender, Clicks))+geom_pointrange(ymin=0,ymax=5)
ggplot(subset(loggedIn_mstr, Clicks>0), aes(x=Clicks,fill=Gender)) + geom_histogram()+scale_y_continuous(trans="log10", name="density") 

# Click Through Rate vs Ages
ggplot(Clickers_data, aes(x=ClickThrRate, y=Age))+ geom_density(aes(colour = age_cat))

# Click Activity vs Ctr
qplot(ClickThrRate, data=Clickers_data, geom="density", fill = clickActivity)

#Clicks vs age
qplot(Clicks, Age, data=master_data, geom="jitter", colour = age_cat)
ggplot(subset(Clickers_data,Age>0), aes(x= Age, colour=clickActivity))+geom_density()

###################################################################################
library(sqldf)
#Logged In vs Days
loggedIn_day <- sqldf("select Day, count(*) as LoggedIn from master_data where Signed_in =1 group by Day")
ggplot(loggedIn_day, aes(x=Day,y=LoggedIn))+geom_bar(stat="identity", position ="dodge")

NotloggedIn_day <- sqldf("select Day, count(*) Not_loggedIn from master_data where Signed_in !=1 group by Day")
ggplot(NotloggedIn_day, aes(x=Day,y=Count))+geom_bar(stat="identity", position ="dodge")

#Total Clicks per day
Clicks_per_day <- sqldf("select Day, sum(Clicks) from loggedIn_mstr group by Day" )
ggplot(Clicks_per_day, aes(x=Clicks_per_day[,1],y=Clicks_per_day[,2]))+geom_bar(stat="identity", position ="dodge")

#Clicks per Day Gender vise
Clicks_per_day_gender <- sqldf("select Day, Gender, sum(Clicks) from loggedIn_mstr group by Day, Gender" )
ggplot(Clicks_per_day_gender, aes(x=Clicks_per_day_gender[,1],y=Clicks_per_day_gender[,3], fill = factor(Clicks_per_day_gender[,2])))+geom_bar(stat="identity", position ="dodge")

#Impressions per Day
Impression_per_day_gender <- sqldf("select Day, Gender, sum(Impressions) from loggedIn_mstr group by Day, Gender" )
ggplot(Impression_per_day_gender, aes(x=Impression_per_day_gender[,1],y=Impression_per_day_gender[,3], fill = factor(Impression_per_day_gender[,2])))+geom_bar(stat="identity", position ="dodge")

#ClickThrough Rate per Day Categarised with Ages
ggplot(Clickers_data, aes(x= Day, y=ClickThrRate, group=Day))+geom_boxplot()
