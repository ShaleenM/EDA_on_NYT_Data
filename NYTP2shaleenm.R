#Import Data
data <- read.csv(url("http://stat.columbia.edu/~rachel/datasets/nyt1.csv"))
  
# categorize
data$age_cat <-cut(data$Age,c(-Inf,0,18,24,34,44,54,64,Inf))

# create categories based on Impressions
data$scode[data$Impressions==0] <- "NoImps"
data$scode[data$Impressions >0] <- "Imps"
data$scode[data$Clicks >0] <- "Clicks"

#Filter NotLogged in
loggedIn_mstr <- subset(data, data$Signed_In == 1)

#Adding Click through Rate
data$ClickThrRate <- data$Clicks/data$Impressions

# Categorise based on Click Behaviour
Clickers_data <-subset(data,ClickThrRate > 0, Impressions>0)
Clickmax <- max(Clickers_data$Clicks)
Clickers_data$clickActivity[Clickers_data$Clicks > (0.75*Clickmax)] <- "High"
Clickers_data$clickActivity[Clickers_data$Clicks > (0.25*Clickmax) & Clickers_data$Clicks <= (0.75*Clickmax)] <- "Mid"
Clickers_data$clickActivity[Clickers_data$Clicks <= (0.25*Clickmax)] <- "low"

# Logged IN cs Not Logged IN
qplot(Signed_In, data=data, geom="bar")+scale_x_discrete(breaks=c("-0.5","0","1","1.5"),labels=c("","Not_Logged_In", "Logged_In","")) 

#Distribution of Users under 18 years
loggedIn_mstr$isAdult[loggedIn_mstr$Age >=18] <- "Over 18"
loggedIn_mstr$isAdult[loggedIn_mstr$Age <18] <- "Under 18"

loggedIn_mstr$Gender[loggedIn_mstr$Gender==0] <- "Female"
loggedIn_mstr$Gender[loggedIn_mstr$Gender==1] <- "Male"

ggplot(loggedIn_mstr, aes(x=Gender, fill=isAdult))+geom_bar()

# Jitter Plot of Age vs Gender
ggplot(loggedIn_mstr, aes(Gender, Age))+geom_jitter(aes(colour = age_cat))
ggplot(subset(loggedIn_mstr, Clicks>0), aes(x=Clicks,colour=age_cat)) + geom_density()+ylab("No of Users for given Click Count")

# Male Clicked vs Female Clicked
ggplot(loggedIn_mstr, aes(Gender, Clicks))+geom_pointrange(ymin=0,ymax=5)
ggplot(subset(loggedIn_mstr, Clicks>0), aes(x=Clicks,fill=Gender)) + geom_bar()+scale_y_continuous(trans="log10", name="No of Users for given Click Count")

# Click Through Rate vs Age
ggplot(Clickers_data, aes(x=ClickThrRate, y=Age))+ geom_jitter(aes(colour = age_cat))

# Click Activity vs Ctr
qplot(ClickThrRate, data=Clickers_data, geom="density", fill = clickActivity)

#Clicks vs age
qplot(Clicks, Age, data=data, geom="jitter", colour = age_cat)


#
ggplot(subset(Clickers_data,Age>0), aes(x= Age, colour=clickActivity))+geom_density()

#categorize users based on their click behavior.
#look at levels