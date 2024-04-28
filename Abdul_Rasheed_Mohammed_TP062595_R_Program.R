############################################### House Dataset Deep Analysis Using R Programming #########################

#### Data Import ####

house_rent_data = read.csv("C:\\Users\\rashi\\OneDrive\\Documents\\Desktop\\R assignment\\house_rent_dataset.csv",header=TRUE)

#### Cleaning and Pre Processing ######
#Install All These Packages

install.packages("hbrthemes")
install.packages("ggthemes")
install.packages("tidyverse")
install.packages("lessR")

#Initiate these Libraries 

library(tidyverse)
library(ggthemes)
library(hbrthemes)
library(ggplot2)
library(plotrix)
library(lessR)

view(house_rent_data) # TO View the Dataset

#To Change the column names of the dataset
names(house_rent_data)=c("Date_Posted","No_of_Rooms","Monthly_Rent_Price","Size_of_house","Floor_Level","Area_Type","Locality","City","Furnishing","Tenant_Wanted","No_of_Washrooms","Person_to_Contact")

head(house_rent_data,10) #Prints First 10 rows

tail(house_rent_data,15)  #Prints last 15 rows

str(house_rent_data) # Prints the type of each column

summary(house_rent_data) #Prints overall summary and details about each column

attributes(house_rent_data) #To check the attributes of the dataset



############## Q1 What type of houses are best available for Families Only ###########


  ############################ 1.1 Comparison of Number of Family Only Houses with Furnishing preference in mind #####
house_rent_data%>%
  filter(Tenant_Wanted=="Family")%>%
  ggplot(aes(x=City,fill=Furnishing))+geom_bar(position="dodge",alpha=2)+
  ggtitle("Number of Family Only Houses with Furnishing Preference in mind")+xlab("Cities")+
  ylab("No of Houses")+theme_economist()+labs(fill="Furnishing Type")

  ############################ 1.2 Comparison of No of Family Only houses with Area Type in mind with Each city  ##############################

house_rent_data%>%
  filter(Tenant_Wanted=="Family")%>%
  ggplot(aes(x=City,fill=Area_Type))+geom_bar(position="fill",alpha=10)+
  ggtitle("Number of Family Only Houses with Floor Area preference in mind")+xlab("Cities")+
  ylab("No of Houses")+theme_economist()+labs(fill="Floor Type")

  ############################ 1.3 Comparison of Average Rent for Only Family houses in Each city  ##############################
#Filtering the data 
hyd_family_dataset1=house_rent_data[house_rent_data$City=="Hyderabad"&(house_rent_data$`Tenant_Wanted`=="Family"),]

kol_family_dataset1=house_rent_data[house_rent_data$City=="Kolkata"&(house_rent_data$`Tenant_Wanted`=="Family"),]

mum_family_dataset1=house_rent_data[house_rent_data$City=="Mumbai"&(house_rent_data$`Tenant_Wanted`=="Family"),]

ban_family_dataset1=house_rent_data[house_rent_data$City=="Bangalore"&(house_rent_data$`Tenant_Wanted`=="Family"),]

del_family_dataset1=house_rent_data[house_rent_data$City=="Delhi"&(house_rent_data$`Tenant_Wanted`=="Family"),]

che_family_dataset1=house_rent_data[house_rent_data$City=="Chennai"&(house_rent_data$`Tenant_Wanted`=="Family"),]
# Average Rent
hyd_fam_rent=mean(hyd_family_dataset1$`Monthly_Rent_Price`)

kol_fam_rent=mean(kol_family_dataset1$`Monthly_Rent_Price`)

mum_fam_rent=mean(mum_family_dataset1$`Monthly_Rent_Price`)

ban_fam_rent=mean(ban_family_dataset1$`Monthly_Rent_Price`)

del_fam_rent=mean(del_family_dataset1$`Monthly_Rent_Price`)

che_fam_rent=mean(che_family_dataset1$`Monthly_Rent_Price`)


city_name=c("Hyderabad","Kolkata","Mumbai","Chennai","Bangalore","Delhi")
avg_fam_rent=c(floor(hyd_fam_rent),floor(kol_fam_rent),floor(mum_fam_rent),floor(che_fam_rent),floor(ban_fam_rent),floor(del_fam_rent))
data <- data.frame( City=city_name ,Houses=avg_fam_rent)

### Point and Line Graph 
plot(avg_fam_rent,type="o",xlab="City",ylab="Rent Price in Rs",
     main="Comparison of Average Rent for Only Family houses in Each city",col="orange",lwd=2,xaxt = "n",)+
    axis(1,at=1:6,labels=city_name[1:6])

### Bar Graph 
ggplot(data, aes(x=City, y=Houses)) + geom_bar(stat = "identity",width=0.5,color="blue",fill="yellow")+
  ylab("Rent in RM")+geom_text(aes(label=Houses),position=position_dodge(width=0.9), vjust=-0.25)+
  ggtitle("Comparison of Average Rent for Only Family Houses in Each city")+theme_economist()

### Pie Chart 
pie(number1,city_name,radius=1,main="Comparison of Average Rent for Only Family houses in Each City",
    col=c("green","blue","red","yellow","grey","pink"),clockwise=TRUE)

  ############################ 1.4 Comparison of Average Area Size For Only Family houses in Each City################
# Filtering out the required rows
hyd_family_dataset=house_rent_data[house_rent_data$City=="Hyderabad"&(house_rent_data$`Tenant_Wanted`=="Family"),]

kol_family_dataset=house_rent_data[house_rent_data$City=="Kolkata"&(house_rent_data$`Tenant_Wanted`=="Family"),]

mum_family_dataset=house_rent_data[house_rent_data$City=="Mumbai"&(house_rent_data$`Tenant_Wanted`=="Family"),]

ban_family_dataset=house_rent_data[house_rent_data$City=="Bangalore"&(house_rent_data$`Tenant_Wanted`=="Family"),]

del_family_dataset=house_rent_data[house_rent_data$City=="Delhi"&(house_rent_data$`Tenant_Wanted`=="Family"),]

che_family_dataset=house_rent_data[house_rent_data$City=="Chennai"&(house_rent_data$`Tenant_Wanted`=="Family"),]

#Average of size of family only size
hyd_fam_size=mean(hyd_family_dataset$`Size_of_house`)

kol_fam_size=mean(kol_family_dataset$`Size_of_house`)

mum_fam_size=mean(mum_family_dataset$`Size_of_house`)

ban_fam_size=mean(ban_family_dataset$`Size_of_house`)

del_fam_size=mean(del_family_dataset$`Size_of_house`)

che_fam_size=mean(che_family_dataset$`Size_of_house`)


city_name=c("Hyderabad","Kolkata","Mumbai","Chennai","Bangalore","Delhi")
avg_fam_size=c(floor(hyd_fam_size),floor(kol_fam_size),floor(mum_fam_size),floor(che_fam_size),floor(ban_fam_size),floor(del_fam_size))
data <- data.frame( City=city_name ,Houses=avg_fam_size)

### Line Graph 
plot(avg_fam_size,type="l",xlab="City",ylab="Square Ft",main="Comparison of Average Area Size For Only Family Houses in Each City",
     col="maroon",xaxt = "n",lwd=2)+axis(1,at=1:6,labels=city_name[1:6])

### Bar Graph 
ggplot(data, aes(x=City, y=Houses)) + geom_bar(stat = "identity",width=0.5,color="blue",fill="red")+ylab("Area in Sqft")+
  geom_text(aes(label=Houses),position=position_dodge(width=0.9), vjust=-0.25)+
  ggtitle("Comparison of Average Area Size For Only Family Houses in Each City")+theme_economist_white()

### 3D Pie Chart 
pie3D(avg_fam_size,main="Comparison of Average Area Size For Only Family Houses in Each City",labels=city_name,explode=.15)


  ############################ 1.5 Comparison of No of Family Only Houses with more than 1bedroom and 1 washroom in each city ####
house_rent_data %>%
  filter(Tenant_Wanted=="Family" & No_of_Rooms>1 & No_of_Washrooms>1  )%>%
  ggplot(aes(x=No_of_Rooms,fill=City))+geom_histogram()+
  ggtitle("Comparison of No of Family Only Houses with more than 1bedroom and 1 washroom in each city")+
  theme_gdocs()+xlab("No of Rooms")+ylab("No of Houses")+stat_bin(binwidth=0.5)

  ############################ 1.6 Relation between Area size and Monthly Rent of Family Only Houses with respect to Each City ####
house_rent_data%>%
  filter(Tenant_Wanted=="Family")%>%
  ggplot(aes(x=log(Size_of_house),y=log(Monthly_Rent_Price)))+geom_point(aes(shape=factor(City),color=factor(City)))+
  scale_color_manual(values = c("red", "blue","grey","brown","orange","#226E07"))+scale_color_discrete(name = "Cities")+
  labs(shape="Cities")+
  ggtitle(" Relation between Area size and Monthly Rent of Family Only Houses with respect to each City")+
  stat_smooth(method="lm",col="black",se=FALSE,size=1)+theme_economist_white()+
  xlab("Logirithmic value of Area of House in (Sqft)")+ylab("Logirithmic value of Monthly Rent of House in (Rs)")






############## Q2 What type of houses are best available for Bachelors Only ###########

  ########################### 2.1  Comparison of Total Number of Bachelors Only Houses with Furnishing Preference in mind #####
house_rent_data%>%
  filter(Tenant_Wanted=="Bachelors")%>%
  ggplot(aes(x=City,fill=Furnishing))+geom_bar(position="fill",alpha=1)+
  ggtitle("Number of Bachelor Only Houses with Furnishing Preference in mind")+
  xlab("Cities")+ylab("No of Houses")+theme_economist()+labs(fill="Furnishing Type")

  ########################### 2.2	 Comparison of No of Bachelors Only houses with Area Type in mind with Each city  ##############################

house_rent_data%>%
  filter(Tenant_Wanted=="Bachelors")%>%
  ggplot(aes(x=City,fill=Area_Type))+geom_bar(position="dodge",alpha=1,color="black")+
  ggtitle("Number of Family Only Houses with Area Type Preference in mind")+
  xlab("Cities")+ylab("No of Houses")+theme()+labs(fill="Area Type")

  ########################### 2.3	 Comparison of Average Rent for Only Bachelors houses in Each city  ##############################
#Filtering Bachelor Records
hyd_bach_dataset2=house_rent_data[house_rent_data$City=="Hyderabad"&(house_rent_data$`Tenant_Wanted`=="Bachelors"),]

kol_bach_dataset2=house_rent_data[house_rent_data$City=="Kolkata"&(house_rent_data$`Tenant_Wanted`=="Bachelors"),]

mum_bach_dataset2=house_rent_data[house_rent_data$City=="Mumbai"&(house_rent_data$`Tenant_Wanted`=="Bachelors"),]

ban_bach_dataset2=house_rent_data[house_rent_data$City=="Bangalore"&(house_rent_data$`Tenant_Wanted`=="Bachelors"),]

del_bach_dataset2=house_rent_data[house_rent_data$City=="Delhi"&(house_rent_data$`Tenant_Wanted`=="Bachelors"),]

che_bach_dataset2=house_rent_data[house_rent_data$City=="Chennai"&(house_rent_data$`Tenant_Wanted`=="Bachelors"),]
#Finding Average Rent in Each City

hyd_bach_rent=mean(hyd_bach_dataset2$`Monthly_Rent_Price`)

kol_bach_rent=mean(kol_bach_dataset2$`Monthly_Rent_Price`)

mum_bach_rent=mean(mum_bach_dataset2$`Monthly_Rent_Price`)

ban_bach_rent=mean(ban_bach_dataset2$`Monthly_Rent_Price`)

del_bach_rent=mean(del_bach_dataset2$`Monthly_Rent_Price`)

che_bach_rent=mean(che_bach_dataset2$`Monthly_Rent_Price`)


city_name=c("Hyderabad","Kolkata","Mumbai","Chennai","Bangalore","Delhi")
number1=c(floor(hyd_bach_rent),floor(kol_bach_rent),floor(mum_bach_rent),floor(che_bach_rent),floor(ban_bach_rent),floor(del_bach_rent))
data <- data.frame( City=city_name ,Houses=number1)

### Point Graph
plot(number1,type="p",xlab="City",ylab="Rent Price",main="Average Rent for Only Bachelors houses in Each city",col="blue",lwd=2,xaxt = "n",)+
  axis(1,at=1:6,labels=city_name[1:6])

### Bar Graph
ggplot(data, aes(x=City, y=Houses)) + geom_bar(stat = "identity",width=0.5,color="blue",fill="turquoise")+ylab("Rent in Rs")+
  geom_text(aes(label=Houses),position=position_dodge(width=0.9), vjust=-0.25,color="white")+
  ggtitle("Comparison of Average Rent for Only Bachelor houses in Each city")+theme_dark()

### 3D Pie graph
pie3D(number1,main="Comparison of Average Rent for Only Bachelors houses in Each City",labels=city_name,explode=.15)



  ########################### 2.4  Comparison of Average Area Size For Only Bachelors houses in Each City################
#Filtering Bachelor Records
hyd_bach_dataset1=house_rent_data[house_rent_data$City=="Hyderabad"&(house_rent_data$`Tenant_Wanted`=="Bachelors"),]

kol_bach_dataset1=house_rent_data[house_rent_data$City=="Kolkata"&(house_rent_data$`Tenant_Wanted`=="Bachelors"),]

mum_bach_dataset1=house_rent_data[house_rent_data$City=="Mumbai"&(house_rent_data$`Tenant_Wanted`=="Bachelors"),]

ban_bach_dataset1=house_rent_data[house_rent_data$City=="Bangalore"&(house_rent_data$`Tenant_Wanted`=="Bachelors"),]

del_bach_dataset1=house_rent_data[house_rent_data$City=="Delhi"&(house_rent_data$`Tenant_Wanted`=="Bachelors"),]

che_bach_dataset1=house_rent_data[house_rent_data$City=="Chennai"&(house_rent_data$`Tenant_Wanted`=="Bachelors"),]

#Finding Average Rent in Each City
hyd_bach_size=mean(hyd_bach_dataset1$`Size_of_house`)

kol_bach_size=mean(kol_bach_dataset1$`Size_of_house`)

mum_bach_size=mean(mum_bach_dataset1$`Size_of_house`)

ban_bach_size=mean(ban_bach_dataset1$`Size_of_house`)

del_bach_size=mean(del_bach_dataset1$`Size_of_house`)

che_bach_size=mean(che_bach_dataset1$`Size_of_house`)


city_name=c("Hyderabad","Kolkata","Mumbai","Chennai","Bangalore","Delhi")
number1=c(floor(hyd_bach_size),floor(kol_bach_size),floor(mum_bach_size),floor(che_bach_size),floor(ban_bach_size),floor(del_bach_size))
data <- data.frame( City=city_name ,Houses=number1)

### Line Graph
plot(number1,type="o",xlab="City",ylab="Square Ft",main="Comparison of Average Area Size For Only Bachelors Houses in Each City",col="orange",xaxt = "n",)+
  axis(1,at=1:6,labels=city_name[1:6])

### Bar Graph
ggplot(data, aes(x=City, y=Houses)) + geom_bar(stat = "identity",width=0.5,color="blue",fill="#219CE7")+ylab("Area in Sqft")+
  geom_text(aes(label=Houses),position=position_dodge(width=0.9), vjust=-0.25)+
  ggtitle("Comparison of Average Area Size For Only Bachelors houses in Each City")+theme_grey()

### Pie
pie(number1,city_name,radius=1,main="Comparison of Average Area Size For Only Bachelors Houses in Each Cityy",
    col=c("green","blue","red","yellow","grey","pink"),clockwise=TRUE)




  ########################### 2.5  Comparison of No of Bachelors Only Houses with No of Rooms with respect to each city ####
house_rent_data %>%
  filter(Tenant_Wanted=="Bachelors"   )%>%
  ggplot(aes(x=No_of_Rooms,fill=City))+geom_density(alpha=0.6)+ggtitle("Comparison of No of Bachelors Only Houses with No of Rooms in each city")+
  theme_dark()+xlab("No of Rooms")+ylab("No of Houses")






  ########################### 2.6 Relation between Area size and Monthly Rent of Bachelors Only Houses with respect to Each City ####
house_rent_data%>%
  filter(Tenant_Wanted=="Bachelors")%>%
  ggplot(aes(x=log(Size_of_house),y=log(Monthly_Rent_Price)))+geom_point(aes(shape=factor(City),color=factor(City)))+
  scale_color_manual(values = c("red", "blue","grey","brown","orange","#226E07"))+scale_color_discrete(name = "Cities")+
  labs(shape="Cities")+
  ggtitle(" Relation between Area size and Monthly Rent of Bachelors Only Houses with respect to each City")+
  stat_smooth(method="lm",col="black",se=FALSE,size=1)+theme_bw()+
  xlab("Logirithmic value of Area of House in (Sqft)")+ylab("Logirithmic value of Monthly Rent of House in (Rs)")


############# Q3 Which city is the cheapest for Bachelors Only Housing ################

############################# 3.1 Cheapest Rent for Bachelors Only Houses Based on Area type
################################## 3.1.1 No of Houses Bachelors Only House Below Average Rent Based on Super Type Preference #######

#Filtering Records according to city
bach_hyd=house_rent_data[house_rent_data$City=="Hyderabad"&(house_rent_data$`Tenant_Wanted`=="Bachelors")&(house_rent_data$Area_Type=="Super Area"),]

bach_kol=house_rent_data[house_rent_data$City=="Kolkata"&(house_rent_data$`Tenant_Wanted`=="Bachelors")&(house_rent_data$Area_Type=="Super Area"),]

bach_mum=house_rent_data[house_rent_data$City=="Mumbai"&(house_rent_data$`Tenant_Wanted`=="Bachelors")&(house_rent_data$Area_Type=="Super Area"),]

bach_ban=house_rent_data[house_rent_data$City=="Bangalore"&(house_rent_data$`Tenant_Wanted`=="Bachelors")&(house_rent_data$Area_Type=="Super Area"),]

bach_del=house_rent_data[house_rent_data$City=="Delhi"&(house_rent_data$`Tenant_Wanted`=="Bachelors") &(house_rent_data$Area_Type=="Super Area"),]

bach_che=house_rent_data[house_rent_data$City=="Chennai"&(house_rent_data$`Tenant_Wanted`=="Bachelors")&(house_rent_data$Area_Type=="Super Area"),]

#Finding average of each city

r1=mean(bach_hyd$`Monthly_Rent_Price`)

r2=mean(bach_kol$`Monthly_Rent_Price`)

r3=mean(bach_mum$`Monthly_Rent_Price`)

r4=mean(bach_ban$`Monthly_Rent_Price`)

r5=mean(bach_del$`Monthly_Rent_Price`)

r6=mean(bach_che$`Monthly_Rent_Price`)

below_hyd=house_rent_data[house_rent_data$City=="Hyderabad"&(house_rent_data$`Tenant_Wanted`=="Bachelors")&(house_rent_data$Area_Type=="Super Area") &(house_rent_data$Monthly_Rent_Price<r1),]

below_kol=house_rent_data[house_rent_data$City=="Kolkata"&(house_rent_data$`Tenant_Wanted`=="Bachelors")&(house_rent_data$Area_Type=="Super Area") &(house_rent_data$Monthly_Rent_Price<r2),]

below_mum=house_rent_data[house_rent_data$City=="Mumbai"&(house_rent_data$`Tenant_Wanted`=="Bachelors")&(house_rent_data$Area_Type=="Super Area")&(house_rent_data$Monthly_Rent_Price<r3),]

below_ban=house_rent_data[house_rent_data$City=="Bangalore"&(house_rent_data$`Tenant_Wanted`=="Bachelors")&(house_rent_data$Area_Type=="Super Area") &(house_rent_data$Monthly_Rent_Price<r4),]

below_del=house_rent_data[house_rent_data$City=="Delhi"&(house_rent_data$`Tenant_Wanted`=="Bachelors") &(house_rent_data$Area_Type=="Super Area") &(house_rent_data$Monthly_Rent_Price<r5),]

below_che=house_rent_data[house_rent_data$City=="Chennai"&(house_rent_data$`Tenant_Wanted`=="Bachelors")&(house_rent_data$Area_Type=="Super Area")&(house_rent_data$Monthly_Rent_Price<r6),]

f1=nrow(below_hyd)
f2=nrow(below_kol)
f3=nrow(below_mum)
f4=nrow(below_ban)
f5=nrow(below_del)
f6=nrow(below_che)


city_name=c("Hyderabad","Kolkata","Mumbai","Chennai","Bangalore","Delhi")
number1=c(f1,f2,f3,f6,f4,f5)
data <- data.frame( City=city_name ,Houses=number1)

#Bar Graph
ggplot(data, aes(x=City, y=Houses)) + geom_bar(stat = "identity",width=0.5,color="red",fill="cyan")+ylab("No of Houses")+
  geom_text(aes(label=Houses),position=position_dodge(width=0.9), vjust=-0.25)+ggtitle("No of Houses for Rent which are below the Average Monthly Rent and Have Super Area in each City")+theme_bw()


################################## 3.1.2 Cheapest Rent for Bachelors Only Houses Based on Carpet type ########

#Filtering Records according to city
bach_hyd=house_rent_data[house_rent_data$City=="Hyderabad"&(house_rent_data$`Tenant_Wanted`=="Bachelors")&(house_rent_data$Area_Type=="Carpet Area"),]

bach_kol=house_rent_data[house_rent_data$City=="Kolkata"&(house_rent_data$`Tenant_Wanted`=="Bachelors")&(house_rent_data$Area_Type=="Carpet Area"),]

bach_mum=house_rent_data[house_rent_data$City=="Mumbai"&(house_rent_data$`Tenant_Wanted`=="Bachelors")&(house_rent_data$Area_Type=="Carpet Area"),]

bach_ban=house_rent_data[house_rent_data$City=="Bangalore"&(house_rent_data$`Tenant_Wanted`=="Bachelors")&(house_rent_data$Area_Type=="Carpet Area"),]

bach_del=house_rent_data[house_rent_data$City=="Delhi"&(house_rent_data$`Tenant_Wanted`=="Bachelors") &(house_rent_data$Area_Type=="Carpet Area"),]

bach_che=house_rent_data[house_rent_data$City=="Chennai"&(house_rent_data$`Tenant_Wanted`=="Bachelors")&(house_rent_data$Area_Type=="Carpet Area"),]

#Finding average of each city

r1=mean(bach_hyd$`Monthly_Rent_Price`)

r2=mean(bach_kol$`Monthly_Rent_Price`)

r3=mean(bach_mum$`Monthly_Rent_Price`)

r4=mean(bach_ban$`Monthly_Rent_Price`)

r5=mean(bach_del$`Monthly_Rent_Price`)

r6=mean(bach_che$`Monthly_Rent_Price`)

below_hyd=house_rent_data[house_rent_data$City=="Hyderabad"&(house_rent_data$`Tenant_Wanted`=="Bachelors")&(house_rent_data$Area_Type=="Carpet Area") &(house_rent_data$Monthly_Rent_Price<r1),]

below_kol=house_rent_data[house_rent_data$City=="Kolkata"&(house_rent_data$`Tenant_Wanted`=="Bachelors")&(house_rent_data$Area_Type=="Carpet Area") &(house_rent_data$Monthly_Rent_Price<r2),]

below_mum=house_rent_data[house_rent_data$City=="Mumbai"&(house_rent_data$`Tenant_Wanted`=="Bachelors")&(house_rent_data$Area_Type=="Carpet Area")&(house_rent_data$Monthly_Rent_Price<r3),]

below_ban=house_rent_data[house_rent_data$City=="Bangalore"&(house_rent_data$`Tenant_Wanted`=="Bachelors")&(house_rent_data$Area_Type=="Carpet Area") &(house_rent_data$Monthly_Rent_Price<r4),]

below_del=house_rent_data[house_rent_data$City=="Delhi"&(house_rent_data$`Tenant_Wanted`=="Bachelors") &(house_rent_data$Area_Type=="Carpet Area") &(house_rent_data$Monthly_Rent_Price<r5),]

below_che=house_rent_data[house_rent_data$City=="Chennai"&(house_rent_data$`Tenant_Wanted`=="Bachelors")&(house_rent_data$Area_Type=="Carpet Area")&(house_rent_data$Monthly_Rent_Price<r6),]

f1=nrow(below_hyd)
f2=nrow(below_kol)
f3=nrow(below_mum)
f4=nrow(below_ban)
f5=nrow(below_del)
f6=nrow(below_che)


city_name=c("Hyderabad","Kolkata","Mumbai","Chennai","Bangalore","Delhi")
number1=c(f1,f2,f3,f6,f4,f5)
data <- data.frame( City=city_name ,Houses=number1)

#Bar Graph
ggplot(data, aes(x=City, y=Houses)) + geom_bar(stat = "identity",width=0.5,color="red",fill="orange")+ylab("No of Houses")+
  geom_text(aes(label=Houses),position=position_dodge(width=0.9), vjust=-0.25)+ggtitle("No of Houses for Rent which are below the Average Monthly Rent and have Carpet Area in Each City")+theme_clean()


############################# 3.2 Cheapest Rent for Bachelors Only Houses Based on Furnishing Type#########
################################## 3.2.1 Cheapest Rent for Bachelors Only Houses For Furnished Houses ###########

bach_furn_hyd=house_rent_data[house_rent_data$City=="Hyderabad"&(house_rent_data$`Tenant_Wanted`=="Bachelors") &(house_rent_data$Furnishing=="Furnished"),]

bach_furn_kol=house_rent_data[house_rent_data$City=="Kolkata"&(house_rent_data$`Tenant_Wanted`=="Bachelors") &(house_rent_data$Furnishing=="Furnished"),]

bach_furn_mum=house_rent_data[house_rent_data$City=="Mumbai"&(house_rent_data$`Tenant_Wanted`=="Bachelors") &(house_rent_data$Furnishing=="Furnished"),]

bach_furn_ban=house_rent_data[house_rent_data$City=="Bangalore"&(house_rent_data$`Tenant_Wanted`=="Bachelors")&(house_rent_data$Furnishing=="Furnished"),]

bach_furn_del=house_rent_data[house_rent_data$City=="Delhi"&(house_rent_data$`Tenant_Wanted`=="Bachelors") &(house_rent_data$Furnishing=="Furnished"),]

bach_furn_che=house_rent_data[house_rent_data$City=="Chennai"&(house_rent_data$`Tenant_Wanted`=="Bachelors")&(house_rent_data$Furnishing=="Furnished"),]


r1=min(bach_furn_hyd$`Monthly_Rent_Price`)

r2=min(bach_furn_kol$`Monthly_Rent_Price`)

r3=min(bach_furn_mum$`Monthly_Rent_Price`)

r4=min(bach_furn_ban$`Monthly_Rent_Price`)

r5=min(bach_furn_del$`Monthly_Rent_Price`)

r6=min(bach_furn_che$`Monthly_Rent_Price`)

city_name=c("Hyderabad","Kolkata","Mumbai","Chennai","Bangalore","Delhi")
number1=c(floor(r1),floor(r2),floor(r3),floor(r6),floor(r4),floor(r5))
data <- data.frame( City=city_name ,Houses=number1)

pie(number1,radius=.5,main="Rent of the Cheapest Bachelor Only Furnished House in Each City",
    col=c("green","blue","red","yellow","grey","pink"),clockwise=TRUE,labels=paste0("Rs ",number1))
legend("topright", legend = c("Hyderabad", "Kolkata", "Mumbai","Chennai","Bangalore","Delhi"),
       fill =  c("green","blue","red","yellow","grey","pink"))

################################## 3.2.2 Cheapest Rent for Bachelors Only Houses For Semi-Furnished Houses #########

bach_furn_hyd1=house_rent_data[house_rent_data$City=="Hyderabad"&(house_rent_data$`Tenant_Wanted`=="Bachelors") &(house_rent_data$Furnishing=="Semi-Furnished"),]

bach_furn_kol1=house_rent_data[house_rent_data$City=="Kolkata"&(house_rent_data$`Tenant_Wanted`=="Bachelors") &(house_rent_data$Furnishing=="Semi-Furnished"),]

bach_furn_mum1=house_rent_data[house_rent_data$City=="Mumbai"&(house_rent_data$`Tenant_Wanted`=="Bachelors") &(house_rent_data$Furnishing=="Semi-Furnished"),]

bach_furn_ban1=house_rent_data[house_rent_data$City=="Bangalore"&(house_rent_data$`Tenant_Wanted`=="Bachelors")&(house_rent_data$Furnishing=="Semi-Furnished"),]

bach_furn_del1=house_rent_data[house_rent_data$City=="Delhi"&(house_rent_data$`Tenant_Wanted`=="Bachelors") &(house_rent_data$Furnishing=="Semi-Furnished"),]

bach_furn_che1=house_rent_data[house_rent_data$City=="Chennai"&(house_rent_data$`Tenant_Wanted`=="Bachelors")&(house_rent_data$Furnishing=="Semi-Furnished"),]


r1=min(bach_furn_hyd1$`Monthly_Rent_Price`)

r2=min(bach_furn_kol1$`Monthly_Rent_Price`)

r3=min(bach_furn_mum1$`Monthly_Rent_Price`)

r4=min(bach_furn_ban1$`Monthly_Rent_Price`)

r5=min(bach_furn_del1$`Monthly_Rent_Price`)

r6=min(bach_furn_che1$`Monthly_Rent_Price`)

city_name=c("Hyderabad","Kolkata","Mumbai","Chennai","Bangalore","Delhi")
number1=c(floor(r1),floor(r2),floor(r3),floor(r6),floor(r4),floor(r5))
data2 <- data.frame( City=city_name ,Houses=number1)

pie(number1,radius=.5,main="Rent of the Cheapest Bachelor Only Semi- Furnished House in Each City",
    col=c("green","blue","red","yellow","grey","pink"),clockwise=TRUE,labels=paste0("Rs ",number1))
legend("topright", legend = c("Hyderabad", "Kolkata", "Mumbai","Chennai","Bangalore","Delhi"),
       fill =  c("green","blue","red","yellow","grey","pink"))

############################# 3.3 Total No of Houses Available below the Average Rent For Bachelor Houses #######

g1=house_rent_data[house_rent_data$City=="Hyderabad"&(house_rent_data$`Tenant_Wanted`=="Bachelors"),]

g2=house_rent_data[house_rent_data$City=="Kolkata"&(house_rent_data$`Tenant_Wanted`=="Bachelors"),]

g3=house_rent_data[house_rent_data$City=="Mumbai"&(house_rent_data$`Tenant_Wanted`=="Bachelors") ,]

g4=house_rent_data[house_rent_data$City=="Bangalore"&(house_rent_data$`Tenant_Wanted`=="Bachelors"),]

g5=house_rent_data[house_rent_data$City=="Delhi"&(house_rent_data$`Tenant_Wanted`=="Bachelors") ,]

g6=house_rent_data[house_rent_data$City=="Chennai"&(house_rent_data$`Tenant_Wanted`=="Bachelors"),]


r1=mean(g1$`Monthly_Rent_Price`)

r2=mean(g2$`Monthly_Rent_Price`)

r3=mean(g3$`Monthly_Rent_Price`)

r4=mean(g4$`Monthly_Rent_Price`)

r5=mean(g5$`Monthly_Rent_Price`)

r6=mean(g6$`Monthly_Rent_Price`)


g7=house_rent_data[house_rent_data$City=="Hyderabad"&(house_rent_data$`Tenant_Wanted`=="Bachelors")& (house_rent_data$Monthly_Rent_Price<r1),]

g8=house_rent_data[house_rent_data$City=="Kolkata"&(house_rent_data$`Tenant_Wanted`=="Bachelors") & (house_rent_data$Monthly_Rent_Price<r2),]

g9=house_rent_data[house_rent_data$City=="Mumbai"&(house_rent_data$`Tenant_Wanted`=="Bachelors") & (house_rent_data$Monthly_Rent_Price<r3) ,]

g10=house_rent_data[house_rent_data$City=="Bangalore"&(house_rent_data$`Tenant_Wanted`=="Bachelors") & (house_rent_data$Monthly_Rent_Price<r4),]

g11=house_rent_data[house_rent_data$City=="Delhi"&(house_rent_data$`Tenant_Wanted`=="Bachelors") & (house_rent_data$Monthly_Rent_Price<r5),]

g12=house_rent_data[house_rent_data$City=="Chennai"&(house_rent_data$`Tenant_Wanted`=="Bachelors") & (house_rent_data$Monthly_Rent_Price<r6),]

f1=nrow(g7)
f2=nrow(g8)
f3=nrow(g9)
f4=nrow(g10)
f5=nrow(g11)
f6=nrow(g12)


city_name=c("Hyderabad","Kolkata","Mumbai","Chennai","Bangalore","Delhi")
number1=c(f1,f2,f3,f6,f4,f5)
data <- data.frame( City=city_name ,Houses=number1)

#Bar Graph
ggplot(data, aes(x=City, y=Houses)) + geom_bar(stat = "identity",width=0.5,alpha=2,fill="#F2BE19",color="black")+ylab("No of Houses")+
  geom_text(aes(label=Houses),position=position_dodge(width=0.9), vjust=-0.25)+ggtitle("Total No of Bachelor Only Houses below the average rent in each city")+
  theme_bw()








############# Q4 Which city is the cheapest for Family Only Housing ################

############################# 4.1 Cheapest Rent for Family Only Houses Based on Area type
################################## 4.1.1 Cheapest Rent for Family Only Houses Based on Super Type #######

#Filtering Records according to city
bach_hyd=house_rent_data[house_rent_data$City=="Hyderabad"&(house_rent_data$`Tenant_Wanted`=="Family")&(house_rent_data$Area_Type=="Super Area"),]

bach_kol=house_rent_data[house_rent_data$City=="Kolkata"&(house_rent_data$`Tenant_Wanted`=="Family")&(house_rent_data$Area_Type=="Super Area"),]

bach_mum=house_rent_data[house_rent_data$City=="Mumbai"&(house_rent_data$`Tenant_Wanted`=="Family")&(house_rent_data$Area_Type=="Super Area"),]

bach_ban=house_rent_data[house_rent_data$City=="Bangalore"&(house_rent_data$`Tenant_Wanted`=="Family")&(house_rent_data$Area_Type=="Super Area"),]

bach_del=house_rent_data[house_rent_data$City=="Delhi"&(house_rent_data$`Tenant_Wanted`=="Family") &(house_rent_data$Area_Type=="Super Area"),]

bach_che=house_rent_data[house_rent_data$City=="Chennai"&(house_rent_data$`Tenant_Wanted`=="Family")&(house_rent_data$Area_Type=="Super Area"),]

#Finding average of each city

r1=mean(bach_hyd$`Monthly_Rent_Price`)

r2=mean(bach_kol$`Monthly_Rent_Price`)

r3=mean(bach_mum$`Monthly_Rent_Price`)

r4=mean(bach_ban$`Monthly_Rent_Price`)

r5=mean(bach_del$`Monthly_Rent_Price`)

r6=mean(bach_che$`Monthly_Rent_Price`)

below_hyd=house_rent_data[house_rent_data$City=="Hyderabad"&(house_rent_data$`Tenant_Wanted`=="Family")&(house_rent_data$Area_Type=="Super Area") &(house_rent_data$Monthly_Rent_Price<r1),]

below_kol=house_rent_data[house_rent_data$City=="Kolkata"&(house_rent_data$`Tenant_Wanted`=="Family")&(house_rent_data$Area_Type=="Super Area") &(house_rent_data$Monthly_Rent_Price<r2),]

below_mum=house_rent_data[house_rent_data$City=="Mumbai"&(house_rent_data$`Tenant_Wanted`=="Family")&(house_rent_data$Area_Type=="Super Area")&(house_rent_data$Monthly_Rent_Price<r3),]

below_ban=house_rent_data[house_rent_data$City=="Bangalore"&(house_rent_data$`Tenant_Wanted`=="Family")&(house_rent_data$Area_Type=="Super Area") &(house_rent_data$Monthly_Rent_Price<r4),]

below_del=house_rent_data[house_rent_data$City=="Delhi"&(house_rent_data$`Tenant_Wanted`=="Family") &(house_rent_data$Area_Type=="Super Area") &(house_rent_data$Monthly_Rent_Price<r5),]

below_che=house_rent_data[house_rent_data$City=="Chennai"&(house_rent_data$`Tenant_Wanted`=="Family")&(house_rent_data$Area_Type=="Super Area")&(house_rent_data$Monthly_Rent_Price<r6),]

f1=nrow(below_hyd)
f2=nrow(below_kol)
f3=nrow(below_mum)
f4=nrow(below_ban)
f5=nrow(below_del)
f6=nrow(below_che)


city_name=c("Hyderabad","Kolkata","Mumbai","Chennai","Bangalore","Delhi")
number1=c(f1,f2,f3,f6,f4,f5)
data <- data.frame( City=city_name ,Houses=number1)

#Bar Graph
ggplot(data, aes(x=City, y=Houses)) + geom_bar(stat = "identity",width=0.5,color="red",fill="yellow")+ylab("No of Houses")+
  geom_text(aes(label=Houses),position=position_dodge(width=0.9), vjust=-0.25)+
  ggtitle("No of Family Only Houses for Rent which are below the Average Monthly Rent and Have Super Area in each City")+theme_bw()

################################## 4.1.2 Cheapest Rent for Family Only Houses Based on Carpet type ########

#Filtering Records according to city
bach_hyd=house_rent_data[house_rent_data$City=="Hyderabad"&(house_rent_data$`Tenant_Wanted`=="Family")&(house_rent_data$Area_Type=="Carpet Area"),]

bach_kol=house_rent_data[house_rent_data$City=="Kolkata"&(house_rent_data$`Tenant_Wanted`=="Family")&(house_rent_data$Area_Type=="Carpet Area"),]

bach_mum=house_rent_data[house_rent_data$City=="Mumbai"&(house_rent_data$`Tenant_Wanted`=="Family")&(house_rent_data$Area_Type=="Carpet Area"),]

bach_ban=house_rent_data[house_rent_data$City=="Bangalore"&(house_rent_data$`Tenant_Wanted`=="Family")&(house_rent_data$Area_Type=="Carpet Area"),]

bach_del=house_rent_data[house_rent_data$City=="Delhi"&(house_rent_data$`Tenant_Wanted`=="Family") &(house_rent_data$Area_Type=="Carpet Area"),]

bach_che=house_rent_data[house_rent_data$City=="Chennai"&(house_rent_data$`Tenant_Wanted`=="Family")&(house_rent_data$Area_Type=="Carpet Area"),]

#Finding average of each city

r1=mean(bach_hyd$`Monthly_Rent_Price`)

r2=mean(bach_kol$`Monthly_Rent_Price`)

r3=mean(bach_mum$`Monthly_Rent_Price`)

r4=mean(bach_ban$`Monthly_Rent_Price`)

r5=mean(bach_del$`Monthly_Rent_Price`)

r6=mean(bach_che$`Monthly_Rent_Price`)

below_hyd=house_rent_data[house_rent_data$City=="Hyderabad"&(house_rent_data$`Tenant_Wanted`=="Family")&(house_rent_data$Area_Type=="Carpet Area") &(house_rent_data$Monthly_Rent_Price<r1),]

below_kol=house_rent_data[house_rent_data$City=="Kolkata"&(house_rent_data$`Tenant_Wanted`=="Family")&(house_rent_data$Area_Type=="Carpet Area") &(house_rent_data$Monthly_Rent_Price<r2),]

below_mum=house_rent_data[house_rent_data$City=="Mumbai"&(house_rent_data$`Tenant_Wanted`=="Family")&(house_rent_data$Area_Type=="Carpet Area")&(house_rent_data$Monthly_Rent_Price<r3),]

below_ban=house_rent_data[house_rent_data$City=="Bangalore"&(house_rent_data$`Tenant_Wanted`=="Family")&(house_rent_data$Area_Type=="Carpet Area") &(house_rent_data$Monthly_Rent_Price<r4),]

below_del=house_rent_data[house_rent_data$City=="Delhi"&(house_rent_data$`Tenant_Wanted`=="Family") &(house_rent_data$Area_Type=="Carpet Area") &(house_rent_data$Monthly_Rent_Price<r5),]

below_che=house_rent_data[house_rent_data$City=="Chennai"&(house_rent_data$`Tenant_Wanted`=="Family")&(house_rent_data$Area_Type=="Carpet Area")&(house_rent_data$Monthly_Rent_Price<r6),]

f1=nrow(below_hyd)
f2=nrow(below_kol)
f3=nrow(below_mum)
f4=nrow(below_ban)
f5=nrow(below_del)
f6=nrow(below_che)


city_name=c("Hyderabad","Kolkata","Mumbai","Chennai","Bangalore","Delhi")
number1=c(f1,f2,f3,f6,f4,f5)
data <- data.frame( City=city_name ,Houses=number1)

#Bar Graph
ggplot(data, aes(x=City, y=Houses)) + geom_bar(stat = "identity",width=0.5,color="red",fill="green")+ylab("No of Houses")+
  geom_text(aes(label=Houses),position=position_dodge(width=0.9), vjust=-0.25)+
  ggtitle("No of Family Only Houses for Rent which are below the Average Monthly Rent and have Carpet Area in Each City")+theme_economist()



############################# 4.2 Cheapest Rent for Bachelors Only Houses Based on Furnishing Type##########
################################## 4.2.1 Cheapest Rent for Family Only Houses For Furnished Houses ###########

bach_furn_hyd=house_rent_data[house_rent_data$City=="Hyderabad"&(house_rent_data$`Tenant_Wanted`=="Family") &(house_rent_data$Furnishing=="Furnished"),]

bach_furn_kol=house_rent_data[house_rent_data$City=="Kolkata"&(house_rent_data$`Tenant_Wanted`=="Family") &(house_rent_data$Furnishing=="Furnished"),]

bach_furn_mum=house_rent_data[house_rent_data$City=="Mumbai"&(house_rent_data$`Tenant_Wanted`=="Family") &(house_rent_data$Furnishing=="Furnished"),]

bach_furn_ban=house_rent_data[house_rent_data$City=="Bangalore"&(house_rent_data$`Tenant_Wanted`=="Family")&(house_rent_data$Furnishing=="Furnished"),]

bach_furn_del=house_rent_data[house_rent_data$City=="Delhi"&(house_rent_data$`Tenant_Wanted`=="Family") &(house_rent_data$Furnishing=="Furnished"),]

bach_furn_che=house_rent_data[house_rent_data$City=="Chennai"&(house_rent_data$`Tenant_Wanted`=="Family")&(house_rent_data$Furnishing=="Furnished"),]


r1=min(bach_furn_hyd$`Monthly_Rent_Price`)

r2=min(bach_furn_kol$`Monthly_Rent_Price`)

r3=min(bach_furn_mum$`Monthly_Rent_Price`)

r4=min(bach_furn_ban$`Monthly_Rent_Price`)

r5=min(bach_furn_del$`Monthly_Rent_Price`)

r6=min(bach_furn_che$`Monthly_Rent_Price`)

city_name=c("Hyderabad","Kolkata","Mumbai","Chennai","Bangalore","Delhi")
number1=c(floor(r1),floor(r2),floor(r3),floor(r6),floor(r4),floor(r5))
data <- data.frame( City=city_name ,Houses=number1)

pie(number1,radius=1.5,main="Rent of the Cheapest Family Only Furnished House in Each City",
    col=c("green","blue","red","yellow","grey","pink"),clockwise=TRUE,labels=paste0("Rs ",number1))
legend("topright", legend = c("Hyderabad", "Kolkata", "Mumbai","Chennai","Bangalore","Delhi"),
       fill =  c("green","blue","red","yellow","grey","pink"))

################################## 4.2.2 Cheapest Rent for Family Only Houses For Semi-Furnished Houses #########

bach_furn_hyd=house_rent_data[house_rent_data$City=="Hyderabad"&(house_rent_data$`Tenant_Wanted`=="Family") &(house_rent_data$Furnishing=="Semi-Furnished"),]

bach_furn_kol=house_rent_data[house_rent_data$City=="Kolkata"&(house_rent_data$`Tenant_Wanted`=="Family") &(house_rent_data$Furnishing=="Semi-Furnished"),]

bach_furn_mum=house_rent_data[house_rent_data$City=="Mumbai"&(house_rent_data$`Tenant_Wanted`=="Family") &(house_rent_data$Furnishing=="Semi-Furnished"),]

bach_furn_ban=house_rent_data[house_rent_data$City=="Bangalore"&(house_rent_data$`Tenant_Wanted`=="Family")&(house_rent_data$Furnishing=="Semi-Furnished"),]

bach_furn_del=house_rent_data[house_rent_data$City=="Delhi"&(house_rent_data$`Tenant_Wanted`=="Family") &(house_rent_data$Furnishing=="Semi-Furnished"),]

bach_furn_che=house_rent_data[house_rent_data$City=="Chennai"&(house_rent_data$`Tenant_Wanted`=="Family")&(house_rent_data$Furnishing=="Semi-Furnished"),]


r1=min(bach_furn_hyd$`Monthly_Rent_Price`)

r2=min(bach_furn_kol$`Monthly_Rent_Price`)

r3=min(bach_furn_mum$`Monthly_Rent_Price`)

r4=min(bach_furn_ban$`Monthly_Rent_Price`)

r5=min(bach_furn_del$`Monthly_Rent_Price`)

r6=min(bach_furn_che$`Monthly_Rent_Price`)

city_name=c("Hyderabad","Kolkata","Mumbai","Chennai","Bangalore","Delhi")
number1=c(floor(r1),floor(r2),floor(r3),floor(r6),floor(r4),floor(r5))
data <- data.frame( City=city_name ,Houses=number1)

pie(number1,radius=.5,main="Rent of the Cheapest Family Only Semi-Furnished House in Each City",
    col=c("green","blue","red","yellow","grey","pink"),clockwise=TRUE,labels=paste0("Rs ",number1))
legend("topright", legend = c("Hyderabad", "Kolkata", "Mumbai","Chennai","Bangalore","Delhi"),
       fill =  c("green","blue","red","yellow","grey","pink"))


############################ 3.3 Cheapest Rent for Bachelors Only based on no of rooms in Each City 


############################# 4.3 No of Houses Available below the Average Rent For Family Houses #######

g1=house_rent_data[house_rent_data$City=="Hyderabad"&(house_rent_data$`Tenant_Wanted`=="Family"),]

g2=house_rent_data[house_rent_data$City=="Kolkata"&(house_rent_data$`Tenant_Wanted`=="Family"),]

g3=house_rent_data[house_rent_data$City=="Mumbai"&(house_rent_data$`Tenant_Wanted`=="Family") ,]

g4=house_rent_data[house_rent_data$City=="Bangalore"&(house_rent_data$`Tenant_Wanted`=="Family"),]

g5=house_rent_data[house_rent_data$City=="Delhi"&(house_rent_data$`Tenant_Wanted`=="Family") ,]

g6=house_rent_data[house_rent_data$City=="Chennai"&(house_rent_data$`Tenant_Wanted`=="Family"),]


r1=mean(g1$`Monthly_Rent_Price`)

r2=mean(g2$`Monthly_Rent_Price`)

r3=mean(g3$`Monthly_Rent_Price`)

r4=mean(g4$`Monthly_Rent_Price`)

r5=mean(g5$`Monthly_Rent_Price`)

r6=mean(g6$`Monthly_Rent_Price`)


g7=house_rent_data[house_rent_data$City=="Hyderabad"&(house_rent_data$`Tenant_Wanted`=="Family")& (house_rent_data$Monthly_Rent_Price<r1),]

g8=house_rent_data[house_rent_data$City=="Kolkata"&(house_rent_data$`Tenant_Wanted`=="Family") & (house_rent_data$Monthly_Rent_Price<r2),]

g9=house_rent_data[house_rent_data$City=="Mumbai"&(house_rent_data$`Tenant_Wanted`=="Family") & (house_rent_data$Monthly_Rent_Price<r3) ,]

g10=house_rent_data[house_rent_data$City=="Bangalore"&(house_rent_data$`Tenant_Wanted`=="Family") & (house_rent_data$Monthly_Rent_Price<r4),]

g11=house_rent_data[house_rent_data$City=="Delhi"&(house_rent_data$`Tenant_Wanted`=="Family") & (house_rent_data$Monthly_Rent_Price<r5),]

g12=house_rent_data[house_rent_data$City=="Chennai"&(house_rent_data$`Tenant_Wanted`=="Family") & (house_rent_data$Monthly_Rent_Price<r6),]

f1=nrow(g7)
f2=nrow(g8)
f3=nrow(g9)
f4=nrow(g10)
f5=nrow(g11)
f6=nrow(g12)


city_name=c("Hyderabad","Kolkata","Mumbai","Chennai","Bangalore","Delhi")
number1=c(f1,f2,f3,f6,f4,f5)
data <- data.frame( City=city_name ,Houses=number1)

#Bar Graph
ggplot(data, aes(x=City, y=Houses)) + geom_bar(stat = "identity",width=0.5,alpha=2,fill="#24EBBE",color="black")+ylab("No of Houses")+
  geom_text(aes(label=Houses),position=position_dodge(width=0.9), vjust=-0.25)+ggtitle("Total No of Family Only Houses below the average rent in each city")+
  theme_bw()

############################# 3.4 Cheapest House Among All Cities for Only Bachelor

############# Q5 Which city is the Most Expensive for Bachelors Only Housing ################

############################# 5.1 Most Expensive Rent for Bachelors Only Houses Based on Area type
################################## 5.1.1 Most Expensive Rent for Bachelors Only Houses Based on Super Type #######

bach_area_hyd=house_rent_data[house_rent_data$City=="Hyderabad"&(house_rent_data$`Tenant_Wanted`=="Bachelors") &(house_rent_data$Area_Type=="Super Area"),]

bach_area_kol=house_rent_data[house_rent_data$City=="Kolkata"&(house_rent_data$`Tenant_Wanted`=="Bachelors") &(house_rent_data$Area_Type=="Super Area"),]

bach_area_mum=house_rent_data[house_rent_data$City=="Mumbai"&(house_rent_data$`Tenant_Wanted`=="Bachelors") &(house_rent_data$Area_Type=="Super Area"),]

bach_area_ban=house_rent_data[house_rent_data$City=="Bangalore"&(house_rent_data$`Tenant_Wanted`=="Bachelors")&(house_rent_data$Area_Type=="Super Area"),]

bach_area_del=house_rent_data[house_rent_data$City=="Delhi"&(house_rent_data$`Tenant_Wanted`=="Bachelors") &(house_rent_data$Area_Type=="Super Area"),]

bach_area_che=house_rent_data[house_rent_data$City=="Chennai"&(house_rent_data$`Tenant_Wanted`=="Bachelors")&(house_rent_data$Area_Type=="Super Area"),]


r1=max(bach_area_hyd$`Monthly_Rent_Price`)

r2=max(bach_area_kol$`Monthly_Rent_Price`)

r3=max(bach_area_mum$`Monthly_Rent_Price`)

r4=max(bach_area_ban$`Monthly_Rent_Price`)

r5=max(bach_area_del$`Monthly_Rent_Price`)

r6=max(bach_area_che$`Monthly_Rent_Price`)

city_name=c("Hyderabad","Kolkata","Mumbai","Chennai","Bangalore","Delhi")
number1=c(r1,r2,r3,r6,r4,r5)
data <- data.frame( City=city_name ,Houses=number1)

#Bar Graph
ggplot(data, aes(x=City, y=Houses)) + geom_bar(stat = "identity",width=0.5,color="red",fill="white")+ylab("Rent in Rs")+
  geom_text(aes(label=Houses),position=position_dodge(width=0.9), vjust=-0.25,color="white")+ggtitle("Comparison of Rent of Most expensive unit for Bachelors with Super Type Area in Each city")+
  theme_dark()



################################## 5.1.2 Most Expensive Rent for Bachelors Only Houses Based on Carpet type ########

bach_area_hyd=house_rent_data[house_rent_data$City=="Hyderabad"&(house_rent_data$`Tenant_Wanted`=="Bachelors") &(house_rent_data$Area_Type=="Carpet Area"),]

bach_area_kol=house_rent_data[house_rent_data$City=="Kolkata"&(house_rent_data$`Tenant_Wanted`=="Bachelors") &(house_rent_data$Area_Type=="Carpet Area"),]

bach_area_mum=house_rent_data[house_rent_data$City=="Mumbai"&(house_rent_data$`Tenant_Wanted`=="Bachelors") &(house_rent_data$Area_Type=="Carpet Area"),]

bach_area_ban=house_rent_data[house_rent_data$City=="Bangalore"&(house_rent_data$`Tenant_Wanted`=="Bachelors")&(house_rent_data$Area_Type=="Carpet Area"),]

bach_area_del=house_rent_data[house_rent_data$City=="Delhi"&(house_rent_data$`Tenant_Wanted`=="Bachelors") &(house_rent_data$Area_Type=="Carpet Area"),]

bach_area_che=house_rent_data[house_rent_data$City=="Chennai"&(house_rent_data$`Tenant_Wanted`=="Bachelors")&(house_rent_data$Area_Type=="Carpet Area"),]


r1=max(bach_area_hyd$`Monthly_Rent_Price`)

r2=max(bach_area_kol$`Monthly_Rent_Price`)

r3=max(bach_area_mum$`Monthly_Rent_Price`)

r4=max(bach_area_ban$`Monthly_Rent_Price`)

r5=max(bach_area_del$`Monthly_Rent_Price`)

r6=max(bach_area_che$`Monthly_Rent_Price`)

city_name=c("Hyderabad","Kolkata","Mumbai","Chennai","Bangalore","Delhi")
number1=c(r1,r2,r3,r6,r4,r5)
data <- data.frame( City=city_name ,Houses=number1)

#Bar Graph
ggplot(data, aes(x=City, y=Houses)) + geom_bar(stat = "identity",width=0.5,color="red",fill="blue")+ylab("Rent in Rs")+
  geom_text(aes(label=Houses),position=position_dodge(width=0.9), vjust=-0.25,color="black")+ggtitle("Rent of Most Expensive unit for Bachelors with Carpet Type Area in Each city")+
  theme_bw()


############################# 5.2 Most Expensive Unit Rent for Bachelors Only Houses Based on Furnishing Type##########
################################## 5.2.1 Most Expensive Rent for Bachelors Only Houses For Furnished Houses ###########

bach_furn_hyd=house_rent_data[house_rent_data$City=="Hyderabad"&(house_rent_data$`Tenant_Wanted`=="Bachelors") &(house_rent_data$Furnishing=="Furnished"),]

bach_furn_kol=house_rent_data[house_rent_data$City=="Kolkata"&(house_rent_data$`Tenant_Wanted`=="Bachelors") &(house_rent_data$Furnishing=="Furnished"),]

bach_furn_mum=house_rent_data[house_rent_data$City=="Mumbai"&(house_rent_data$`Tenant_Wanted`=="Bachelors") &(house_rent_data$Furnishing=="Furnished"),]

bach_furn_ban=house_rent_data[house_rent_data$City=="Bangalore"&(house_rent_data$`Tenant_Wanted`=="Bachelors")&(house_rent_data$Furnishing=="Furnished"),]

bach_furn_del=house_rent_data[house_rent_data$City=="Delhi"&(house_rent_data$`Tenant_Wanted`=="Bachelors") &(house_rent_data$Furnishing=="Furnished"),]

bach_furn_che=house_rent_data[house_rent_data$City=="Chennai"&(house_rent_data$`Tenant_Wanted`=="Bachelors")&(house_rent_data$Furnishing=="Furnished"),]


r1=max(bach_furn_hyd$`Monthly_Rent_Price`)

r2=max(bach_furn_kol$`Monthly_Rent_Price`)

r3=max(bach_furn_mum$`Monthly_Rent_Price`)

r4=max(bach_furn_ban$`Monthly_Rent_Price`)

r5=max(bach_furn_del$`Monthly_Rent_Price`)

r6=max(bach_furn_che$`Monthly_Rent_Price`)

city_name=c("Hyderabad","Kolkata","Mumbai","Chennai","Bangalore","Delhi")
number1=c(floor(r1),floor(r2),floor(r3),floor(r6),floor(r4),floor(r5))
data <- data.frame( City=city_name ,Houses=number1)

pie(number1,radius=.5,main="Rent of the Most Expensive Bachelor Only Furnished House in Each City",
    col=c("green","blue","red","yellow","cyan","pink"),clockwise=TRUE,labels=paste0("Rs ",number1))
legend("topright", legend = c("Hyderabad", "Kolkata", "Mumbai","Chennai","Bangalore","Delhi"),
       fill =  c("green","blue","red","yellow","cyan","pink"))

################################## 5.2.2 Most Expensive Rent for Bachelors Only Houses For Semi-Furnished Houses #########

bach_furn_hyd=house_rent_data[house_rent_data$City=="Hyderabad"&(house_rent_data$`Tenant_Wanted`=="Bachelors") &(house_rent_data$Furnishing=="Semi-Furnished"),]

bach_furn_kol=house_rent_data[house_rent_data$City=="Kolkata"&(house_rent_data$`Tenant_Wanted`=="Bachelors") &(house_rent_data$Furnishing=="Semi-Furnished"),]

bach_furn_mum=house_rent_data[house_rent_data$City=="Mumbai"&(house_rent_data$`Tenant_Wanted`=="Bachelors") &(house_rent_data$Furnishing=="Semi-Furnished"),]

bach_furn_ban=house_rent_data[house_rent_data$City=="Bangalore"&(house_rent_data$`Tenant_Wanted`=="Bachelors")&(house_rent_data$Furnishing=="Semi-Furnished"),]

bach_furn_del=house_rent_data[house_rent_data$City=="Delhi"&(house_rent_data$`Tenant_Wanted`=="Bachelors") &(house_rent_data$Furnishing=="Semi-Furnished"),]

bach_furn_che=house_rent_data[house_rent_data$City=="Chennai"&(house_rent_data$`Tenant_Wanted`=="Bachelors")&(house_rent_data$Furnishing=="Semi-Furnished"),]


r1=max(bach_furn_hyd$`Monthly_Rent_Price`)

r2=max(bach_furn_kol$`Monthly_Rent_Price`)

r3=max(bach_furn_mum$`Monthly_Rent_Price`)

r4=max(bach_furn_ban$`Monthly_Rent_Price`)

r5=max(bach_furn_del$`Monthly_Rent_Price`)

r6=max(bach_furn_che$`Monthly_Rent_Price`)

city_name=c("Hyderabad","Kolkata","Mumbai","Chennai","Bangalore","Delhi")
number1=c(floor(r1),floor(r2),floor(r3),floor(r6),floor(r4),floor(r5))
data <- data.frame( City=city_name ,Houses=number1)

pie(number1,radius=0.8,main="Rent of the Most Expensive Bachelor Only Semi-Furnished House in Each City",
    col=c("green","blue","red","yellow","#EB2476","pink"),clockwise=TRUE,labels=paste0("Rs ",number1))
legend("topright", legend = c("Hyderabad", "Kolkata", "Mumbai","Chennai","Bangalore","Delhi"),
       fill =  c("green","blue","red","yellow","#EB2476","pink"))

############################# 5.3 No of Houses Available Above the Average Rent For Bachelor Houses #######

g1=house_rent_data[house_rent_data$City=="Hyderabad"&(house_rent_data$`Tenant_Wanted`=="Bachelors"),]

g2=house_rent_data[house_rent_data$City=="Kolkata"&(house_rent_data$`Tenant_Wanted`=="Bachelors"),]

g3=house_rent_data[house_rent_data$City=="Mumbai"&(house_rent_data$`Tenant_Wanted`=="Bachelors") ,]

g4=house_rent_data[house_rent_data$City=="Bangalore"&(house_rent_data$`Tenant_Wanted`=="Bachelors"),]

g5=house_rent_data[house_rent_data$City=="Delhi"&(house_rent_data$`Tenant_Wanted`=="Bachelors") ,]

g6=house_rent_data[house_rent_data$City=="Chennai"&(house_rent_data$`Tenant_Wanted`=="Bachelors"),]


r1=mean(g1$`Monthly_Rent_Price`)

r2=mean(g2$`Monthly_Rent_Price`)

r3=mean(g3$`Monthly_Rent_Price`)

r4=mean(g4$`Monthly_Rent_Price`)

r5=mean(g5$`Monthly_Rent_Price`)

r6=mean(g6$`Monthly_Rent_Price`)


g7=house_rent_data[house_rent_data$City=="Hyderabad"&(house_rent_data$`Tenant_Wanted`=="Bachelors")& (house_rent_data$Monthly_Rent_Price>r1),]

g8=house_rent_data[house_rent_data$City=="Kolkata"&(house_rent_data$`Tenant_Wanted`=="Bachelors") & (house_rent_data$Monthly_Rent_Price>r2),]

g9=house_rent_data[house_rent_data$City=="Mumbai"&(house_rent_data$`Tenant_Wanted`=="Bachelors") & (house_rent_data$Monthly_Rent_Price>r3) ,]

g10=house_rent_data[house_rent_data$City=="Bangalore"&(house_rent_data$`Tenant_Wanted`=="Bachelors") & (house_rent_data$Monthly_Rent_Price>r4),]

g11=house_rent_data[house_rent_data$City=="Delhi"&(house_rent_data$`Tenant_Wanted`=="Bachelors") & (house_rent_data$Monthly_Rent_Price>r5),]

g12=house_rent_data[house_rent_data$City=="Chennai"&(house_rent_data$`Tenant_Wanted`=="Bachelors") & (house_rent_data$Monthly_Rent_Price>r6),]

f1=nrow(g7)
f2=nrow(g8)
f3=nrow(g9)
f4=nrow(g10)
f5=nrow(g11)
f6=nrow(g12)


city_name=c("Hyderabad","Kolkata","Mumbai","Chennai","Bangalore","Delhi")
number1=c(f1,f2,f3,f6,f4,f5)
data <- data.frame( City=city_name ,Houses=number1)

#Bar Graph
ggplot(data, aes(x=City, y=Houses)) + geom_bar(stat = "identity",width=0.5,alpha=2,fill="#3E6151",color="black")+ylab("No of Houses")+
  geom_text(aes(label=Houses),position=position_dodge(width=0.9), vjust=-0.25)+ggtitle("Total No of Bachelor Only Houses above the average rent in each city")+
  theme_bw()



############################# 3.4 Cheapest House Among All Cities for Only Bachelor




############# Q6 Which city is the Most Expensive for Family Only Housing ################

############################# 5.1 Most Expensive Rent for Bachelors Only Houses Based on Area type
################################## 6.1.1 Most Expensive Rent for Family Only Houses Based on Super Type #######

bach_area_hyd=house_rent_data[house_rent_data$City=="Hyderabad"&(house_rent_data$`Tenant_Wanted`=="Family") &(house_rent_data$Area_Type=="Super Area"),]

bach_area_kol=house_rent_data[house_rent_data$City=="Kolkata"&(house_rent_data$`Tenant_Wanted`=="Family") &(house_rent_data$Area_Type=="Super Area"),]

bach_area_mum=house_rent_data[house_rent_data$City=="Mumbai"&(house_rent_data$`Tenant_Wanted`=="Family") &(house_rent_data$Area_Type=="Super Area"),]

bach_area_ban=house_rent_data[house_rent_data$City=="Bangalore"&(house_rent_data$`Tenant_Wanted`=="Family")&(house_rent_data$Area_Type=="Super Area"),]

bach_area_del=house_rent_data[house_rent_data$City=="Delhi"&(house_rent_data$`Tenant_Wanted`=="Family") &(house_rent_data$Area_Type=="Super Area"),]

bach_area_che=house_rent_data[house_rent_data$City=="Chennai"&(house_rent_data$`Tenant_Wanted`=="Family")&(house_rent_data$Area_Type=="Super Area"),]


r1=max(bach_area_hyd$`Monthly_Rent_Price`)

r2=max(bach_area_kol$`Monthly_Rent_Price`)

r3=max(bach_area_mum$`Monthly_Rent_Price`)

r4=max(bach_area_ban$`Monthly_Rent_Price`)

r5=max(bach_area_del$`Monthly_Rent_Price`)

r6=max(bach_area_che$`Monthly_Rent_Price`)

city_name=c("Hyderabad","Kolkata","Mumbai","Chennai","Bangalore","Delhi")
number1=c(r1,r2,r3,r6,r4,r5)
data <- data.frame( City=city_name ,Houses=number1)

#Bar Graph
ggplot(data, aes(x=City, y=Houses)) + geom_bar(stat = "identity",width=0.5,color="red",fill="blue")+ylab("Rent in Rs")+
  geom_text(aes(label=Houses),position=position_dodge(width=0.9), vjust=-0.25,color="red")+
  ggtitle("Comparison of Rent of Most Expensive unit for Family with Super Type Area in Each city")+
  theme_solarized()



################################## 6.1.2 Most Expensive Rent for Family Only Houses Based on Carpet type ########

bach_area_hyd=house_rent_data[house_rent_data$City=="Hyderabad"&(house_rent_data$`Tenant_Wanted`=="Family") &(house_rent_data$Area_Type=="Carpet Area"),]

bach_area_kol=house_rent_data[house_rent_data$City=="Kolkata"&(house_rent_data$`Tenant_Wanted`=="Family") &(house_rent_data$Area_Type=="Carpet Area"),]

bach_area_mum=house_rent_data[house_rent_data$City=="Mumbai"&(house_rent_data$`Tenant_Wanted`=="Family") &(house_rent_data$Area_Type=="Carpet Area"),]

bach_area_ban=house_rent_data[house_rent_data$City=="Bangalore"&(house_rent_data$`Tenant_Wanted`=="Family")&(house_rent_data$Area_Type=="Carpet Area"),]

bach_area_del=house_rent_data[house_rent_data$City=="Delhi"&(house_rent_data$`Tenant_Wanted`=="Family") &(house_rent_data$Area_Type=="Carpet Area"),]

bach_area_che=house_rent_data[house_rent_data$City=="Chennai"&(house_rent_data$`Tenant_Wanted`=="Family")&(house_rent_data$Area_Type=="Carpet Area"),]


r1=max(bach_area_hyd$`Monthly_Rent_Price`)

r2=max(bach_area_kol$`Monthly_Rent_Price`)

r3=max(bach_area_mum$`Monthly_Rent_Price`)

r4=max(bach_area_ban$`Monthly_Rent_Price`)

r5=max(bach_area_del$`Monthly_Rent_Price`)

r6=max(bach_area_che$`Monthly_Rent_Price`)

city_name=c("Hyderabad","Kolkata","Mumbai","Chennai","Bangalore","Delhi")
number1=c(r1,r2,r3,r6,r4,r5)
data <- data.frame( City=city_name ,Houses=number1)

#Bar Graph
ggplot(data, aes(x=City, y=Houses)) + geom_bar(stat = "identity",width=0.5,color="red",fill="#A2D524")+ylab("Rent in Rs")+
  geom_text(aes(label=Houses),position=position_dodge(width=0.9), vjust=-0.25,color="red")+
  ggtitle("Comparison of Rent of Most Expensive unit for Family with Carpet Type Area in Each city")+
  theme_economist()


############################# 6.2 Most Expensive Unit Rent for Family Only Houses Based on Furnishing Type##########
################################## 6.2.1 Most Expensive Rent for Family Only Houses For Furnished Houses ###########

bach_furn_hyd=house_rent_data[house_rent_data$City=="Hyderabad"&(house_rent_data$`Tenant_Wanted`=="Family") &(house_rent_data$Furnishing=="Furnished"),]

bach_furn_kol=house_rent_data[house_rent_data$City=="Kolkata"&(house_rent_data$`Tenant_Wanted`=="Family") &(house_rent_data$Furnishing=="Furnished"),]

bach_furn_mum=house_rent_data[house_rent_data$City=="Mumbai"&(house_rent_data$`Tenant_Wanted`=="Family") &(house_rent_data$Furnishing=="Furnished"),]

bach_furn_ban=house_rent_data[house_rent_data$City=="Bangalore"&(house_rent_data$`Tenant_Wanted`=="Family")&(house_rent_data$Furnishing=="Furnished"),]

bach_furn_del=house_rent_data[house_rent_data$City=="Delhi"&(house_rent_data$`Tenant_Wanted`=="Family") &(house_rent_data$Furnishing=="Furnished"),]

bach_furn_che=house_rent_data[house_rent_data$City=="Chennai"&(house_rent_data$`Tenant_Wanted`=="Family")&(house_rent_data$Furnishing=="Furnished"),]

r1=max(bach_furn_hyd$`Monthly_Rent_Price`)

r2=max(bach_furn_kol$`Monthly_Rent_Price`)

r3=max(bach_furn_mum$`Monthly_Rent_Price`)

r4=max(bach_furn_ban$`Monthly_Rent_Price`)

r5=max(bach_furn_del$`Monthly_Rent_Price`)

r6=max(bach_furn_che$`Monthly_Rent_Price`)

city_name=c("Hyderabad","Kolkata","Mumbai","Chennai","Bangalore","Delhi")
number1=c(floor(r1),floor(r2),floor(r3),floor(r6),floor(r4),floor(r5))
data <- data.frame( City=city_name ,Houses=number1)

pie(number1,radius=.5,main="Rent of the Most Expensive Bachelor Only Furnished House in Each City",
    col=c("green","blue","red","yellow","grey","pink"),clockwise=TRUE,labels=paste0("Rs ",number1))
legend("topright", legend = c("Hyderabad", "Kolkata", "Mumbai","Chennai","Bangalore","Delhi"),
       fill =  c("green","blue","red","yellow","grey","pink"))

################################## 6.2.2 Most Expensive Rent for Family Only Houses For Semi-Furnished Houses #########

bach_furn_hyd=house_rent_data[house_rent_data$City=="Hyderabad"&(house_rent_data$`Tenant_Wanted`=="Family") &(house_rent_data$Furnishing=="Semi-Furnished"),]

bach_furn_kol=house_rent_data[house_rent_data$City=="Kolkata"&(house_rent_data$`Tenant_Wanted`=="Family") &(house_rent_data$Furnishing=="Semi-Furnished"),]

bach_furn_mum=house_rent_data[house_rent_data$City=="Mumbai"&(house_rent_data$`Tenant_Wanted`=="Family") &(house_rent_data$Furnishing=="Semi-Furnished"),]

bach_furn_ban=house_rent_data[house_rent_data$City=="Bangalore"&(house_rent_data$`Tenant_Wanted`=="Family")&(house_rent_data$Furnishing=="Semi-Furnished"),]

bach_furn_del=house_rent_data[house_rent_data$City=="Delhi"&(house_rent_data$`Tenant_Wanted`=="Family") &(house_rent_data$Furnishing=="Semi-Furnished"),]

bach_furn_che=house_rent_data[house_rent_data$City=="Chennai"&(house_rent_data$`Tenant_Wanted`=="Family")&(house_rent_data$Furnishing=="Semi-Furnished"),]


r1=max(bach_furn_hyd$`Monthly_Rent_Price`)

r2=max(bach_furn_kol$`Monthly_Rent_Price`)

r3=max(bach_furn_mum$`Monthly_Rent_Price`)

r4=max(bach_furn_ban$`Monthly_Rent_Price`)

r5=max(bach_furn_del$`Monthly_Rent_Price`)

r6=max(bach_furn_che$`Monthly_Rent_Price`)

city_name=c("Hyderabad","Kolkata","Mumbai","Chennai","Bangalore","Delhi")
number1=c(floor(r1),floor(r2),floor(r3),floor(r6),floor(r4),floor(r5))
data <- data.frame( City=city_name ,Houses=number1)

pie(number1,radius=0.8,main="Rent of the Most Expensive Family Only Semi-Furnished House in Each City",
    col=c("green","blue","red","yellow","grey","pink"),clockwise=TRUE,labels=paste0("Rs ",number1))
legend("topright", legend = c("Hyderabad", "Kolkata", "Mumbai","Chennai","Bangalore","Delhi"),
       fill =  c("green","blue","red","yellow","grey","pink"))

############################# 6.3 No of Houses Available Above the Average Rent For Family Houses #######

g1=house_rent_data[house_rent_data$City=="Hyderabad"&(house_rent_data$`Tenant_Wanted`=="Family"),]

g2=house_rent_data[house_rent_data$City=="Kolkata"&(house_rent_data$`Tenant_Wanted`=="Family"),]

g3=house_rent_data[house_rent_data$City=="Mumbai"&(house_rent_data$`Tenant_Wanted`=="Family") ,]

g4=house_rent_data[house_rent_data$City=="Bangalore"&(house_rent_data$`Tenant_Wanted`=="Family"),]

g5=house_rent_data[house_rent_data$City=="Delhi"&(house_rent_data$`Tenant_Wanted`=="Family") ,]

g6=house_rent_data[house_rent_data$City=="Chennai"&(house_rent_data$`Tenant_Wanted`=="Family"),]


r1=mean(g1$`Monthly_Rent_Price`)

r2=mean(g2$`Monthly_Rent_Price`)

r3=mean(g3$`Monthly_Rent_Price`)

r4=mean(g4$`Monthly_Rent_Price`)

r5=mean(g5$`Monthly_Rent_Price`)

r6=mean(g6$`Monthly_Rent_Price`)


g7=house_rent_data[house_rent_data$City=="Hyderabad"&(house_rent_data$`Tenant_Wanted`=="Family")& (house_rent_data$Monthly_Rent_Price>r1),]

g8=house_rent_data[house_rent_data$City=="Kolkata"&(house_rent_data$`Tenant_Wanted`=="Family") & (house_rent_data$Monthly_Rent_Price>r2),]

g9=house_rent_data[house_rent_data$City=="Mumbai"&(house_rent_data$`Tenant_Wanted`=="Family") & (house_rent_data$Monthly_Rent_Price>r3) ,]

g10=house_rent_data[house_rent_data$City=="Bangalore"&(house_rent_data$`Tenant_Wanted`=="Family") & (house_rent_data$Monthly_Rent_Price>r4),]

g11=house_rent_data[house_rent_data$City=="Delhi"&(house_rent_data$`Tenant_Wanted`=="Family") & (house_rent_data$Monthly_Rent_Price>r5),]

g12=house_rent_data[house_rent_data$City=="Chennai"&(house_rent_data$`Tenant_Wanted`=="Family") & (house_rent_data$Monthly_Rent_Price>r6),]

f1=nrow(g7)
f2=nrow(g8)
f3=nrow(g9)
f4=nrow(g10)
f5=nrow(g11)
f6=nrow(g12)


city_name=c("Hyderabad","Kolkata","Mumbai","Chennai","Bangalore","Delhi")
number1=c(f1,f2,f3,f6,f4,f5)
data <- data.frame( City=city_name ,Houses=number1)

#Bar Graph
ggplot(data, aes(x=City, y=Houses)) + geom_bar(stat = "identity",width=0.5,alpha=2,fill="#44BDC2",color="black")+ylab("No of Houses")+
  geom_text(aes(label=Houses),position=position_dodge(width=0.9), vjust=-0.25)+ggtitle("Total No of Family Only Houses above the average rent in each city")+
  theme_clean()






############## Q7 Which city is better for Nuclear Families (with Less than 2 children ) ###########

############################ 7.1 Comparison of Family Houses with less than or equal to 2bedrooms and 2 washrooms with Furnishing preference in mind #####
house_rent_data%>%
  filter(Tenant_Wanted=="Family"|(Tenant_Wanted=="Bachelors/Family") &(house_rent_data$No_of_Rooms<=2) &(house_rent_data$No_of_Washrooms<=2) )%>%
  ggplot(aes(x=City,fill=Furnishing))+geom_bar(position="dodge",alpha=1)+ylab("No of Houses")+
  ggtitle("Family Houses with less than or equal to 2bedrooms and 2 washrooms vs Furnishing preference")+theme_clean()

############################ 7.2 Comparison of Average Area of Family houses less than 2bedroom and 2 washrooms #####

g1=house_rent_data[house_rent_data$City=="Hyderabad"&(house_rent_data$`Tenant_Wanted`=="Family") | (house_rent_data$`Tenant_Wanted`=="Bachelors/Family") &(house_rent_data$No_of_Rooms<=2) &(house_rent_data$No_of_Washrooms<=2),]

g2=house_rent_data[house_rent_data$City=="Kolkata"&(house_rent_data$`Tenant_Wanted`=="Family") | (house_rent_data$`Tenant_Wanted`=="Bachelors/Family")&(house_rent_data$No_of_Rooms<=2)&(house_rent_data$No_of_Washrooms<=2),]

g3=house_rent_data[house_rent_data$City=="Mumbai"&(house_rent_data$`Tenant_Wanted`=="Family") | (house_rent_data$`Tenant_Wanted`=="Bachelors/Family")&(house_rent_data$No_of_Rooms<=2) &(house_rent_data$No_of_Washrooms<=2),]

g4=house_rent_data[house_rent_data$City=="Bangalore"&(house_rent_data$`Tenant_Wanted`=="Family") | (house_rent_data$`Tenant_Wanted`=="Bachelors/Family")&(house_rent_data$No_of_Rooms<=2)&(house_rent_data$No_of_Washrooms<=2),]

g5=house_rent_data[house_rent_data$City=="Delhi"&(house_rent_data$`Tenant_Wanted`=="Family") | (house_rent_data$`Tenant_Wanted`=="Bachelors/Family") &(house_rent_data$No_of_Rooms<=2) &(house_rent_data$No_of_Washrooms<=2),]

g6=house_rent_data[house_rent_data$City=="Chennai"&(house_rent_data$`Tenant_Wanted`=="Family") | (house_rent_data$`Tenant_Wanted`=="Bachelors/Family")&(house_rent_data$No_of_Rooms<=2) &(house_rent_data$No_of_Washrooms<=2),]

r1= mean(g1$Size_of_house)

r2= mean(g2$Size_of_house)

r3= mean(g3$Size_of_house)

r4= mean(g4$Size_of_house)

r5= mean(g5$Size_of_house)

r6= mean(g6$Size_of_house)


city_name=c("Hyderabad","Kolkata","Mumbai","Chennai","Bangalore","Delhi")
number1=c(floor(r1),floor(r2),floor(r3),floor(r6),floor(r4),floor(r5))
data <- data.frame( City=city_name ,Houses=number1)

ggplot(data, aes(x=City, y=Houses)) + geom_bar(stat = "identity",width=0.5,color="red",fill="#EDB813")+ylab("Area in Sqft")+
  geom_text(aes(label=Houses),position=position_dodge(width=0.9), vjust=-0.25,color="red")+
  ggtitle(" Comparison of Average Area of Family houses less than 2bedroom and 2 washrooms")+theme_clean()


############################ 7.3 Comparison of Average rent of Family houses less than 2bedroom and 2 washrooms #####

g1=house_rent_data[house_rent_data$City=="Hyderabad"&(house_rent_data$`Tenant_Wanted`=="Family") | (house_rent_data$`Tenant_Wanted`=="Bachelors/Family")&(house_rent_data$No_of_Rooms<=2) &(house_rent_data$No_of_Washrooms<=2),]

g2=house_rent_data[house_rent_data$City=="Kolkata"&(house_rent_data$`Tenant_Wanted`=="Family") | (house_rent_data$`Tenant_Wanted`=="Bachelors/Family")&(house_rent_data$No_of_Rooms<=2) &(house_rent_data$No_of_Washrooms<=2),]

g3=house_rent_data[house_rent_data$City=="Mumbai"&(house_rent_data$`Tenant_Wanted`=="Family") | (house_rent_data$`Tenant_Wanted`=="Bachelors/Family")&(house_rent_data$No_of_Rooms<=2) &(house_rent_data$No_of_Washrooms<=2),]

g4=house_rent_data[house_rent_data$City=="Bangalore"&(house_rent_data$`Tenant_Wanted`=="Family") | (house_rent_data$`Tenant_Wanted`=="Bachelors/Family")&(house_rent_data$No_of_Rooms<=2) &(house_rent_data$No_of_Washrooms<=2),]

g5=house_rent_data[house_rent_data$City=="Delhi"&(house_rent_data$`Tenant_Wanted`=="Family") | (house_rent_data$`Tenant_Wanted`=="Bachelors/Family")&(house_rent_data$No_of_Rooms<=2) &(house_rent_data$No_of_Washrooms<=2),]

g6=house_rent_data[house_rent_data$City=="Chennai"&(house_rent_data$`Tenant_Wanted`=="Family") | (house_rent_data$`Tenant_Wanted`=="Bachelors/Family")&(house_rent_data$No_of_Rooms<=2) &(house_rent_data$No_of_Washrooms<=2),]

r1= mean(g1$Monthly_Rent_Price)

r2= mean(g2$Monthly_Rent_Price)

r3= mean(g3$Monthly_Rent_Price)

r4= mean(g4$Monthly_Rent_Price)

r5= mean(g5$Monthly_Rent_Price)

r6= mean(g6$Monthly_Rent_Price)


city_name=c("Hyderabad","Kolkata","Mumbai","Chennai","Bangalore","Delhi")
number1=c(floor(r1),floor(r2),floor(r3),floor(r6),floor(r4),floor(r5))
data <- data.frame( City=city_name ,Houses=number1)

ggplot(data, aes(x=City, y=Houses)) + geom_bar(stat = "identity",width=0.5,color="red",fill="light blue")+ylab("Rent in Rs")+
  geom_text(aes(label=Houses),position=position_dodge(width=0.9), vjust=-0.25)+
  ggtitle("Comparison of Average rent of houses with cities for Nuclear Families")+theme_wsj()



############## Q8 Which city is better for Extended Families (with more than 2 children and extra ) ###########


############################ 8.1 Comparison of Houses with more than or equal to 3bedrooms and 3 washrooms with Furnishing preference in mind #####
house_rent_data%>%
  filter(Tenant_Wanted=="Family"|(Tenant_Wanted=="Bachelors/Family") &(house_rent_data$No_of_Rooms>=3) &(house_rent_data$No_of_Washrooms>=3) )%>%
  ggplot(aes(x=City,fill=Furnishing))+geom_bar(position="dodge",alpha=1)+ylab("No of Houses")+
  ggtitle("No of Houses For Extended Families in Each City according to Furnishing preference")+theme_solarized_2()+scale_fill_manual(values=c("#9933FF","orange","#1FE90E"))

############################ 8.2 Comparison of Average Area of Family houses for Extended Families #####

g1=house_rent_data[house_rent_data$City=="Hyderabad"&(house_rent_data$`Tenant_Wanted`=="Family") | (house_rent_data$`Tenant_Wanted`=="Bachelors/Family") &(house_rent_data$No_of_Rooms>=3) &(house_rent_data$No_of_Washrooms>=3),]

g2=house_rent_data[house_rent_data$City=="Kolkata"&(house_rent_data$`Tenant_Wanted`=="Family") | (house_rent_data$`Tenant_Wanted`=="Bachelors/Family")&(house_rent_data$No_of_Rooms>=3) &(house_rent_data$No_of_Washrooms>=3),]

g3=house_rent_data[house_rent_data$City=="Mumbai"&(house_rent_data$`Tenant_Wanted`=="Family") | (house_rent_data$`Tenant_Wanted`=="Bachelors/Family")&(house_rent_data$No_of_Rooms>=3) &(house_rent_data$No_of_Washrooms>=3),]

g4=house_rent_data[house_rent_data$City=="Bangalore"&(house_rent_data$`Tenant_Wanted`=="Family") | (house_rent_data$`Tenant_Wanted`=="Bachelors/Family")&(house_rent_data$No_of_Rooms>=3) &(house_rent_data$No_of_Washrooms>=3),]

g5=house_rent_data[house_rent_data$City=="Delhi"&(house_rent_data$`Tenant_Wanted`=="Family") | (house_rent_data$`Tenant_Wanted`=="Bachelors/Family") &(house_rent_data$No_of_Rooms>=3) &(house_rent_data$No_of_Washrooms>=3),]

g6=house_rent_data[house_rent_data$City=="Chennai"&(house_rent_data$`Tenant_Wanted`=="Family") | (house_rent_data$`Tenant_Wanted`=="Bachelors/Family")&(house_rent_data$No_of_Rooms>=3) &(house_rent_data$No_of_Washrooms>=3),]


r1= mean(g1$Size_of_house)

r2= mean(g2$Size_of_house)

r3= mean(g3$Size_of_house)

r4= mean(g4$Size_of_house)

r5= mean(g5$Size_of_house)

r6= mean(g6$Size_of_house)


city_name=c("Hyderabad","Kolkata","Mumbai","Chennai","Bangalore","Delhi")
number1=c(floor(r1),floor(r2),floor(r3),floor(r6),floor(r4),floor(r5))
data <- data.frame( City=city_name ,Houses=number1)

ggplot(data, aes(x=City, y=Houses)) + geom_bar(stat = "identity",width=0.5,color="red",fill="#AEF404")+ylab("Area in Sqft")+geom_text(aes(label=Houses),position=position_dodge(width=0.9), vjust=-0.25)+
  ggtitle(" Comparison of Average Area of Family houses For Extended Families")+theme_economist_white()


############################ 8.3 Comparison of Average Rent of Family houses more than 3bedroom and 3 washrooms For Extended Families#####

g1=house_rent_data[house_rent_data$City=="Hyderabad"&(house_rent_data$`Tenant_Wanted`=="Family") | (house_rent_data$`Tenant_Wanted`=="Bachelors/Family") &(house_rent_data$No_of_Rooms<=2) &(house_rent_data$No_of_Washrooms<=2),]

g2=house_rent_data[house_rent_data$City=="Kolkata"&(house_rent_data$`Tenant_Wanted`=="Family") | (house_rent_data$`Tenant_Wanted`=="Bachelors/Family")&(house_rent_data$No_of_Rooms<=2) &(house_rent_data$No_of_Washrooms<=2),]

g3=house_rent_data[house_rent_data$City=="Mumbai"&(house_rent_data$`Tenant_Wanted`=="Family") | (house_rent_data$`Tenant_Wanted`=="Bachelors/Family")&(house_rent_data$No_of_Rooms<=2) &(house_rent_data$No_of_Washrooms<=2),]

g4=house_rent_data[house_rent_data$City=="Bangalore"&(house_rent_data$`Tenant_Wanted`=="Family") | (house_rent_data$`Tenant_Wanted`=="Bachelors/Family")&(house_rent_data$No_of_Rooms<=2) &(house_rent_data$No_of_Washrooms<=2),]

g5=house_rent_data[house_rent_data$City=="Delhi"&(house_rent_data$`Tenant_Wanted`=="Family") | (house_rent_data$`Tenant_Wanted`=="Bachelors/Family") &(house_rent_data$No_of_Rooms<=2) &(house_rent_data$No_of_Washrooms<=2),]

g6=house_rent_data[house_rent_data$City=="Chennai"&(house_rent_data$`Tenant_Wanted`=="Family") | (house_rent_data$`Tenant_Wanted`=="Bachelors/Family")&(house_rent_data$No_of_Rooms<=2) &(house_rent_data$No_of_Washrooms<=2),]

r1= mean(g1$Monthly_Rent_Price)

r2= mean(g2$Monthly_Rent_Price)

r3= mean(g3$Monthly_Rent_Price)

r4= mean(g4$Monthly_Rent_Price)

r5= mean(g5$Monthly_Rent_Price)

r6= mean(g6$Monthly_Rent_Price)


city_name=c("Hyderabad","Kolkata","Mumbai","Chennai","Bangalore","Delhi")
number1=c(floor(r1),floor(r2),floor(r3),floor(r6),floor(r4),floor(r5))
data <- data.frame( City=city_name ,Houses=number1)

ggplot(data, aes(x=City, y=Houses)) + geom_bar(stat = "identity",width=0.5,color="black",fill="#DAF7A6")+
  ylab("Rent in Rs")+geom_text(aes(label=Houses),position=position_dodge(width=0.9), vjust=-0.25)+
  ggtitle("Comparison of Average rent of Family houses For Extended Families")+theme_economist()





############## Q9 Relation of Person to Contact with Different houses #######
############################ 9.1 No of houses on sale by Person to Contacts with different tenant preferences#####
ggplot(house_rent_data,aes(x=Tenant_Wanted,fill=Person_to_Contact))+geom_bar(position="dodge",alpha=1)+
  ggtitle(" No of houses on sale by Person to Contacts with different Tenant Preferences")+xlab("Cities")+
  ylab("No of Houses")+theme_economist_white()+labs(fill="Person To Contact")


############################ 9.2 No of houses on sale by Person to Contacts with different furnishing preferences#####
ggplot(house_rent_data,aes(x=Furnishing,fill=Person_to_Contact))+geom_bar(position="dodge",alpha=1)+
  ggtitle("  No of houses on sale by Person to Contacts vs Furnishing preferencess")+xlab("Furnishing Preferences")+
  ylab("No of Houses")+theme_economist_white()+labs(fill="Person To Contact")+scale_fill_manual(values=c("#EB2474","green","#9BA0A0"))


############################ 9.3 Relation between Person to contacts with respect to Rent and Area of The Houses#####

house_rent_data%>%
  ggplot(aes(x=log(Size_of_house),y=log(Monthly_Rent_Price)))+geom_point(aes(shape=factor(Person_to_Contact),color=factor(Person_to_Contact)))+
  scale_color_manual(values = c("red", "blue","black"))+ggtitle("Relation between Size of the Houses vs Rent with Respect to cities")+
  scale_color_discrete(name = "Person To Contact")+labs(shape="Person To Contact")+ stat_smooth(method="lm",col="black",se=FALSE,size=1)+
  theme_clean()+xlab("Logirithmic value of Area of House in (Sqft)")+ylab("Logirithmic value of Monthly Rent of House in (Rs)")
  




############################ 9.4 Relation between Person to Contacts with respect to No of Houses for sale in each City#####

house_rent_data%>%
  ggplot(aes(x=Person_to_Contact,fill=City))+geom_bar()+ggtitle("Relation between Person to Contacts Vs No of houses for sale in each City")+
  theme_bw()+xlab("Types of Person To Contact")+ylab("No of Houses")+theme_hc()



############################ 9.5 Relation between Person to Contacts with respect to Area Type#####
house_rent_data%>%
  ggplot(aes(x=Area_Type,fill=Person_to_Contact))+geom_bar(position="dodge")+
  ggtitle("Relation between Person to Contacts vs No of houses with different Area Type for sale in each City")+  theme_bw()+
  xlab("Area Type Preference")+ylab("No of Houses")+labs(fill="Contractors")



############# Q10 Relation of Date Posted with Different houses
############## ############ Q10.1 Relation between Dated Posted vs Furnishing Status ######
data_new <- house_rent_data
data_new$Date_Posted <- data_new$Date_Posted
data_new$Date_Posted <-as.Date(c,"%m/%d/%y")
data_new <- data_new[order(data_new$Date_Posted),]

ggplot(data_new,aes(x=Date_Posted, fill=Furnishing))+geom_histogram(color="black")+
  scale_x_date(date_labels = "%m / %y")+theme_dark()+xlab("Month Posted")+labs(fill="Furnishing Status")+
  ggtitle("Relation between Month Posted and Furnishing Status")+ylab("No of Houses")

########################### Q10.2 Relation between Dated Posted and Tenant Wanted ######
data_new <- house_rent_data
data_new$Date_Posted <- data_new$Date_Posted
data_new$Date_Posted <-as.Date(c,"%m/%d/%y")
data_new <- data_new[order(data_new$Date_Posted),]

ggplot(data_new,aes(x=Date_Posted, fill=Tenant_Wanted))+
  geom_density(color="black",alpha=0.3)+
  scale_x_date(date_labels = "%m / %y")+
  theme_economist()+xlab("Month Posted")+
  labs(fill="Preferred Tenants")+
  ggtitle("Relation between Month Posted and Tenant Wanted")+theme_wsj()


########################### Q10.3 Relation between Dated Posted and Cities ######
data_new <- house_rent_data
data_new$Date_Posted <- data_new$Date_Posted
data_new$Date_Posted <-as.Date(c,"%m/%d/%y")
data_new <- data_new[order(data_new$Date_Posted),]

ggplot(data_new,aes(x=Date_Posted, fill=City))+geom_density(color="black",alpha=0.3)+
  scale_x_date(date_labels = "%m / %y")+theme_clean()+xlab("Month Posted")+labs(fill="Cities")+
  ggtitle("Relation between Month Posted and Cities")



