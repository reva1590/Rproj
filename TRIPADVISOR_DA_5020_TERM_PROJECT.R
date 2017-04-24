#### SETTING PATH ####
setwd("C:\\Users\\Reva Tiwari\\Desktop\\files")


#### INSTALLING PACKAGE ####
library(stringr)
library(RSQLite)
library(rvest)
library(plyr)


#### LISTING ALL THE FILES HAVING .DAT FORMAT ####
filelist1 = list.files(pattern = ".*.dat")


#### READING THE DATA AND CREATING DATAFRAME ####
q <- as.numeric()
for( j in 1:length(filelist1))
{
  myfile <- readLines(filelist1[j])
  p <- grep("<URL>",myfile)
  if(length(p)==0){
    q <- append(q,j)
  } 
}
filelist <- filelist1[-q]

main_frame <- data.frame(Hotel=character(),City=character(),State=character(),OverallRating=numeric(),Overall=numeric(),Cleanliness=numeric(),Rooms=numeric(),
                         Value=numeric(),Service=numeric(),Location=numeric(),Cf=numeric(),Bs=numeric(),
                         Price=character(),stringsAsFactors = FALSE)


#### RUNNING A LOOP FOR ALL THE PARAMETERS OF THE DATASET AND CALCULATING MEAN FOR MULTIPLE PARAMETERS ####
for(i in 1 : length(filelist))
{
  myfile <- readLines(filelist[i])                                #reading all the files line by line
  overall_rating <- myfile[grep("<Overall Rating>", myfile)]      #matching tags <Overall Rating>
  count_overall_no <- as.numeric(gsub(".*>", "", overall_rating)) #counting tag <Overall Rating>
  
  overall <- myfile[grep("<Overall>", myfile)]                    #matching tags <Overall>
  count_overall <- as.numeric(gsub(".*>", "", overall))           #counting tag <Overall>
  overall_no <- round(mean(count_overall),digits = 0)
  
  cleanliness <- myfile[grep("<Cleanliness>", myfile)]            #matching tags <Cleanliness>
  count_cleanliness <- gsub("-1", count_overall_no, cleanliness)  #replacing the -1  rating values by the overall rating from the  count_overall_rating_df
  cleanliness_all <- as.numeric(gsub(".*>", "", count_cleanliness))#counting tag <Overall>
  cleanliness_no <- round(mean(cleanliness_all),digits = 0) 
  
  rooms <- myfile[grep("<Rooms>", myfile)]                        #matching tags <Rooms>
  count_rooms <- gsub("-1", count_overall_no, rooms)              #replacing the -1  rating values by the overall rating from the  count_overall_rating_df
  rooms_all <- as.numeric(gsub(".*>", "", count_rooms))           #counting tag <Rooms>
  rooms_no <- round(mean(rooms_all),digits = 0)                   #putting count in the data-frame rooms_df
  
  value <- myfile[grep("<Value>", myfile)]                        #matching tags <Value>
  count_value <- gsub("-1", count_overall_no, value)              #replacing the -1  rating values by the overall rating from the  count_overall_rating_df
  value_all <- as.numeric(gsub(".*>", "", count_value))           #counting tag <Value>
  value_no <- round(mean(value_all),digits = 0)                   #putting count in the data-frame value_df
  
  service <- myfile[grep("<Service>", myfile)]                    #matching tags <Service>
  count_service <- gsub("-1", count_overall_no, service)          #replacing the -1  rating values by the overall rating from the  count_overall_rating_df
  service_all <- as.numeric(gsub(".*>", "", count_service))       #counting tag <Service>
  service_no <- round(mean(service_all),digits = 0)               #putting count in the data-frame service_df
  
  location <- myfile[grep("<Location>", myfile)]                  #matching tags <Location>
  count_location <- gsub("-1", count_overall_no, location)        #replacing the -1  rating values by the overall rating from the  count_overall_rating_df
  location_all <- as.numeric(gsub(".*>", "", count_location))     #counting tag <Location>
  location_no <- round(mean(location_all),digits = 0)             #putting count in the data-frame location_df
  
  cf <- myfile[grep("<Check in / front desk>", myfile)]           #matching tags <Check in / front desk>
  count_cf <- gsub("-1", count_overall_no, cf)                    #replacing the -1  rating values by the overall rating from the  count_overall_rating_df
  cf_all <- as.numeric(gsub(".*>", "", count_cf))                 #counting tag <Check in / front desk>                   
  cf_no <- round(mean(cf_all),digits = 0)                         #putting count in the data-frame cf_df
  
  bs <- myfile[grep("<Business service>", myfile)]                #matching tags <Business service>
  count_bs <- gsub("-1", count_overall_no, bs)                    #replacing the -1  rating values by the overall rating from the  count_overall_rating_df
  bs_all <- as.numeric(gsub(".*>", "", count_bs))                 #counting tag <Business service>   
  bs_no <- round(mean(bs_all),digits = 0)                         #putting count in the data-frame bs_df
  
  price <- myfile[grep("<Avg. Price>", myfile)]                   #matching tags <Overall>
  count_price <-gsub(".*>", "", price)                            #counting tag <Overall>
  price_no <- count_price                                         #putting count in the data-frame overall_df
 
#### EXTRACTING THE CITY AND STATE NAME #### 
  y <- grep("<URL>",myfile)                                       
  z <- gsub(".html","",myfile[y])
  a1 <- data.frame(str_split_fixed(z,"\\d{8}-",2),stringsAsFactors = FALSE)
  colnames(a1) <- c("A","B")
  a2 <- data.frame(str_split_fixed(a1$B,"-",2),stringsAsFactors = FALSE)
  colnames(a2) <- c("HotelName","CityState")
  Hotel <- gsub("_"," ",a2$HotelName)
  str_match(a2$CityState, "(.*?)_([^_]+)$")
  city <- str_match(a2$CityState, "(.*?)_([^_]+)$")[, 2]
  state <- str_match(a2$CityState, "(.*?)_([^_]+)$")[, 3]
  a3 <- data.frame(city,state,stringsAsFactors = FALSE)
  colnames(a3) <- c("City","State")
  #a3$City
  #a3$State
  
  main_frame <- rbind(main_frame,data.frame(Hotel,a3$City,a3$State,count_overall_no,overall_no,cleanliness_no,
                                            rooms_no,value_no,service_no,location_no,cf_no,bs_no,price_no,
                                            stringsAsFactors = FALSE))
}

colnames(main_frame) <- c("Hotel","City","State","Overall_Count","Overall","Cleanliness","Rooms","Value",
                          "Service","Location","CF","BS","Price")

#### REPLACING THE EMPTY CELLS WITH NA AND THEN REMOVING NA FROM THE DATAFRAME ####
main_frame[main_frame==""]<-NA
main_frame<-na.omit(main_frame)

#### EXTRACTING NEWYORK SEPARATELY AS THIS IS A SPECIAL CASE IN THE FILE ####
main_frame$City <-gsub("_"," ",main_frame$City)
main_frame$City[main_frame$State == 'York'] <- 'New York City'
main_frame$State[main_frame$State == 'York'] <- 'New York'


#### REPLACING UNKNOWN IN PRICE COLUMN WITH NA ####

main<-main_frame$Price[main_frame$Price=="Unkonwn"]<-NA
#main<-na.omit(main)

#main_frame$Price[98]
##main_frame$Price[main_frame$Price=="Unknown"]<-NA

################################### DB Connection code####################################

#### FORMING A CONNECTION WITH THE DATABASE ####

db <- dbConnect(SQLite(), dbname="Tripadvisor.sqlite")


##### Queries for creating tables ####
dbRemoveTable(db, "city")
dbRemoveTable(db, "state")
dbRemoveTable(db, "HotelRating")


dbSendQuery(db,  "CREATE TABLE state (
            state_ID INTEGER PRIMARY KEY,
            stateName TEXT)")

dbSendQuery(db,  "CREATE TABLE city (
            city_ID INTEGER PRIMARY KEY,
            cityName TEXT)")

dbSendQuery(db,  "CREATE TABLE HotelRating (
            HotelID INTEGER PRIMARY KEY,
            HotelName Text,
             city_ID Integer,
            state_ID Integer,
            cleanliness integer,
            rooms_rating integer,
            value_rating integer,
            service_rating integer,
            location_rating integer,
            CF_rating integer,
            BS_rating integer,
            price double,
            overall_rating integer,
            Foreign key(city_ID) references city(city_ID),
            Foreign key(state_ID) references state(state_ID)
          )")



#### CREATING CITYDATA FROM Tripadvisor.sqlite CONTAINING DATA FOR CITYDATA ####
cityData <- unique(cbind.data.frame(main_frame$City))
cityData <-(cbind.data.frame(1:nrow(cityData),cityData))
colnames(cityData) <- c("city_ID","cityName")
dbWriteTable(conn = db, name = "city",value = cityData,append = TRUE)


#### CREATING STATEDATA FROM Tripadvisor.sqlite CONTAINING DATA FOR STATEDATA ####
stateData <- unique(cbind.data.frame(main_frame$State))
stateData <-(cbind.data.frame(1:nrow(stateData),stateData))
colnames(stateData) <- c("state_ID","stateName")
dbWriteTable(conn = db, name = "state",value = stateData,append = TRUE)


# MERGING THE CITYDATA AND STATEDATA 
merge.data <- merge(main_frame,cityData,by.x =c("City"),by.y = c("cityName"))
merge.data <- merge(merge.data,stateData,by.x =c("State"),by.y = c("stateName"))
rm('ratingData')
ratingData <- unique(cbind.data.frame(merge.data$Hotel,
                                      merge.data$city_ID,
                                      merge.data$state_ID,
                                      merge.data$Cleanliness,
                                      merge.data$Rooms,
                                      merge.data$Value,
                                      merge.data$Service,
                                      merge.data$Location,
                                      merge.data$CF,
                                      merge.data$BS,
                                      merge.data$Price,
                                      merge.data$Overall))
ratingData <-(cbind.data.frame(1:nrow(ratingData),ratingData))
colnames(ratingData) <- c('HotelID' , 'HotelName' , 'city_ID' ,'state_ID' ,'cleanliness','rooms_rating' ,'value_rating',
                          'service_rating','location_rating' , 'CF_rating', 
                          'BS_rating', 'price' ,'overall_rating')
dbWriteTable(conn = db, name = "HotelRating",value = ratingData,append = TRUE)
dbReadTable(db,'HotelRating')



#### QUERY TO RETREIVE THE HOTEL NAME AND ITS PRICE FOR WHICH THE RATING IS FOUR OR GREATER THAN FOUR #### 
rating_higher <- dbGetQuery(db,"select HotelName,price,overall_rating from HotelRating where overall_rating >='4'");
rating_higher


#### QUERY TO RETREIVE THE HOTEL NAME AND ITS PRICE FOR WHICH THE CLEANLINESS RATING IS FOUR OR GREATER THAN FOUR #### 
cleanliness_higher <- dbGetQuery(db,"select HotelName,price,cleanliness from HotelRating where cleanliness >='4'");
cleanliness_higher


#### QUERY TO RETREIVE THE HOTEL NAME AND ITS PRICE FOR WHICH THE ROOMS RATING IS FOUR OR GREATER THAN FOUR #### 
rooms_higher <- dbGetQuery(db,"select HotelName,price,rooms_rating from HotelRating where rooms_rating >='4'");
rooms_higher

#### QUERY TO RETREIVE THE HOTEL NAME AND ITS PRICE FOR WHICH THE VALUE RATING IS FOUR OR GREATER THAN FOUR ####
value_higher <- dbGetQuery(db,"select HotelName,price,value_rating from HotelRating where value_rating >='4'");
value_higher


#### QUERY TO RETREIVE THE HOTEL NAME AND ITS PRICE FOR WHICH THE SERVICE RATING IS FOUR OR GREATER THAN FOUR ####
Service_higher <- dbGetQuery(db,"select HotelName,price,service_rating from HotelRating where service_rating >='4'");
Service_higher

#### QUERY TO RETREIVE THE HOTEL NAME AND ITS PRICE FOR WHICH THE LOCATION RATING IS FOUR OR GREATER THAN FOUR ####
location_higher <- dbGetQuery(db,"select HotelName,price,location_rating from HotelRating where location_rating >='4'");
location_higher

#### QUERY TO RETREIVE THE HOTEL NAME AND ITS PRICE FOR WHICH THE CF RATING IS FOUR OR GREATER THAN FOUR ####
cf_higher <- dbGetQuery(db,"select HotelName,price,cf_rating from HotelRating where cf_rating >='4'");
cf_higher

#### QUERY TO RETREIVE THE HOTEL NAME AND ITS PRICE FOR WHICH THE CF RATING IS FOUR OR GREATER THAN FOUR ####
bs_higher <- dbGetQuery(db,"select HotelName,price,bs_rating from HotelRating where bs_rating >='4'");
bs_higher



################################# ANALYSIS ######################################



#### FINDING THE CORRELATION OF OVERALL_COUNT WITH THE REST OF THE PARAMETERS ####

cleanlinesscorr<-cor.test(main_frame$Overall_Count,main_frame$Cleanliness)
Roomscorr<-cor.test(main_frame$Overall_Count,main_frame$Rooms)
Valuecorr<-cor.test(main_frame$Overall_Count,main_frame$Value)
Servicecorr<-cor.test(main_frame$Overall_Count,main_frame$Service)
Locationcorr<-cor.test(main_frame$Overall_Count,main_frame$Location)
CFcorr<-cor.test(main_frame$Overall_Count,main_frame$CF)
BScorr<-cor.test(main_frame$Overall_Count,main_frame$BS)



##################################    SCRAPING    #################################


hotel <- read_html("http://www.travelandleisure.com/worlds-best/hotels-top-100-overall#inn-above-tide-sausalito-california")

#### SCRAPING THE SCORE OF THE BEST 100 HOTEL IN DATAFRAME ####
score <- (hotel %>%
          html_nodes("p:nth-child(1)") %>%      
          html_text())
score_df <- data.frame(score)

#### STORING THE CLEANED SCORE DATAFRAME ####
clean_score_df <- data.frame(score_df[3:102,], stringsAsFactors=FALSE)


#### SCRAPING THE HOTEL NAME OF THE BEST 100 HOTEL IN DATABASE ####
hotel <- (hotel %>%
          html_nodes(".slide-meta__title") %>%      
          html_text())
 
#### REMOVING THE BLANK SPACE AND \N FROM THE DATAFRAME ####
content <- gsub(pattern = "([\t\n\r])",replacement = "", x =hotel , ignore.case = TRUE) 


#### TRIMMING SPACES FROM LEFT,RIGHT ####
names<- trimws(content, which = c("both", "left", "right"))


hotel_list <- gsub("(?<=[\\s])\\s*|^\\s+$","", names, perl=TRUE)
hotel_list <- hotel_list[hotel_list != ""]


#### EXTRACTING THE UNQIUE 100 HOTELS FROM THE HOTELLIST ####
hoteldf <- data.frame(hotel_list)
unique_df <- unique(hoteldf)


#### CREATING A DIFFERENT DATAFRAME FOR SCORE, RANK AND NAME AND COMBINING THEM TO A DATAFRAME
rank<- data.frame(rank = numeric())
name_1<- data.frame(name= character(),stringsAsFactors = F)


#### RUNNING A LOOP TO REMOVE THE ADDITIONAL VALUES GENERATED WHILE SCRAPING THE DATA ####
for(i in 1:nrow(unique_df))
{
  pos<- gregexpr(pattern='. ',unique_df[i,1])
  name_1[i,]<- substring(as.character(unique_df[i,1]),(pos[[1]][1]+2),nchar(as.character(unique_df[i,1])))
  pos<- gregexpr(pattern='. ',name_1[i,])
  rank[i,]<- substring(as.character(name_1[i,]),1,(pos[[1]][1])-1)
  name_1[i,]<- substring(as.character(name_1[i,]),(pos[[1]][1]+2),nchar(as.character(name_1[i,])))
}
new_df <- data.frame(rank, name_1, clean_score_df)
colnames(new_df) <- c("rank","Hotel_Name","Score")


#### RANKING OF TOP 100 HOTELS AND THE SCORES ####
scraped_df <- new_df[order(nrow(new_df):1),]


