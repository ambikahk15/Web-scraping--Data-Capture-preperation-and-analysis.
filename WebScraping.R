#****************************************************************************
# Data Capture and Preparations-Assignment2
# Team: The Data Cranes   
# Members: Ambika Kapanaiah
#          Yingqi Chen
#          Anu Maria George  
#          Priyank Sharma
#****************************************************************************


##############------clean all global variables------############################

rm(list = ls())
#clean Console
cat("\014") 

##############-------Loading all the required library------#####################

if(!("tidyverse" %in% rownames(installed.packages()))){
  install.packages("tidyverse")
}
library(tidyverse)
if(!("stringr" %in% rownames(installed.packages()))){
  install.packages("stringr")
}
library(stringr)
if(!("rvest" %in% rownames(installed.packages()))){
  install.packages("rvest")
}
library(rvest)


################----------------Part1--------------############################

###---1. Retrieve and load all the data from the url into R.
WH_url <- "https://en.wikipedia.org/wiki/List_of_World_Heritage_in_Danger"

###---2. Obtain the table legend (Figure 1) and store all its elements. 

#You can use any data structure to store the data.
WH_scraped_html <- read_html(WH_url)

#As the "Table Legend" is under <dl> tag in html code, extracting the same.
WH_text_all <- WH_scraped_html %>%
  html_nodes ("dl")

#Extracting text alone here
Table_legend <- html_text(WH_text_all[2])
WHTable_legend <- unlist(strsplit(Table_legend,"\n"))
WHTable_legend <- data.frame(WHTable_legend)

###---3. Scrape the endangered list, which contains the current listed sites. 
#You can use any data structure to store the table.

#As there are 2 tables scraping the first table along which is under
#currently listed sites
WHtbl_endangerd <- WH_scraped_html %>%
  html_nodes("table") %>%
  .[2] %>%
  html_table(fill = TRUE)

WHtbl_endangerd <- as.data.frame(WHtbl_endangerd)
WHtbl_endangerd

###---4. Scrape all available hyperlinks in the url.

#reading all the url from the wikipage provided by 
#matching <a href tag from html
html <- paste0(readLines(WH_url), collapse="\n", warn=F)
#ignore the warning, as the end of line is the criteria to match and collapse.
matched <- str_match_all(html, "<a href=\"(.*?)\"")
matched <- as.data.frame(matched)
All_hyperlinks <- str_replace(matched$X1, "<a href=\"", "https://en.wikipedia.org")
AllWH_hyperlinks <- str_replace(All_hyperlinks, '[$\"]', '')
AllWH_hyperlinks

###---5. Using computational methods, obtain the hyperlink that you 
#can click on to jump to a new page that contains the selection 
#criteria used to classify a site as cultural or natural heritage.

#Extracting just the hyperlink which leads to Criteria with 
#both Cultural and Natural.
url_cltrl_ntrl <- AllWH_hyperlinks[grep("Selection_criteria", AllWH_hyperlinks)]

#As there is copy of url because of the previously listed site area
#considering the first one
url_cltrl_ntrl <- url_cltrl_ntrl[1]
url_cltrl_ntrl


###---6. Use the hyperlink obtained in the previous step and scrape the
#two lists (cultural and natural) and store them in two separated data
#structures within a list

#scraping everything from cultural and natural hyperlink
CulNat_scraped_html <- read_html(url_cltrl_ntrl)

#extracting the table Cultural and Natural which comes under <ol> tag of html 
CulNat_text_all <- CulNat_scraped_html %>%
  html_nodes ("ol")

#segregating Culural and Natural elements.
cultural <- html_text(CulNat_text_all[1])
Natural <- html_text(CulNat_text_all[2])
cultural
Natural

cultural_heritage <- unlist(strsplit(cultural,"\n"))
cultural_heritage <- str_replace_all(cultural_heritage, "\"","")
cultural_heritage
cultural_heritage <- data.frame(cultural_heritage)
class(cultural_heritage)
cultural_heritage

Natural_heritage <- unlist(strsplit(Natural,"\n"))
Natural_heritage <- str_replace_all(Natural_heritage, "\"","")
Natural_heritage <- data.frame(Natural_heritage)
Natural_heritage
class(Natural_heritage)
Natural_heritage

##################----------------Part2--------------###########################

###---1. From the table containing the endangered sites, remove the
#undesired variables (Image and Refs)

#dropping of Image and Refs columns
drop <- c("Image","Refs")
WHtbl_endangerd = WHtbl_endangerd[,!(names(WHtbl_endangerd) %in% drop)]
colnames(WHtbl_endangerd)

###---2.Then, obtain the country from the "Location" variable. 
#Using computational methods (e.g., Regex) fix any inconsistencies
#in the variable and then to extract the country only. 

for(r in 1:nrow(WHtbl_endangerd)){
  WHtbl_endangerd$Location[r] <- gsub("[0-9]+°.*","\\1",WHtbl_endangerd$Location[r])
  WHtbl_endangerd$Location[r] <- gsub("[\\.].*", "", WHtbl_endangerd$Location[r])
  WHtbl_endangerd$Location[r] <- gsub("\n","",WHtbl_endangerd$Location[r])
  WHtbl_endangerd$Location[r] <- gsub("[[$].*","",WHtbl_endangerd$Location[r])
  WHtbl_endangerd$Location[r] <- gsub("[($].*","",WHtbl_endangerd$Location[r])
  WHtbl_endangerd$Location[r] <- gsub(".*,\\s", "", WHtbl_endangerd$Location[r])
  WHtbl_endangerd$Location[r] <- gsub("\\,", "", WHtbl_endangerd$Location[r])
  WHtbl_endangerd$Location[r] <- gsub(" $", "", WHtbl_endangerd$Location[r])
  WHtbl_endangerd$Location[r] <- gsub(".*[\\*]+\\s","",WHtbl_endangerd$Location[r])
  WHtbl_endangerd$Location[r] <- gsub("\\*", "", WHtbl_endangerd$Location[r])
  }
WHtbl_endangerd$Location <- str_trim(WHtbl_endangerd$Location,"l")

WHtbl_endangerd$Location


###---3.Using computational methods (Regex), split the variable 
#that contains the criteria ("Criteria") into two variables: 
#"Type" (cultural/natural) and "Criteria" (containing roman numbers).

#As the 2 variables seperated by ":"
WHtbl_endangerd$Criteria <- str_split_fixed(WHtbl_endangerd$Criteria,":",2)

Type <- list()
for(rows in 1:nrow(WHtbl_endangerd)){
  Type[rows] <- WHtbl_endangerd$Criteria[rows,1]
}
Criteria <- list()
for(rows in 1:nrow(WHtbl_endangerd)){
  Criteria[rows] <- WHtbl_endangerd$Criteria[rows,2]
}

WHtbl_endangerd$Type <- unlist(Type)
WHtbl_endangerd$Criteria <- unlist(Criteria)
colnames(WHtbl_endangerd)
WHtbl_endangerd <- WHtbl_endangerd %>% relocate(Type, .before = Criteria)
colnames(WHtbl_endangerd)


###---4.Then, maintain only the data in acres and remove the
#hectares (ha) from the "Area" variable. Remove any extra 
#characters from the numbers.

#Renaming the "Araha..acre" colun name to "Area_Acres".
colnames(WHtbl_endangerd)[5] <- "Area_Acres"

for(r in 1:nrow(WHtbl_endangerd)){
  WHtbl_endangerd$Area_Acres[r] <- gsub("-","(NA)",
                                        WHtbl_endangerd$Area_Acres[r])
  WHtbl_endangerd$Area_Acres[r] <- gsub(
    "[\\(\\)]","", regmatches(WHtbl_endangerd$Area_Acres[r],
               gregexpr("\\(.*?\\)",                                                           WHtbl_endangerd$Area_Acres[r])))
  WHtbl_endangerd$Area_Acres[r] <- gsub(",","",WHtbl_endangerd$Area_Acres[r])}

WHtbl_endangerd$Area_Acres <-  as.double(WHtbl_endangerd$Area_Acres)

#NA's were introduced and Keeping the NA's as it is for the columns 
#with no Acres mentioned
WHtbl_endangerd$Area_Acres

###---5. Using computational methods (Regex), clean the variable
#Endangered and maintain only the very last year.
#Remove any unwanted characters.
for(r in 1:nrow(WHtbl_endangerd)){
  WHtbl_endangerd$Endangered[r] <- gsub('-', '', WHtbl_endangerd$Endangered[r])
  WHtbl_endangerd$Endangered[r] <- gsub(".*, ","", WHtbl_endangerd$Endangered[r])
  WHtbl_endangerd$Endangered[r] <- gsub("[^[:alnum:]]","", WHtbl_endangerd$Endangered[r])
}

typeof(WHtbl_endangerd$Endangered)

WHtbl_endangerd$Endangered <-  as.numeric(WHtbl_endangerd$Endangered)

###--6. Make sure that you have numeric vectors and characters vectors only.
summary(WHtbl_endangerd)
class(WHtbl_endangerd$Endangered)
class(WHtbl_endangerd$Year..WHS.)
class(WHtbl_endangerd$Area_Acres)
#All columns are verified . They are either character or integer Type.

#Renaming column name  "Year..WHS." to "Year_WHS"
colnames(WHtbl_endangerd)[6] <- "Year_WHS"
colnames(WHtbl_endangerd)

########################------------Part3-------------------####################


###---1.What type of site (cultural or natural) is the most
#common in the endangered list and how many does each type of site have?

site_type <- count(WHtbl_endangerd, Type)
if(site_type[1,2]>site_type[2,2]){
  print(paste0("The type Cultural is the most common in the endangered list,and is=", site_type[1,2]))
}else{
  print(paste0("The type Natural is the most common in the endangered list, and is=", site_type[2,2]))
}

print(cat("The type Cultural=",site_type[1,2],'\n',"The type Natural=",
          site_type[2,2],'\n'))

###---2. What site has the largest area (in m2) 
#and what site has the smallest area (in m2)? 

max_area=sapply(WHtbl_endangerd["Area_Acres"], max, na.rm = TRUE)
min_area=sapply(WHtbl_endangerd["Area_Acres"], min, na.rm = TRUE)

# 1Acre = 4046.86 sqmeter(m2). storing it in a constant variable.
Sqmeter <- 4046.86

#extracting max Acre row index converting to square meter(m2) and printing
row_numb <- which.max(WHtbl_endangerd$Area_Acres)

Ares_m2 <- WHtbl_endangerd[row_numb,"Area_Acres"]*Sqmeter
print(paste0("The site which has the largest area is ",
             WHtbl_endangerd[row_numb,"Name"],
             ", ",WHtbl_endangerd[row_numb,"Type"],
             ", from the Country ",WHtbl_endangerd[row_numb,"Location"],
             " and is = ",Ares_m2,"m2"))

#extracting min Acre row index converting to square meter(m2) and printing.
row_numb <- which.min(WHtbl_endangerd$Area_Acres)
Ares_m2 <- WHtbl_endangerd[row_numb,"Area_Acres"]*Sqmeter
print(paste0("The site which has the smallest area is ",
             WHtbl_endangerd[row_numb,"Name"],
             ", ",WHtbl_endangerd[row_numb,"Type"],
             ", from the Country ",WHtbl_endangerd[row_numb,"Location"],
             " and is = ",min_area*Sqmeter,"m2"))

###---3. What is the frequency (in years) with which sites were put on the endangered list?
#For example, how many were put on the list between 2010 and 2015? Use a plot to
#answer this question (e.g. histogram), remember to label and title you plot correctly. 

#sub-setting and grouping by Year of column endangered and 
#get the count of number of sites put in endangered list in each year.
Endangered_Yearwise <- as.data.frame(WHtbl_endangerd %>% 
                                   group_by(Endangered) %>% 
                                   summarise(Count = n()))

Endangered_Yearwise

library(ggplot2)
ggplot(Endangered_Yearwise, aes(x = Endangered, fill= ..count..)) + 
  geom_histogram(binwidth = 5,color="yellow") + 
  scale_x_continuous(name = "Year from(1980-2020)", labels = scales::comma) +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 12, hjust = 0.5),
        axis.title.y = element_text(size = 12, hjust = 0.5),
        legend.position ="right") +
  ylab("Number of sites put on the endangered list") + 
  ggtitle("Frequency chart of sites in endangered list from (1980 to 2020)") +
  theme(plot.title=element_text(hjust=0.5)) +
  scale_fill_gradient(low="navy", high="pink")

###---4. What country has more sites in the list? 
#and how many sites has each country in the list?

#grouping by Location which has country names for each site and sub-setting
#counting the number of sites for each country.
Countrycount <- as.data.frame(WHtbl_endangerd %>% 
                                   group_by(Location) %>% 
                                   summarise(Count = n()))

print(paste0("The country which has maximum number of sites in the list is ",
             Countrycount[which.max(Countrycount$Count),1],
             " And the number of sites listed in the list are=",
             max(Countrycount$Count)))

#this variable has the number of sites each country has
#We could also visualise by using pie chart
Countrycount
pie(Countrycount$Count,
    labels = Countrycount$Location,
    col = topo.colors(6),
    lty = 2)


###---5. How long took each site to be in the endangered list?

#getting the difference between year_WHS and Endangered List Year
YrstoReach_endangeredList <- WHtbl_endangerd$Endangered-WHtbl_endangerd$Year_WHS

#adding the column to original data frame WHtbl_endangerd as numeric column
WHtbl_endangerd$YrstoReach_endangeredList <- as.numeric(YrstoReach_endangeredList)

colnames(WHtbl_endangerd)

###---6. What is the frequency with which sites were put on the endangered list after they
#were inscribed in the World Heritage List? For instance, how many sites were in the
#endangered list after 3 years in the World Heritage list. Use a plot to answer this
#question.

#subsetting and grouping by the year of difference each site moved to 
#endangered list from WHS list and getting the count of sites
freqncy_siteendangered <- as.data.frame(WHtbl_endangerd %>% 
                                       group_by(YrstoReach_endangeredList) %>% 
                                       summarise(Count = n()))
freqncy_siteendangered

ggplot(freqncy_siteendangered, aes(x = YrstoReach_endangeredList,
                                   y = Count)) +
  geom_bar(stat = "identity",
           fill="tomato2")+
  scale_x_continuous(breaks = seq(min(YrstoReach_endangeredList ), max(YrstoReach_endangeredList), by = 1))+
  ggtitle("Frequency of number of sites moved to Endangered List") +
  ylab("Number of sites") +
  xlab("Number of Years") +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 15, hjust = 0.5),
        axis.title.y = element_text(size = 12, hjust = 0.5),
        axis.text.x = element_text(angle = 30,  
                                   vjust = 1, 
                                   size = 6, 
                                   hjust = 1))

                 


#############-------Observation from the bar chart------######################
#1. Only in 3rd year 1 site has been moved to Endangered list. After 3 years 
#no site has been moved from WHS list to Endangered list.
#2. The frequency of number of sites moving to endangered list is high.
# Within an year 8 sites were moved from World Heritage list to 
#Endangered list.
#3.There were 4 sites moved after being listed in WHS list for 34 long years
#and so on.
#4. There were no sites entered after 4 years until 7 years.There are 
#few gaps similar to this as we see from bar chart after 9th year no sites are
#put under endangered list for the complete year and so on.





####################------End of the Assignment2-----###########################