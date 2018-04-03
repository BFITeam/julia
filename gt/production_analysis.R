################################################################################
# File Name:    clean_counts.R
# Author(s):    Graham Tierney
# Written:      04/02/2018
#
# Description: merge the three years of data, reshape to unique production-author rows
# 
#
################################################################################

################################################################################
#---------------------------- Section 1: Preliminaries ------------------------#
################################################################################

require(tidyverse)
require(data.table)

#################
### Load Data ###
#################

count15 <- read.csv("data/COUNT 2014-2015 DATAJMJ.csv",stringsAsFactors = F)
count16 <- read.csv("data/COUNT 2015-2016 3_7_17.csv",stringsAsFactors = F)
names(count16)[7] <- "New.Play.or.Revival"
count17 <- read.csv("data/COUNT 2016-2017 DATA JMJ.csv",stringsAsFactors = F)
names(count17)[7] <- "New.Play.or.Revival"

count_data <- rbind(count15,count16,count17)
rm(count15,count16,count17)

names(count_data) <- str_to_lower(names(count_data)) %>% str_replace_all("\\.","_")

###############
### Reshape ###
###############

### reshape into one row per production-writer ###

#duplicate each row a number of times equal to the number of writers of the production
count_data <- count_data %>% mutate(num_writers = ("" != writer_1_name) + ("" != writer_2_name) + ("" != writer_3_name) + ("" != writer_4_name) + ("" != writer_5_name)) #,writer_3_name,writer_4_name,writer_5_name)))
dup_rows <- rep(1:nrow(count_data),times = count_data$num_writers)
count_data <- count_data[dup_rows,]

#make count from 1 to number of writers for each production
#this should get the same result wihout grouping: unlist(sapply(X = count_data$group_num,FUN = function(num){1:num}))
count_data <- count_data %>% group_by(production_title,theatre_name,theatre_season,theatre_city) %>% 
  mutate(group_num = row_number())

replace_writer_var <- function(var_name,data = count_data,row_id_var = "group_num"){
  
  #data-length list of which variable to pull
  vec_var_names <- str_c("writer_",pull(data,row_id_var),"_",var_name)
  
  #for each row, pull the variable specified by the group_num
  result <- sapply(X = 1:nrow(data),FUN = function(row){data[row,] %>% pull(vec_var_names[row])})
  unlist(result)
}

count_data <- count_data %>% ungroup() %>%  mutate(writer_name = replace_writer_var(var_name = "name"),
                                                   writer_gender = replace_writer_var("gender"),
                                                   writer_race = replace_writer_var("race"),
                                                   writer_nationality = replace_writer_var("nationality"))

#add weights
count_data <- count_data %>% mutate(weight = 1/num_writers)

#remove old columns
count_data <- count_data %>% select(-matches("^writer_[0-9]"))

#######################
### Clean Varaibles ###
#######################

count_data <- count_data %>% filter(theatre_name != "Theatre Name") #drop header rows recorded as variables

check_errors <- rbind(count_data %>% filter_at(vars(starts_with("writer_")),any_vars(. == "")),
                      count_data %>% filter(writer_nationality == "Female"))

count_data <- count_data %>% filter_at(vars(starts_with("writer_")),all_vars(. != "")) #drop all writers missing some information
count_data <- count_data %>% filter(writer_nationality != "Female") #Robert Hull from 2016 production of Empire

count_data$theatre_region[count_data$theatre_region == "Pacific Northwest" & count_data$theatre_city == "Atlanta"] <- "Deep South"

count_data <- count_data %>% group_by(production_title,theatre_name,theatre_season,theatre_city) %>% 
  mutate(num_writers = n(),
         weight = 1/num_writers) %>% #update number of writers and race for above drop rules 
  ungroup()

#detects if string matches any of the items in pattern, vectorized over string
str_detect_any <- function(string,pattern){
  sapply(X = pattern,FUN = str_detect,string = string) %>% apply(MARGIN = 1,max) %>% as.logical()
}
str_detect_any(c("AA","BB","C","d"),c("A","C"))

#trim all strings
count_data <- count_data %>% mutate_if(is.character,str_trim)

count_data <- count_data %>% mutate(writer_race = case_when(str_detect(writer_race,"(w|W)hite") ~ "white",
                                                            str_detect(writer_race,"Whtie") ~ "white", 
                                                            str_detect(writer_race,"(c|C)olor") ~ "of-color",
                                                            TRUE ~ writer_race)) #catchall at the end

count_data <- count_data %>% mutate(writer_gender = case_when(str_detect(writer_gender,"Male") ~ "male",
                                                            str_detect(writer_gender,"Female") ~ "female", 
                                                            str_detect(writer_gender,"Non-Binary") ~ "other",
                                                            str_detect(writer_gender,"Other") ~ "other",
                                                            TRUE ~ writer_gender)) #catchall at the end

count_data <- count_data %>% mutate(writer_nationality = case_when(str_detect(writer_nationality,"Non-American") ~ "non-american",
                                                                   str_detect(writer_nationality,"American") ~ "american",
                                                                   str_detect(writer_nationality,"Amerian") ~ "american",
                                                                   TRUE ~ writer_nationality))
#should La Mirada %>% LA, Skokie %>% Chicago?
count_data <- count_data %>% mutate(theatre_city = case_when(str_detect(theatre_city,"^Washington") ~ "Washington DC",
                                                             str_detect_any(theatre_city,c("Saint Paul","St\\. Paul","Minneapolis")) ~ "Minneapolis/St. Paul",
                                                             str_detect(theatre_city,"New York") ~ "New York City",
                                                             str_detect_any(theatre_city,c("Berkeley","San Francisco")) ~ "Berkeley/San Francisco",
                                                             str_detect_any(theatre_city,"La Miranda") ~ "La Mirada",
                                                             str_detect_any(theatre_city,"Fort Meyers") ~ "Fort Myers",
                                                             #str_detect(theatre_city,"Skokie") ~ "Chicago",
                                                             #str_detect(theatre_city,"La Mirada") ~ "Los Angeles",
                                                             TRUE ~ theatre_city))
################
### Analysis ###
################

count_data <- count_data %>% ungroup() #dataset needs to start ungrouped for analysis

count_freq_table <- function(group_vars,unique_level = "production",proportion_vars = "theatre_season",data = count_data){
  
  data <- ungroup(data) #ungroup for analysis
  group_vars <- unique(c(group_vars,"theatre_season")) #always group by season
  proportion_vars <- unique(c(proportion_vars,"theatre_season"))
  
  #subset to unique counts of unique_vars
  if(unique_level == "writer"){
    #remove duplicates by year-name-grouping varaibles
    data <- data %>% group_by_at(vars(one_of(c(group_vars)),starts_with("writer_"))) %>% filter(row_number() == 1) %>% ungroup()
    data$weight <- 1 #don't use weights for unique writer counts
  } else if(unique_level == "production"){
    #data already formated
  } else{
    return("invalid unique level")
  }
  
  #calculate statistics
  data <- data %>% group_by_at(vars(one_of(group_vars)))
  summary <- data %>% summarise(count = sum(weight)) %>% ungroup()
  summary <- summary %>% group_by_at(vars(one_of(proportion_vars))) %>% mutate(percentage = count/sum(count))
  
  fwrite(summary,file = str_c("output/",unique_level,"s_by_",str_c(group_vars,collapse = "_and_"),".csv"),row.names = F)
  summary
}

count_freq_table(c("theatre_season","writer_nationality"),"production")
count_freq_table(c("theatre_season","writer_race"),"production")
count_freq_table(c("theatre_season","writer_gender"),"production")
count_freq_table(c("theatre_season","writer_race","writer_gender"),"production")

count_freq_table(c("theatre_season","writer_gender"),"writer")

count_freq_table(c("theatre_season","theatre_region","writer_gender"),"writer",proportion_vars = "theatre_region")
