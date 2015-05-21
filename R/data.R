######### Title###########################
#Purpoer : R script for query information from the SKEP website
#
#



##### Loading the libraries ######
library(dplyr)
library(tidyr)


##### Login #####
urUsername <- "sjaisong"
urPassword <- "MovingProton793"
urdbname <- "syngenta"
urhost <- "crophealth.irri.org"


##### Database Connection ######
# I used this function from dplyr not from RMySQL
mydb <- src_mysql(user = urUsername,  
                  password = urPassword, 
                  dbname = urdbname,
                  host = urhost
                  )


##### LOAD DATABASE ############ 

###### Form 1 data #######
# There are 4 main tables
# 1. Farmer info ()
# 2. Fertilizers applied ()
# 3. Pesticides applied ()
# 4. Yields info ()
###########################

######## 1. Farmer info #########
#general.info <- tbl(mydb,"general_info") # keep out first

tbl(mydb,"general_info") %>%
        filter(season_id == 6)

######### select by year  ######
# 2013 dry season : select season_id == 1
# 2013 wet season : select season_id == 2
# 2014 dry season : select season_id == 3
# 2014 wet season : select season_id == 4
# 2015 dry season : select season_id == 5
# 2015 wet season : select season_id == 6
# see the list 
#tbl(mydb, "seasons")
# Example 
#tbl(mydb,"general_info") %>%
#        filter(season_id == 6)
#########################################


# select by country
# 1. Indonesia : select country == ID
# 2. Tamil Nadu : select country == IN and province == "IN-TN"
# 3. Odisha : select country == IN and province == "IN-OR"
# 4. Thailand : select country == "TH"
# 5. Vietnam : select country == "VN"
#see the list 
#tbl(mydb,"iso31662") %>% filter(subdivision_name == "Orissa")
# example
#tbl(mydb,"general_info") %>% filter(country == "IN" & province == "IN-OR")

crop.estab.met <- tbl(mydb,"crop_estab_met")

farm.info <- left_join(general.info, crop_estab_met, by = c( "id" = "main_id"))
head(farm.info)


###### 2. Fertilizer applied #####

fertil.organic <- tbl(mydb,"fertil_organic")

fertil.mineral <- tbl(mydb,"fertil_mineral")

all.ferti <- left_join(fertil.organic, fertil.mineral, by = "id_main")

##### 3. Pesticide applied #####

weed_mgmt <- tbl(mydb,"weed_mgmt")

molluscicide <- tbl(mydb,"molluscicide")

pesticides <- tbl(mydb,"pesticide")

pest_mix_type <- tbl(mydb,"pest_mix_type")

fungicide <- tbl(mydb,"fungicide")

all.mgmt <- left_join(weed_mgmt, molluscicid, pesticides, fungicide ,pest_mix_type ,  by = "id_main")

##### 4.Yields info ##########

weight.harv <- tbl(mydb,"weight_harv")

######### Form 2 data ################
# There are main #####
# 1. injuries info
# 2. systemic injuries info
# 3. weed infestration info
# 4. weed species info


#### 1. injuries info #############

hill.quad <- tbl(mydb, "hill_quad")

hill.quad.df <- collect(hill.quad)

hill.quad.df <-  hill.quad.df %>%
        select(-id_hq) %>%
        transform(type_hq = as.factor(type_hq))

names(hill.quad.df)[names(hill.quad.df) == "hq_sample"] <- "sample"

levels(hill.quad.df$type_hq) <- c("nt", "np", "nl")

hill.quad.df.spread <- spread(hill.quad.df, type_hq, data)
        
#type_hq <- tbl(mydb, "type_hq")

#===================================================
injuries <- tbl(mydb,"injury")

injuries.df <- collect(injuries)

injuries.df <- injuries.df %>%
        select(-id_pest_tp) %>%
        transform(inj_tp_type = as.factor(inj_tp_type))

names(injuries.df)[names(injuries.df) == "inj_sample"] <- "sample"

injury.detail <- collect(tbl(mydb,"injury_detail"))$injury

levels(injuries.df$inj_tp_type) <- injury.detail

injuries.df.spread <- spread(injuries.df, inj_tp_type, inj_tp_data)

#====================================================
#combine

all.injuries <- left_join(hill.quad.df.spread, injuries.df.spread, by =c( "id_ci", "sample" ))

#################################

################################



head(injury.df)
general.info <- tbl(conDdplyr,"general_info")
all.inj <- left_join(hill.quad, injury, by = c("id_ci" = "id_ci", "hq_sample" = "inj_sample"))

all.inj <- collect(all.inj)
class(all.inj)

all.inj1 <- all.inj %>% transform(
        inj_tp_type = as.factor(inj_tp_type)) 

spread.all.inj <- spread(all.inj, inj_tp_type, inj_tp_data)

# myDF1 <- filter(myDF, fare > 150)
# myDF2 <- select(myDF1, pclass,sex,age,fare)
# myDF3 <- group_by(myDF2, pclass,sex)
# myDF4 <- summarise(myDF3, 
#                    avg_age = mean(age),
#                    avg_fare = mean(fare))


head(dbReadTable(mydb, list.table[4]))
names(dbReadTable(mydb, "active_ingr"))


