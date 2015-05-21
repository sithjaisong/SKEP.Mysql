###########################Header##############################################
# title         : data.R;
# purpose       : Loads data from SKEP website of Syngenta SKEP1 data;
# producer      : prepared by S. Jaisong (s.jaisong@irri.org);
# last update   : in Los Baños, Laguna, PHL, May 2015;
# inputs        :  Databsed of SKEP
# outputs       : gernal info and injury profiles
# remarks 1     : ;
# remarks 2     : ;
###############################End############################################
##### Loading the libraries ######
library(dplyr)
library(tidyr)

###### Login ######
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

weed_mgmt <- collect(tbl(mydb,"weed_mgmt"))

weed.mgnt.type <- collect(tbl(mydb,"weed_mgnt_type"))

#  weed_mgnt_type

molluscicide <- tbl(mydb,"molluscicide")

pesticides <- collect(tbl(mydb,"pesticides"))

pest_mix_type <- tbl(mydb,"pest_mix_type")

fungicide <- tbl(mydb,"fungicide")

all.mgmt <- left_join(weed_mgmt, molluscicid, pesticides, fungicide ,pest_mix_type ,  by = "id_main")

##### 4.Yields info ##########

weight.harv <- tbl(mydb,"weight_harv")

######### Form 2 data ################
# There are main 
# 1. injuries info
# 2. systemic injuries info
# 3. weed infestration info


##### 1. injuries info #############
# load data of no of tillesr, no of panicles , and no of leaves per tiller
hill.quad <- tbl(mydb, "hill_quad") 

#save as the data frame
hill.quad.df <- collect(hill.quad) 

# remove unnessary column and correct var.type of type_hq to factor
hill.quad.df <-  hill.quad.df %>%  
        select(-id_hq) %>%
        transform(type_hq = as.factor(type_hq))

# rename column names hq_sample to just sample
names(hill.quad.df)[names(hill.quad.df) == "hq_sample"] <- "sample"

# rename factor names of type_hq from code to abbreviation
levels(hill.quad.df$type_hq) <- c("nt", "np", "nl")

# reference of tyope of hill data
#type_hq <- tbl(mydb, "type_hq")

# make data tidy
hill.quad.df.spread <- spread(hill.quad.df, type_hq, data)
        

# load injury table, which contain the all leave injuries, tiller injuires, panicle injuries
injuries <- tbl(mydb,"injury")

# save as data frame
injuries.df <- collect(injuries)

# revome unnessary column and corract var.type to factor
injuries.df <- injuries.df %>%
        select(-id_pest_tp) %>%
        transform(inj_tp_type = as.factor(inj_tp_type))

# rename inj_sample to sample for matching with the hill data
names(injuries.df)[names(injuries.df) == "inj_sample"] <- "sample"

# save list of injuires 
injury.detail <- collect(tbl(mydb,"injury_detail"))$injury

#rename the levels of injury types 
levels(injuries.df$inj_tp_type) <- injury.detail

# make data more tidy
injuries.df.spread <- spread(injuries.df, inj_tp_type, inj_tp_data)

# join the hill data and injury data
all.injuries <- left_join(hill.quad.df.spread, injuries.df.spread, by =c( "id_ci", "sample" ))


##### 2. systemic injuries info #####
# save as data fram of systemic injuries table
#systemic <- collect(tbl(mydb, "systemis"))
# delete unnessary column 
systemic$id_syst <- NULL
#set as factor of type of systemic injuiries
systemic$sys_type_id <- as.factor(systemic$sys_type_id)

# save the systemic list to data frame
systemic.type <- collect(tbl(mydb,"systemis_type"))$injury
# rename the systemic type code to name of systemic injuries
levels(systemic$sys_type_id) <- systemic.type

# make data more tidy
systemic.df.spread <- spread(systemic, sys_type_id, inj_data)

#### 3. weed infestration info ######

weed.rank <- collect(tbl(mydb, "weed_rank"))

# remove the unnessary column
weed.rank$id_weed_rank <- NULL
weed.rank$main_weed <- NULL

# set weed type as the factor
weed.rank$weed_type <- as.factor(weed.rank$weed_type)

# save the list of weed type
weed.type <- collect(tbl(mydb,"weed_type"))$weed_type

# rename weed type codes to weed type names
levels(weed.rank$weed_type) <- weed.type

# make data more tidy
weed.rank.df.spread <- spread(weed.rank, weed_type, data)

# save as data frame of main weed species
weed.main <- collect(tbl(mydb, "weed_main"))

# set sampling area as the factor
weed.main$area <- as.factor(weed.main$area)

# rename sampling area from A, B, C to 1, 2, 3
levels(weed.main$area) <- c("1", "2", "3")

# remove unnessary column 
weed.main$id_weed_main <- NULL

# save to data frame of weed species lists
weed.list <- collect(tbl(mydb,"weed_list"))

# combine the main weed spcies with weed main species codes
all.weed <- left_join(weed.main, weed.list, by = "weed_list_id" )

# combine weed ranking with main weed species
weed.info <- left_join(weed.rank.df.spread, all.weed, by = c("id_ci" , "area"))

# eos