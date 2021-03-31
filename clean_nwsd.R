
########################################################################################################################
########################################################################################################################
#####################################   CLEANING THE NWSD DATA FOR ANALYSIS   ##########################################
########################################################################################################################
########################################################################################################################


# COMMENT:
# This script cleans the raw strike data accessed directly from https://wildlife.faa.gov/search on April 15th, 2020.
# Data cleaning is performed to keep as much of the original information in the data as possible, though not all 
# variables are included in the analysis. The result of this script is a data frame with the cleaned data that is used 
# in the analyses performed in the analysis.R script.


# Load the raw NWSD data (last accessed on April 15th, 2020):
strike_raw_1 <- read.csv('strike_data_raw_1.csv')
strike_raw_2 <- read.csv('strike_data_raw_2.csv')
strike_raw <- rbind(strike_raw_1, strike_raw_2)
rm(strike_raw_1, strike_raw_2)

# Initialize new data frame using strike id variable:
strike <- data.frame(strike_raw$INDEX_NR)
names(strike)[1] <- 'index_nr'

# Date of the strike (already in POSIXct format):
strike$incident_date <- strike_raw$INCIDENT_DATE

# Month of the strike:
strike$incident_month <- as.factor(strike_raw$INCIDENT_MONTH)
levels(strike$incident_month) <- c('January', 'February', 'March' , 'April', 'May', 'June', 'July', 'August', 
                                   'September' , 'October', 'November', 'December')

# Year of the strike:
strike$incident_year <- strike_raw$INCIDENT_YEAR

# Precise time of the strike:
strike$temp_hour <- as.numeric(sub(':.*', '', strike_raw$TIME))
strike$temp_minute <- as.numeric(sub('.*:', '', strike_raw$TIME))
strike$temp_hour  <- recode(strike$temp_hour, '-24:-1=NA')
strike$temp_minute <- recode(strike$temp_minute, '60:99=30')
strike$time <- hms(hour = strike$temp_hour, minute = strike$temp_minute)
strike$temp_hour <- NULL
strike$temp_minute <- NULL

# Time of day that that strike occurred (lighting):
strike$time_of_day <- strike_raw$TIME_OF_DAY

# Airport identification and information:
strike$airport_id <- as.character(strike_raw$AIRPORT_ID)
strike$airport_name <- as.character(strike_raw$AIRPORT)

# If airport id is missing, but the id can be found in the airport name variable, then replace missing value:
strike$airport_id <- ifelse(is.na(strike$airport_id) & substr(strike$airport_name, 1, 1)=='(', 
                            substr(strike$airport_name, 2, 5),  strike$airport_id)

# If airport id is missing and the airport name variable is reporting the airport id, then replace missing value:
strike$airport_id <- ifelse(is.na(strike$airport_id) & nchar(strike$airport_name)<=7, 
                            strike$airport_name,  strike$airport_id)

# Trim the airport id string variable of excess spaces:
strike$airport_id <- trimws(strike$airport_id)

# Remove odd punctuation from airport id:
strike$airport_id <- str_replace_all(strike$airport_id, '[[:punct:]]', '')

# Replace unknown code 'ZZZZ' with NA for merge (26,525 strike observations occurred at an unknown airport):
strike$airport_id <- recode(strike$airport_id, "'ZZZZ'=NA")

# Merge in airport info data:

    # Download airport information from the airport-codes github repository:
    airports <- read.csv('https://raw.githubusercontent.com/datasets/airport-codes/master/data/airport-codes.csv')

    # Merging airport data to strike data by airport id. There are four variables in the airports data that provide
    # different airport id codes using different classification schemes. It is not clear which codes are reported in
    # NWSD. So merging is performed sequntially on the four different airport identifiers until all matches are found.
    
        # First, merge by the airports$ident variable:
        strike_0 <- merge(strike, airports, by.x = 'airport_id', by.y = 'ident', all.x = TRUE)
        nonmerged_0 <- subset(strike_0, is.na(strike_0$name))
        merged_0 <- subset(strike_0, !is.na(strike_0$name))
        rm(strike_0)
        
        # Second, merge by the airports$local_code variable (only on unmatched observations):
        nonmerged_0 <- nonmerged_0[c(1:8)]
        nonmerged_0 <- merge(nonmerged_0, airports, by.x = 'airport_id', by.y = 'local_code', all.x = TRUE)
        nonmerged_1 <- subset(nonmerged_0, is.na(nonmerged_0$name))
        merged_1 <- subset(nonmerged_0, !is.na(nonmerged_0$name))
        rm(nonmerged_0)
        
        # Third, merge by the airports$iata_code variable (only on unmatched observations):
        nonmerged_1 <- nonmerged_1[c(1:8)]
        nonmerged_1 <- merge(nonmerged_1, airports, by.x = 'airport_id', by.y = 'iata_code', all.x = TRUE)
        nonmerged_2 <- subset(nonmerged_1, is.na(nonmerged_1$name))
        merged_2 <- subset(nonmerged_1, !is.na(nonmerged_1$name))
        rm(nonmerged_1)
        
        # finally, by the airports$gps_code variable (only on unmatched observations):
        nonmerged_2 <- nonmerged_2[c(1:8)]
        nonmerged_2 <- merge(nonmerged_2, airports, by.x = 'airport_id', by.y = 'gps_code', all.x = TRUE)
        nonmerged_3 <- subset(nonmerged_2, is.na(nonmerged_2$name))
        merged_3 <- subset(nonmerged_2, !is.na(nonmerged_2$name))
        rm(nonmerged_2)
        
        # Compiling the merged_0, merged_1, merged_2, merged_3, and nonmerged_3 data.
        vars <- c('index_nr', 'incident_date', 'incident_month', 'incident_year', 'time', 'time_of_day', 
                  'airport_id', 'airport_name', 'name', 'iso_country', 'iso_region', 'type', 
                  'elevation_ft', 'coordinates')
        merged_0 <- merged_0[vars]
        merged_1 <- merged_1[vars]
        merged_2 <- merged_2[vars]
        merged_3 <- merged_3[vars]
        nonmerged_3 <- nonmerged_3[vars]
        strike <- rbind(merged_0, merged_1, merged_2, merged_3, nonmerged_3)
        strike <- strike[,vars]
        rm(vars)
        rm(merged_0, merged_1, merged_2, merged_3, nonmerged_3)
        
        # Only 109 observations with non-missing airport ID failed to merge with the github airport information data.
        # 72 of these 109 are coded as occuring on remote water (H2O), a private air strip (PVT), 
        # or an oil rig (RIGG). The other 37 strike obseravtions are distributed across 29 small airports.
        
        # Remove airport information data frame:
        rm(airports)
        
# Parse lat and long coordinates variable into two separate numeric variables:
strike$coordinates <- as.character(strike$coordinates)
strike$latitude <- as.numeric(sub(',.*', '', strike$coordinates))
strike$longitude <- as.numeric(sub('.*,', '', strike$coordinates))
strike$coordinates <- NULL

# Country codes/names and state codes/names:
strike$country_name <- countrycode(strike$iso_country, origin = 'iso2c', destination = 'country.name')
strike$state_code <- ifelse(strike$country_name=="United States", sub('.*-', '', strike$iso_region), NA)
strike$state_name <- state.name[match(strike$state_code,state.abb)]
strike$state_name[strike$state_code=='DC'] <- 'District of Columbia'
strike$country_code <- as.character(strike$iso_country)

# Re-sort data on strike id so that the rows of the data match for further cleaning:
strike <- strike[order(strike$index_nr),]
strike_raw <- strike_raw[order(strike_raw$INDEX_NR),]

# FAA region variable:
strike$faa_region <- trimws(as.character(strike_raw$FAAREGION))
strike$faa_region <- recode(strike$faa_region, "'ASW'='Southwest (ASW)';
                                'ANM'='Northwest Mountain (ANM)';
                                'AEA'='Eastern (AEA)';
                                'ASO'='Southern (ASO)';
                                'AGL'='Great Lakes (AGL)';
                                'ACE'='Central (ACE)';
                                'ANE'='New England (ANE)';
                                'AWP'='Western Pacific (AWP)';
                                'AAL'='Alaska (AAL)';
                                'FGN'='Foreign (FGN)'")

# Rename some variables:
strike <- rename(strike, airport_name_1 = airport_name, airport_name_2 = name, 
                 airport_type = type, airport_elev = elevation_ft)
strike$airport_name_2 <- as.character(strike$airport_name_2)

# Reorder variables in clean data frame:
vars <- c('index_nr', 'incident_date', 'incident_month', 'incident_year', 'time', 'time_of_day', 
          'airport_id', 'airport_name_1', 'airport_name_2', 'airport_type', 'country_code', 
          'country_name', 'state_code', 'state_name', 'faa_region', 'latitude', 'longitude','airport_elev')
strike <- strike[,vars]
rm(vars)

# Detailed description of where the strike occurred:
strike$unknown_loc_info <- as.character(strike_raw$LOCATION)

# The phase of flight in which the strike occurred:
strike$phase_of_flight <- recode(strike_raw$PHASE_OF_FLIGHT, "'Unknown'=NA", as.factor = TRUE)

# Height of the aircraft (feet from ground level) when the strike occurred:
strike$height <- strike_raw$HEIGHT

# Speed the aircraft was traveling at (knots) when the strike occurred:
strike$speed <- strike_raw$SPEED

# Distance from airport (miles) when the strike occurred:
strike$distance <- strike_raw$DISTANCE

# Title of person reporting the strike:
strike$reporting_person <- strike_raw$PERSON

# Airline operator ID:
strike$op_id <- as.character(strike_raw$OPID)

# Airline operator name.
strike$op_name <- as.character(strike_raw$OPERATOR)

# Aircraft registration:
strike$registration <- trimws(as.character(strike_raw$REG))
strike$registration[strike$registration=='NULL'] <- NA

# Flight number:
strike$flight_num <- trimws(as.character(strike_raw$FLT))
strike$flight_num[strike$flight_num=='NULL'] <- NA

# Aircraft type:
strike$arcft_class <- recode(trimws(strike_raw$AC_CLASS), "'A'='Airplane';
                                   'B'='Helicopter';
                                   'C'='Glider';
                                   'D'='Balloon';
                                   'F'='Dirigible';;
                                   'I'='Gyroplane';
                                   'J'='Ultralight';
                                   'Y'='Other';
                                   'Z'=NA;
                                   'A/B'=NA;
                                   'NULL'=NA", as.factor = TRUE)

# Aircraft mass:
strike$arcft_mass <- as.factor(strike_raw$AC_MASS)
levels(strike$arcft_mass) <- c('2,250 kg or less', '2,251-5,700 kg', '5,701-27,000 kg', 
                             '27,001-272,000 kg', 'above 272,000 kg')

# Aircraft make:
strike$arcft_make_code <- strike_raw$AMA

# Aircraft model:
strike$arcft_model_code <- strike_raw$AMO

# Aircraft engine type:
strike$engn_type <- recode(strike_raw$TYPE_ENG, "'A'='Reciprocating Engine (piston)';
                                                   'B'='Turbojet'; 
                                                   'C'='Turboprop'; 
                                                   'D'='Turbofan'; 
                                                   'E'='None (glider)'; 
                                                   'F'='Turboshaft (helicopter)'; 
                                                   'Y'='Other'",  as.factor = TRUE)

# Engine make:
strike$engn_make_code <- strike_raw$EMA

# Engine model:
strike$engn_model_code <- strike_raw$EMO

# Number of engines:
strike$num_engns <- strike_raw$NUM_ENGS

# The engine position codes in the read_me.xls file and the data don't match up. Simply code them as a factor 
# variable for now:
strike$engn_1_pos <- as.factor(strike_raw$ENG_1_POS)
strike$engn_2_pos <- as.factor(strike_raw$ENG_2_POS)
strike$engn_3_pos <- recode(strike_raw$ENG_3_POS, "'NULL'=NA", as.factor = TRUE)
strike$engn_4_pos <- recode(strike_raw$ENG_4_POS, "'NULL'=NA", as.factor = TRUE)

# Sky and weather descriptions:
strike$sky <- strike_raw$SKY
strike$precipitation <- recode(strike_raw$PRECIPITATION, "'None, Rain'='Rain';'None, Snow'='Snow'",
                             as.factor = TRUE)

# Indicator variable for whether or not the aircraft was damaged:
strike$damage <- recode(strike_raw$INDICATED_DAMAGE, "'0'=0;'1'=1", as.numeric = TRUE)

# Factor variable indicating the intensity of the damage (recode the military codes to the civil codes):
strike$damage_type <- recode(strike_raw$DAMAGE_LEVEL, "'Class A'='D';
                           'Class B'='S';
                           'Class C'='S';
                           'Class D'='M';
                           'Class E'='M'")

# Some strike reports that indicate damage have a 'None' damage type (fix this):
strike$damage_type[strike$damage==1 & strike$damage_type=='N'] <- NA

# Some strike reports that indicate no damage but indicate a damage type (fix this):
strike$damage[strike$damage==0 & strike$damage_type!='N' & !is.na(strike$damage_type)] <- 1

# Some strike reports indicate damage=0 but have missing damage type (fix this):
strike$damage_type[strike$damage==0 & is.na(strike$damage_type)] <- 'N'

# Indicator variables describing where the strike/damage occurred:
strike$str_rad <- recode(strike_raw$STR_RAD, "'0'=0;'1'=1", as.numeric = TRUE)
strike$str_windshld <- recode(strike_raw$STR_WINDSHLD, "'0'=0;'1'=1", as.numeric = TRUE)
strike$str_nose <- recode(strike_raw$STR_NOSE, "'0'=0;'1'=1", as.numeric = TRUE)
strike$str_eng1<- recode(strike_raw$STR_ENG1, "'0'=0;'1'=1", as.numeric = TRUE)
strike$str_eng2 <- recode(strike_raw$STR_ENG2, "'0'=0;'1'=1", as.numeric = TRUE)
strike$str_eng3 <- recode(strike_raw$STR_ENG3, "'0'=0;'1'=1", as.numeric = TRUE)
strike$str_eng4 <- recode(strike_raw$STR_ENG4, "'0'=0;'1'=1", as.numeric = TRUE)
strike$str_wing_rot <- recode(strike_raw$STR_WING_ROT, "'0'=0;'1'=1", as.numeric = TRUE)
strike$str_fuse <- recode(strike_raw$STR_FUSE, "'0'=0;'1'=1", as.numeric = TRUE)
strike$str_lg <- recode(strike_raw$STR_LG, "'0'=0;'1'=1", as.numeric = TRUE)
strike$str_tail <- recode(strike_raw$STR_TAIL, "'0'=0;'1'=1", as.numeric = TRUE)
strike$str_light <- recode(strike_raw$STR_LGHTS, "'0'=0;'1'=1", as.numeric = TRUE)
strike$str_other <- recode(strike_raw$STR_OTHER, "'0'=0;'1'=1", as.numeric = TRUE)
strike$str_prop <- recode(strike_raw$STR_PROP, "'0'=0;'1'=1", as.numeric = TRUE)
strike$dam_rad <- recode(strike_raw$DAM_RAD, "'0'=0;'1'=1", as.numeric = TRUE)
strike$dam_windshld <- recode(strike_raw$DAM_WINDSHLD, "'0'=0;'1'=1", as.numeric = TRUE)
strike$dam_nose <- recode(strike_raw$DAM_NOSE, "'0'=0;'1'=1", as.numeric = TRUE)
strike$dam_eng1 <- recode(strike_raw$DAM_ENG1, "'0'=0;'1'=1", as.numeric = TRUE)
strike$dam_eng2 <- recode(strike_raw$DAM_ENG2, "'0'=0;'1'=1", as.numeric = TRUE)
strike$dam_eng3 <- recode(strike_raw$DAM_ENG3, "'0'=0;'1'=1", as.numeric = TRUE)
strike$dam_eng4 <- recode(strike_raw$DAM_ENG4, "'0'=0;'1'=1", as.numeric = TRUE)
strike$dam_wing_rot <- recode(strike_raw$DAM_WING_ROT, "'0'=0;'1'=1", as.numeric = TRUE)
strike$dam_fuse <- recode(strike_raw$DAM_FUSE, "'0'=0;'1'=1", as.numeric = TRUE)
strike$dam_lg <- recode(strike_raw$DAM_LG, "'0'=0;'1'=1", as.numeric = TRUE)
strike$dam_tail <- recode(strike_raw$DAM_TAIL, "'0'=0;'1'=1", as.numeric = TRUE)
strike$dam_light <- recode(strike_raw$DAM_LGHTS, "'0'=0;'1'=1", as.numeric = TRUE)
strike$dam_other <- recode(strike_raw$DAM_OTHER, "'0'=0;'1'=1", as.numeric = TRUE)
strike$dam_prop <- recode(strike_raw$DAM_PROP, "'0'=0;'1'=1", as.numeric = TRUE)
strike$ingested <- recode(strike_raw$INGESTED, "'0'=0;'1'=1", as.numeric = TRUE)

# If the  strike occurred in an 'other' place on the aircraft, this variable gives the description:
strike$other_specify <- as.character(strike_raw$OTHER_SPECIFY)

# The effect of the strike on the flight:
strike$strike_effect <- recode(strike_raw$EFFECT, "'None, Other'='Other';'None, Precautionary Landing'='Precautionary Landing'")

# If the effect was 'Other', this variable gives the explanation:
strike$strike_effect_other <- as.character(strike_raw$EFFECT_OTHER)

# Information about the wildlife involved in the strike:
strike$remains_collected <- strike_raw$REMAINS_COLLECTED
strike$remains_sent <- strike_raw$REMAINS_SENT
strike$species_id <- as.character(strike_raw$SPECIES_ID)
strike$species <- as.character(strike_raw$SPECIES)

# Number of wildlife seen:
strike$number_seen <- recode(strike_raw$NUM_SEEN, "'NULL'=NA;
                                                   '11-100'='11 or more';
                                                   'More than 100'='11 or more'", as.factor = TRUE)

# Number of wildlife struck:
strike$number_struck <- recode(strike_raw$NUM_STRUCK, "'NULL'=NA;
                                                       '11-100'='11 or more';
                                                       'More than 100'='11 or more'", as.factor = TRUE)

# Size of wildlife struck:
strike$animal_size <- strike_raw$SIZE

# Indicator variable for if the pilot was warned about a potential strike:
strike$warned <- recode(strike_raw$WARNED, "'Unknown'=NA")

# How many hours the aircraft was out of service:
strike$hours_aos <- strike_raw$AOS

# Cost variables (real values measured in 2018 $):
strike$nom_cost_repair <- strike_raw$COST_REPAIRS
strike$nom_cost_other <- strike_raw$OTHER_COST
inf_adjust <- read.csv('inf_adjust.csv')
strike <- merge(strike, inf_adjust, by.x = 'incident_year', by.y = 'year', all.x = TRUE)
strike$real_cost_repair <- strike$nom_cost_repair*strike$inf_multiplier
strike$real_cost_other <- strike$nom_cost_other*strike$inf_multiplier
rm(inf_adjust)

# Some strike reports indicate damage=0 but have repair costs (fix this):
strike$damage_type[strike$damage==0 & strike$real_cost_repair>0 & strike$damage_type=='N'] <- NA
strike$damage[strike$damage==0 & strike$real_cost_repair>0] <- 1

# Injuries and fatalities:
strike$num_injured <- recode(strike_raw$NR_INJURIES, "NA=0")
strike$num_fatal <- recode(strike_raw$NR_FATALITIES, "NA=0")

# Final variable is a string with the 'remarks' left by the reporting person:
strike$remarks <- as.character(strike_raw$REMARKS)


########################################################################################################################
########################################################################################################################
#####################################                   END                   ##########################################
########################################################################################################################
########################################################################################################################
