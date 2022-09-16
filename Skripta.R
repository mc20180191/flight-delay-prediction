library(ggplot2)
library(caret)

dataframe =  read.csv("2017.csv", stringsAsFactors = FALSE)
summary(dataframe)

#Kolona unnamed ima sve NA vrednosti, pa izbacujemo:
dataframe$Unnamed..27 = NULL

################  PROMENLJIVA CANCELLED ###################################
unique(dataframe$CANCELLED)
dataframe$CANCELLED = as.factor(dataframe$CANCELLED)
prop.table(table(dataframe$CANCELLED))
## Otkazanih letova ima samo oko 1%

head(dataframe[dataframe$CANCELLED==1,])
#Posto otkazani letovi, za skoro sve varijable, imaju NA vrednosti (jer se let nije ni desio,
#pa zbog toga nema podataka o njima)
#Te obzervacije, kao i kolonu, necemo da ukljucimo u razmatranje.

dataframe = dataframe[dataframe$CANCELLED==0,]
dataframe$CANCELLED = NULL
dataframe$CANCELLATION_CODE = NULL
#Posto smo izbacili cancelled, izbacujemo i cancellation code jer je on vezan za otkazane letove

############ PRAVLJENJE IZLAZNE PROMENLJIVE DELAYED #########################

sum(is.na(dataframe$ARR_DELAY))
dataframe = dataframe[!is.na(dataframe$ARR_DELAY), ]
#Arr_delay ima 12.518 NA, izbacicemo te obzrv

dataframe$DELAYED = ifelse(dataframe$ARR_DELAY>15, "Yes", "No")
dataframe$DELAYED = as.factor(dataframe$DELAYED)
prop.table(table(dataframe$DELAYED))
#0.82, 0.17
dataframe$ARR_DELAY = NULL

#################################################################################################
#Sada kada je definisana izlazna prom. nastavlja se ostatak analize na manjem skupu:

set.seed(5)
indexes =  createDataPartition(dataframe$DELAYED, p = 0.02, list = FALSE)
sample_df <- dataframe[indexes, ]

prop.table(table(sample_df$DELAYED))
#0.82, 0.17
summary(sample_df)

######################## SREDJIVANJE NA VREDNOSTI ###############################################

apply(sample_df, 2, function(x) sum(is.na(x)) / nrow(sample_df))

#PREKO 80% NA's imaju kolone:
#CARRIER_DELAY, WEATHER_DELAY, NAS_DELAY,  SECURITY_DELAY, LATE_AIRCRAFT_DELAY, izbacujemo ih:
sample_df$CARRIER_DELAY = NULL
sample_df$WEATHER_DELAY = NULL
sample_df$NAS_DELAY = NULL
sample_df$SECURITY_DELAY = NULL
sample_df$LATE_AIRCRAFT_DELAY = NULL

#Isklj varijable koje se desavaju nakon poletanja:
#WHEELS_ON, ARR_TIME, ACTUAL_ELAPSED_TIME, AIR_TIME, TAXI IN:
sample_df$WHEELS_ON = NULL
sample_df$ARR_TIME = NULL
sample_df$ACTUAL_ELAPSED_TIME = NULL
sample_df$AIR_TIME = NULL
sample_df$TAXI_IN = NULL

sample_df$OP_CARRIER_FL_NUM = NULL
#Nije potreban broj leta, koristicemo OP_CARRIER OZNAKU

sample_df$DEP_TIME = NULL
sample_df$DEP_DELAY = NULL
#DEP_DELAY I DEP TIME izbacujemo jer ako nam je on poznat, bice nam poznato i kasnjenje
################################# PROVERA CHAR VARIJABLI ##################################

apply(sample_df[,c(1,2,4,5)], 2, function(x) sum(x=="" | x==" " | x=="-" | x=="/"))
#Nemaju NA


table(sample_df$DIVERTED)
#Pretvaramo Diverted u faktorsku
sample_df$DIVERTED = as.factor(sample_df$DIVERTED)
prop.table(table(sample_df$DIVERTED))
sample_df$DIVERTED = NULL

unique(sample_df$OP_CARRIER)
unique(sample_df$DEST)
unique(sample_df$ORIGIN)
#Ove sve mozemo u faktorske

sample_df$OP_CARRIER = as.factor(sample_df$OP_CARRIER)

summary(as.integer(table(sample_df$ORIGIN)))
summary(as.integer(table(sample_df$DEST)))
#Smanjicemo broj nivoa, onih kojima je frek manja od 20 idu u other

origin_freq <- table(sample_df$ORIGIN)
dest_freq <- table(sample_df$DEST)

origins_to_replace <- names(origin_freq[origin_freq < 20])
dest_to_replace <- names(dest_freq[dest_freq < 20])

sample_df$ORIGIN[sample_df$ORIGIN %in% origins_to_replace] <- "OTHER"
sample_df$DEST[sample_df$DEST %in% dest_to_replace] <- "OTHER"

sample_df$DEST = as.factor(sample_df$DEST)
sample_df$ORIGIN = as.factor(sample_df$ORIGIN)


########################### ARR_TIME I DEP_TIME ##########################################

ggplot(sample_df) +
  geom_density(aes(x = CRS_DEP_TIME, color="CRS_DEP_TIME") ) +
  geom_density(aes(x = WHEELS_OFF, color = "WHEELS_OFF")) +
  theme_minimal()

#Wheels off ima skoro identicnu raspodelu kao crs dep time, tako da mozemo da
#izbacimo wheels of:
sample_df$WHEELS_OFF = NULL

############################ FL_DATE ###################################################
#Prebacujemo FL_DATE U DATUM, da bismo izvukli MESEC i DAN
#Godina nije potrebna jer je sve iz 2017.

sample_df$FL_DATE = as.Date(sample_df$FL_DATE)

sample_df$MONTH = months(sample_df$FL_DATE)
sample_df$DAY = weekdays(sample_df$FL_DATE)
sample_df$MONTH = factor(sample_df$MONTH, levels = c("January","February","March","April","May","June","July","August","September","October","November","December"))
sample_df$DAY = factor(sample_df$DAY, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday","Sunday"))

sample_df$FL_DATE = NULL

sample_df = sample_df[, c(1:8,10,11,9)]

########################### PLOTOVI #####################################################

ggplot(sample_df, aes(x=OP_CARRIER, fill=DELAYED)) +
  geom_bar(position="fill") + labs(title = "Delayed flight by carriers", y="Proportion")

ggplot(sample_df, aes(x=DEST, fill=DELAYED)) +
  geom_bar(position="fill") + 
  labs(title = "Flight destination", y="Proportion") +
  theme(axis.text.x=element_blank())

ggplot(sample_df, aes(x=ORIGIN, fill=DELAYED)) +
  geom_bar(position="fill") + 
  labs(title = "Flight origin", y="Proportion") +
  theme(axis.text.x=element_blank())

ggplot(sample_df, aes(x=CRS_DEP_TIME, fill=DELAYED)) +
  geom_density(alpha=0.5)

ggplot(sample_df, aes(x=CRS_ARR_TIME, fill=DELAYED)) +
  geom_density(alpha=0.5)

ggplot(sample_df, aes(x=CRS_ELAPSED_TIME, fill=DELAYED)) +
  geom_density(alpha=0.5)
sample_df$CRS_ELAPSED_TIME=NULL

ggplot(sample_df, aes(x=MONTH, fill=DELAYED)) +
  geom_bar(position="fill") + labs(title = "Delayed flight by month", y="Proportion")

ggplot(sample_df, aes(x=DAY, fill=DELAYED)) +
  geom_bar(position="fill") + labs(title = "Delayed flight by day", y="Proportion")

#############################################################################

ggplot(sample_df, aes(x=TAXI_OUT, fill=DELAYED)) +
  geom_density(alpha=0.5)

ggplot(sample_df, aes(x=DISTANCE, fill=DELAYED)) +
  geom_density(alpha=0.5)

sample_df$DISTANCE = NULL

##############################################################################

saveRDS(sample_df, file = "sample_dataset.rds")
