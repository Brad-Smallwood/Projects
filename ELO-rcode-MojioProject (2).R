# ELO R Code

# Packages
install.packages("dplyr")
install.packages("ggplot2")
install.packages("plyr")
install.packages("varhandle")

library(dplyr)
library(ggplot2)
library(plyr)
library(varhandle)

# Data Readin

# Working Directory
setwd("~/Desktop/Case Study 2017- Mojio")

# Data Set 1
load("case_study_dt1.RData")

# Splitting Dataframe by source_id

cs1 <- split(case_study_dt1, list(case_study_dt1$source_id))
nameHolder = names(cs1)
car1 <- cs1[[nameHolder[1]]] # list elements need to be double indexed like so
car74 = cs1[[nameHolder[74]]]
car52 = cs1[[nameHolder[52]]]
hist(as.numeric(car52$source_Vehicle_Speed_Value))
car110 = cs1[[nameHolder[110]]]
hist(as.numeric(car110$source_Vehicle_Speed_Value))
car1$source_Vehicle_Location_Address_County

View(car1)

# Function below converts time factor to time date variable
# The df should be double selected from the list before proceeding
# The var to be passed must be done so as a character because it will be thought of otherwise as an object
cleaner = function(df, var){
  time = as.character(df[ , var])
  time = substr(time, 1, 19)
  time = gsub("T", " ", time)
  time = strptime(time, "%Y-%m-%d %H:%M:%S", tz = "")
  return(time)
}

# Calls to cleaner() function
car1$source_Vehicle_Acceleration_Timestamp = cleaner(car1, "source_Vehicle_Acceleration_Timestamp")
car1$source_Vehicle_Speed_Timestamp        = cleaner(car1, "source_Vehicle_Speed_Timestamp")
car74$source_Vehicle_Acceleration_Timestamp= cleaner(car74,"source_Vehicle_Acceleration_Timestamp")
car74$source_Vehicle_Speed_Timestamp       = cleaner(car74,"source_Vehicle_Speed_Timestamp")
str(car1$source_Vehicle_Acceleration_Timestamp)
str(car74$source_Vehicle_Speed_Timestamp)

car1.order = car1[order(car1$source_Vehicle_Speed_Timestamp),]
View(car1.order)

# New thought
# Each car has clear journeys
# Noticable by seeing speed at 0 and then next measurement being significantly later in time
# Perhaps cut each car up into journeys
# Then what to do with the journeys?
# These journeys now have more consistent time stamps
# Can hopefully do some analysis on these journeys
# Perhaps calculate mean of each, standard deviation of those means
# Wants us to consider odometer readings for journey starts and ends
# Also wants some thought towards hard accel decel situations, marked as +-9.5kph changes (5.9 mph changes)

# Steps to accomplish the above
# Cut dataframe for each car into sections denoted as journeys
# Create a column variable indicating hard accel decel
# Decide how to interpret the journeys

head(car1.order$source_Vehicle_IgnitionState_Value)
car1.order$source_Vehicle_IgnitionState_Value[1:100]
zeroSpeed = car1.order[car1.order$source_Vehicle_Speed_Value == 0,]
head(zeroSpeed)
head(zeroSpeed$source_Vehicle_IgnitionState_Value)
length((zeroSpeed$source_Vehicle_IgnitionState_Value))

sum(as.numeric(zeroSpeed$source_Vehicle_IgnitionState_Value == FALSE))  # Number of times we are parked/ off
sum(as.numeric(zeroSpeed$source_Vehicle_IgnitionState_Value == TRUE))   # Number of times we are idling (may count 1 idle multiple times)

i = 1
j = 1
journey = c()
for(i in 1:length(car1.order$index)){
  if(car1.order$source_Vehicle_IgnitionState_Value[i] == FALSE){
    journey[j] = i
    if(j >= 2){
      if(journey[j] - journey[j-1] < 10){
        j = j - 1
      }
      
    }
    
    j = j + 1
    
  }
  
}
journey # End points of each trip for car1
View(car1.order)
indivJournies = 0
indivJournies = list( car1.order[1:journey[1],])

for( k in 2:length(journey)){
  indivJournies[[k]] = car1.order[journey[k-1]+1:journey[k],]
}

# ELO Rating Code

# Setting starting ELOs for later
ELO.B_initial = c(1000)
ELO.A_initial = 1200

# 3 Functions
# Will take the trips for car1 for now but will need to generalize to all cars in the dataset.

# Function to subset each trip to be used as an argument in tripScore
# Takes the endpoints from journey calculated above and the dataset of the car
# Subsets all our data for a that trip into a single dataframe
# Puts all the trips together as a list of dataframes
# Returns that list of dataframes

tripSetter <- function(endPnts, data){
  trips = list()
  trips = list(trips)
  startPnt = 1
  for(i in 1:length(endPnts)){
    trips[[i]] <- c(trip[[i]], data[startPnt:endPnts[i], ]) # Not saving properly.  Trying to get dataframe saved to list element
    startPnt = endPnts[i] + 1 # Update new starting point
  }
  View(trips[[1]])
  return(trips)
}

tripSetter(journey, car1.order)

# Function to Calculate the score for a single trip for a single car
# Returns 1 if not many mistakes M, 0.5 if committed M mistakes, 0 if too many mistakes M
# No idea how to generalize to all cars right now.  Will probably need to make a function to make that general too.  For now code jsut for car 1

View(car1.order[1:82,])

# Testing tripScore Function
triplist <- list()
triplist <- list(triplist)
triplist
triplist[[1]] <- c(triplist[[1]], car1.order[1:82,])

View(triplist)
View(car1.order[1:82,])

triplist_1 <- triplist[1]
View(triplist_1)
tripScore <- function(trips){
  carTripScores = c()
  # M Mistakes with limit of 5 mistakes before you failed trip
  M = 0
  MLim = 5
  
  # Limits for what makes a bad driver.  Very iffy.  Will need to change to make more realistic
  speedLim = 62
  timesIdle = 0
  idleLim = 10
  AccelLim = 3
  DecelLim = 3
  for(i in 1:length(trips)){
    tripDF = trips[[i]]
    for(j in 1:length(tripDF$index)){
      if(as.numeric(tripDF$source_Vehicle_Speed_Value[j]) > speedLim){ # Speed Check
        M = M + 1
      } 
      if(as.numeric(tripDF$source_Vehicle_Speed_Value[j]) == 0){ # Idle Check
        timesIdle = timesIdle + 1
        if(timesIdle > idleLim){
          M = M + 1
        }
      }
      if(as.numeric(tripDF$source_Vehicle_Acceleration_Value[j]) > 3){ # Rapid Acceleration Check
        M = M + 1
      }
      if(as.numeric(tripDF$source_Vehicle_Deceleration_Value[j]) > 3){ # Hard Brake Check
        M = M + 1
      }
    }
    # Checking mistakes for tripDF[i], Assign trip result
    if(M > MLim){ # Too many mistakes
      carTripScores[i] <- 0 
    }else if(M == MLim){ # Could have been better 
      carTripScores[i] <- 0.5
    }else { # Good trip
      carTripScores[i] <- 1
    }
  }
  return(carTripScores) # Return Trip results for given car
  }
  
carTripScores = tripScore(triplist)
carTripScores

# ELO Update Function:
# Takes the ELO of the driver, the population, the trip score calculated above, and a weight factor
# Weight factor set at 32 (Arbitrary for now)
  
UpdateELO <- function(ELO.A, ELO.B, TripScore){
  K = 32 # In example I looked at they set K = 32 for some reason.  Don't know enough to argue
  ELO.Pop = (sum(ELO.B))/(length(ELO.B))
  for(i in 1:length(TripScore)){ # For Each trip for a given car
    exp.TS = (1 / (1 + 10^((ELO.Pop - ELO.A)/400))) # Calculate the expected value of the trip score
    ELO.A = ELO.A + K*( TripScore[i] - exp.TS) # Updating ELO of the car each time
    ELO.Pop = (ELO.A + sum(ELO.Pop))/(length(ELO.Pop) + 1) # Updating the ELO of the population of drivers
  }
  
  ELO_FinalScores = c(ELO.A, ELO.Pop)
  resultNames = c("ELO of Driver", "ELO of Population")
  results = rbind(resultNames, ELO_FinalScores)
  return(results)
}
  
results = UpdateELO(ELO.A_initial, ELO.B_initial, carTripScores)  
results

