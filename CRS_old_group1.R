library(dplyr)
library(data.table)
library(lubridate)

#Parameters for discrete event simulation
n <- 1000 #number of times to run
seed = 289674356 #seed to be used for first run and then incremented for later runs, can effectively be any random number
work_hours <- 9  #Number of working hours (8:00 AM to 5:00 PM)

#Patient type 1
#We use the estimated parameters from the statistical analysis (given in)
#Estimated parameters for patient distribution (in this case poisson with exponential distribution inbetween events)
lambda <- 1.833212

#Distribution for duration of appointment (normal in this case)
mean <- 0.4326699
sd <- 0.09764559
#Mean duration in minutes (rounded up) for patients of this type
mean_duration1 <- ceiling(mean * 60)

#Function that will depict what will happen when the schedule is actually followed
assign_appointments <- function(realised, schedule) {
  
  waiting_time <- array(0, dim = c(1,nrow(schedule))) #Array to keep track of the waiting time
  
  #First patient that calls will always be scheduled at 8:00
  current_time <- "8:00"
  patient <- schedule$Patient[1]
  duration <- schedule$Duration[1]
  time_row <- which(realised$Time == schedule$Time[1])
  realised$Patient[time_row:(time_row + duration -1)] <- patient
  current_time <- realised$Time[time_row + duration]
  
  for (i in 2:nrow(schedule)) {
    
    if(current_time <= schedule$Time[i]){ #If the previous patient finishes before the next patient arrives the next patient does not have waiting time
      patient <- schedule$Patient[i]
      duration <- schedule$Duration[i]
      time_row <- which(realised$Time == schedule$Time[i])
      realised$Patient[time_row:(time_row + duration -1)] <- patient
      
      current_time <- realised$Time[time_row + duration] #updates the time with when the patient finishes
    }else{ #Else the next patient has to wait until the previous patient finishes
      patient <- schedule$Patient[i]
      duration <- schedule$Duration[i]
      current_time_row <- which(realised$Time == current_time)
      realised$Patient[current_time_row:(current_time_row + duration -1)] <- patient
      
      #fill in waiting time for next patient into the array
      time_row <- which(realised$Time == schedule$Time[i])
      waiting_time[i] <- current_time_row - time_row
      current_time <- realised$Time[current_time_row + duration] #updates the time with when the patient finishes
    }
  }
  return(list(realised, current_time, waiting_time))
}

#Function to generate the patient data
Generate_Patients <- function() {
  patient_data <- data.frame(Call_Time = numeric(0), duration = numeric(0), Type = numeric(0))
  current_time <- 0
  
  #Simulate patient calls until the end of the working day
  while (current_time < work_hours) {
    #Generates the time until the next call using the exponential distribution
    interarrival_time <- rexp(1, rate = lambda)
    current_time <- current_time + interarrival_time
    
    #Check if the next call arrives within working hours
    if (current_time < work_hours) {
      duration <- ceiling(60 * rnorm(1, mean,sd)) #Generates the appointment duration
      
      #Format the current time to display as "hh:mm"
      formatted_time <- format(as.POSIXct(current_time * 3600, origin = "1970-01-01"), format = "%H:%M")
      patient_data <- rbind(patient_data, data.frame(Call_Time = formatted_time, Duration = duration, Type = 1))
    }
  }
  
  #Add 7 hours to the Call_Time column (as the calls are now in the timeframe 1:00-10:00)
  patient_data <- patient_data %>%
    mutate(Call_Time = as.integer(as.POSIXct(Call_Time, format = "%H:%M") + hours(7))) %>%
    mutate(Call_Time = strftime(as.POSIXct(Call_Time, origin = "1970-01-01"), format = "%H:%M"))
  
  patient_data$Patient <- 1:nrow(patient_data)
  
  return(patient_data)
}

Scheduling <- function(patient_data, time_slot) {  
  #For the machines
  time_slot_duration <- time_slot
  
  start_time <- as.POSIXct("08:00:00", format = "%H:%M:%S")
  end_time <- as.POSIXct("23:59:00", format = "%H:%M:%S") #Make sure that end time is late enough so all appointments can fit
  time_sequence <- seq(start_time, end_time, by = 60 * time_slot_duration)
  
  #Creates an empty data frame with time and patient columns
  schedule <- data.frame(Time = format(time_sequence, format = "%H:%M"), Patient = NA)
  
  #Assigns patients to schedule with desired timeslots in schedule
  for (i in 1:nrow(patient_data)) {
    patient <- patient_data$Patient[i]
    available_slots <- which(is.na(schedule$Patient))
    schedule$Patient[available_slots[1]] <- patient
  }
  
  schedule <- na.omit(schedule)
  
  #Add column with actual duration for each appointment
  for(i in 1:nrow(schedule)) {
    schedule$Duration[i] <- patient_data$Duration[schedule$Patient[i]]
  }
  
  #For Machine 1
  #Creates an empty data frame with time and patient columns by the minute
  time <- seq(start_time, end_time, by = "1 min")
  realised <- data.frame(Time = format(time, format = "%H:%M"), Patient = NA)
  list <- assign_appointments(realised, schedule)
  names(list) <- c("realised", "current_time", "waiting_time")
  realised <- list$realised
  
  #Some statistics Machine 1
  
  #idle time
  current_time <- list$current_time
  end_time <- which(realised$Time == current_time) -1
  idle_time <- sum(rowSums(is.na(realised[1:end_time,])))
  
  #waiting time
  waiting_time <- list$waiting_time
  waiting_time <- sum(list$waiting_time)
  
  #overtime
  over_time <- max(0,end_time - 541)
  
  #number of patients scheduled
  number_patients <- nrow(schedule)
  
  list_results <- list(idle_time, waiting_time, over_time, number_patients)
  names(list_results) <- c("idle_time", "waiting_time", "overtime", "patients")
  return(list_results)
}


sequence <- c(-3,-2, -1, 0, 1, 2,3)
average_stats <- data.frame(idle_time = numeric(0), waiting_time = numeric(0), overtime = numeric(0), patients = numeric(0))
#Compute average performance stats over a some combinations of time slot durations centered on the means
stats <- data.frame(mean1_deviation = numeric(0), idle_time = numeric(0), waiting_time = numeric(0), overtime = numeric(0), patients = numeric(0))
for(k in 0:(n-1)) {
  set.seed(seed + k)
  patients <- Generate_Patients()
  for(i in sequence) {
    output <- Scheduling(patients, mean_duration1 + i)
    output <- append(output, i, after=0)
    names(output) <- c("mean1_deviation", "idle_time", "waiting_time", "overtime", "patients")
    stats <- rbind(stats, output)
  }
}

#For each combination of time slots, sum up and average out the stats
for(i in sequence) {
  sum_list <- c(sum(stats[which(stats[,1] == i), 2])/n, sum(stats[which(stats[,1] == i), 3])/n, 
                sum(stats[which(stats[,1] == i), 4])/n, sum(stats[which(stats[,1] == i), 5])/n)
  names(sum_list) <- c("idle_time", "waiting_time", "overtime", "patients")
  average_stats <- rbind(average_stats, sum_list)
}

namelist <- c("mean-3","mean-2", "mean-1", "mean", "mean+1", "mean+2","mean+3")

row.names(average_stats) <- namelist
colnames(average_stats) <- c("Idle Time", "Waiting Time", "Overtime", "Number of Patients")
#With the rows and columns named, we can check the average stats data frame for the results
average_stats

#Extra code to manually check the performance of a combination of time slot lengths
# stats <- data.frame(idle_time = numeric(0), waiting_time = numeric(0), overtime = numeric(0), patients = numeric(0))
# for(k in 0:n-1) {
#   set.seed(seed + k)
#   stats <- rbind(stats, DES(mean_duration1 + 10, mean_duration2 + 10))
# }
# sum_list <- c(sum(stats$idle_time)/n, sum(stats$waiting_time)/n, sum(stats$overtime)/n, sum(stats$patients)/n)
# sum_list
