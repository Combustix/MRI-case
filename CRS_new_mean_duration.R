library(dplyr)
library(data.table)
library(lubridate)

#Parameters for discrete event simulation
n <- 100 #number of times to run
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
mean_duration1

#Patient type 2

#Parameters for patient distribution (estimated to be Normally distributed)
#Estimates are based on time in hours
mean2 <- 0.866542
sd2 <- 0.3101457
work_hours2 <- 9  #Number of working hours (8:00 AM to 5:00 PM)

#Distribution for duration of appointments of type 2 (estimated to be Gamma)
gshape <- 12.5784
grate <- 18.79413  
#Note that the mean of a gamma distribution is given by shape/rate
#Mean duration in minutes (rounded up) for patients of this type
mean_duration2 <- ceiling((gshape/grate) * 60)
mean_duration2

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

#Function to run the simulation once using a given time slot durations
DES <- function(time_slot1, time_slot2) {
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
  
  patient_data2 <- data.frame(Call_Time = numeric(0), Duration = numeric(0), Type = numeric(0))
  current_time2 <- 0
  
  #Simulate patient calls until the end of the working day
  while (current_time2 < work_hours2) {
    #Generates the time until the next call using the normal distribution
    #Since the normal distribution can generate negative numbers, we take a minimum of 0
    interarrival_time2 <- max(0, rnorm(1, mean2, sd2))
    current_time2 <- current_time2 + interarrival_time2
    
    #Check if the next call arrives within working hours
    if (current_time2 < work_hours2) {
      duration2 <- ceiling(60 * rgamma(1, gshape, grate)) #Generates the appointment duration
      
      #Format the current time to display as "hh:mm"
      formatted_time2 <- format(as.POSIXct(current_time2 * 3600, origin = "1970-01-01"), format = "%H:%M")
      patient_data2 <- rbind(patient_data2, data.frame(Call_Time = formatted_time2, Duration = duration2, Type = 2))
    }
  }
  
  #Add 7 hours to the Call_Time column (as the calls are now in the timeframe 1:00-10:00)
  patient_data2 <- patient_data2 %>%
    mutate(Call_Time = as.integer(as.POSIXct(Call_Time, format = "%H:%M") + hours(7))) %>%
    mutate(Call_Time = strftime(as.POSIXct(Call_Time, origin = "1970-01-01"), format = "%H:%M"))
  
  
  
  #Merge the patient data lists ordered in time
  patient_data_comb <- rbind(patient_data, patient_data2)
  patient_data_comb <- patient_data_comb[order(patient_data_comb$Call_Time), ]
  
  #add numbers for each patient
  patient_data_comb$Patient <- 1:nrow(patient_data_comb)
  
  #For the machines
  time_slot_duration_type1 <- time_slot1
  time_slot_duration_type2 <- time_slot2
  
  start_time <- as.POSIXct("08:00:00", format = "%H:%M:%S")
  end_time <- as.POSIXct("21:00:00", format = "%H:%M:%S") #Make sure that end time is late enough so all appointments can fit
  time_sequence <- seq(start_time, end_time, by = 60)
  
  #Creates an empty data frame with time and patient columns
  schedule <- data.frame(Time = format(time_sequence, format = "%H:%M"), Patient = NA)
  schedule2 <- data.frame(Time = format(time_sequence, format = "%H:%M"), Patient = NA)
  
  
  #Assigns patients to schedule with desired timeslots in schedule
    for (i in 1:nrow(patient_data_comb)) {
      patient <- patient_data_comb$Patient[i]
      available_slots <- which(is.na(schedule$Patient))
      available_slots2 <- which(is.na(schedule2$Patient))
      
      for (j in available_slots) {
        if(length(available_slots2) < length(available_slots)){
          if(patient_data_comb$Type[i] == 1) {
            schedule$Patient[j:(j)] <- patient
            schedule <- schedule[-c((j+1):(j+time_slot_duration_type1-1)),]
          }
          else {
            schedule$Patient[j:(j)] <- patient
            schedule <- schedule[-c((j+1):(j+time_slot_duration_type2-1)),]
          }
          break
        }
        else{
          if(patient_data_comb$Type[i] == 1) {
            schedule2$Patient[j:(j)] <- patient
            schedule2 <- schedule2[-c((j+1):(j+time_slot_duration_type1-1)),]
          }
          else {
            schedule2$Patient[j:(j+time_slot_duration_type2-1)] <- patient
            schedule2 <- schedule2[-c((j+1):(j+time_slot_duration_type2-1)),]
          }
          break
        }
      }
    }
    
  schedule <- na.omit(schedule)
  schedule2 <- na.omit(schedule2)
  
  #Add column with actual duration for each appointment
  for(i in 1:nrow(schedule)) {
    schedule$Duration[i] <- patient_data_comb$Duration[schedule$Patient[i]]
  }
  
  for(i in 1:nrow(schedule2)) {
    schedule2$Duration[i] <- patient_data_comb$Duration[schedule2$Patient[i]]
  }
  
  #For Machine 1
  #Creates an empty data frame with time and patient columns by the minute
  time <- seq(start_time, end_time, by = "1 min")
  realised <- data.frame(Time = format(time, format = "%H:%M"), Patient = NA)
  list <- assign_appointments(realised, schedule)
  names(list) <- c("realised", "current_time", "waiting_time")
  realised <- list$realised
  
  #For Machine 2
  #Creates an empty data frame with time and patient columns by the minute
  time <- seq(start_time, end_time, by = "1 min")
  realised2 <- data.frame(Time = format(time, format = "%H:%M"), Patient = NA)
  list2 <- assign_appointments(realised2, schedule2)
  names(list2) <- c("realised", "current_time", "waiting_time")
  realised2 <- list2$realised
  
  
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
  
  
  #Some statistics Machine 2
  
  #idle time
  current_time2 <- list2$current_time
  end_time2 <- which(realised2$Time == current_time2) -1
  idle_time2 <- sum(rowSums(is.na(realised2[1:end_time2,])))
  
  #waiting time
  waiting_time2 <- list2$waiting_time
  waiting_time2 <- sum(list2$waiting_time)
  
  #overtime
  over_time2 <- max(0,end_time2 - 541)
  
  #number of patients scheduled
  number_patients2 <- nrow(schedule2)
  
  total_idle_time <- idle_time + idle_time2
  total_waiting_time <- waiting_time + waiting_time2
  total_over_time <- over_time + over_time2
  total_number_patients <- number_patients + number_patients2
  
  list_results <- list(total_idle_time, total_waiting_time, total_over_time, total_number_patients)
  names(list_results) <- c("idle_time", "waiting_time", "overtime", "patients")
  return(list_results)
}


sequence <- c(-5, -3, -1, 0, 1, 3, 5)
average_stats <- data.frame(idle_time = numeric(0), waiting_time = numeric(0), overtime = numeric(0), patients = numeric(0))
#Compute average performance stats over a some combinations of time slot durations centered on the means
for(i in sequence) {
  for(j in sequence) {
    stats <- data.frame(idle_time = numeric(0), waiting_time = numeric(0), overtime = numeric(0), patients = numeric(0))
    for(k in 0:n-1) {
      set.seed(seed + k)
      stats <- rbind(stats, DES(mean_duration1 + i, mean_duration2 + j))
    }
    sum_list <- c(sum(stats$idle_time)/n, sum(stats$waiting_time)/n, sum(stats$overtime)/n, sum(stats$patients)/n)
    names(sum_list) <- c("idle_time", "waiting_time", "overtime", "patients")
    average_stats <- rbind(average_stats, sum_list)
  }
}
namelist <- c("(mean-5, mean-5)", "(mean-5, mean-3)", "(mean-5, mean-1)", "(mean-5, mean)", "(mean-5, mean+1)", "(mean-5, mean+3)", "(mean-5, mean+5)", 
              "(mean-3, mean-5)", "(mean-3, mean-3)", "(mean-3, mean-1)", "(mean-3, mean)", "(mean-3, mean+1)", "(mean-3, mean+3)", "(mean-3, mean+5)", 
              "(mean-1, mean-5)", "(mean-1, mean-3)", "(mean-1, mean-1)", "(mean-1, mean)", "(mean-1, mean+1)", "(mean-1, mean+3)", "(mean-1, mean+5)", 
              "(mean, mean-5)", "(mean, mean-3)", "(mean, mean-1)", "(mean, mean)", "(mean, mean+1)", "(mean, mean+3)", "(mean, mean+5)", 
              "(mean+1, mean-5)", "(mean+1, mean-3)", "(mean+1, mean-1)", "(mean+1, mean)", "(mean+1, mean+1)", "(mean+1, mean+3)", "(mean+1, mean+5)", 
              "(mean+3, mean-5)", "(mean+3, mean-3)", "(mean+3, mean-1)", "(mean+3, mean)", "(mean+3, mean+1)", "(mean+3, mean+3)", "(mean+3, mean+5)", 
              "(mean+5, mean-5)", "(mean+5, mean-3)", "(mean+5, mean-1)", "(mean+5, mean)", "(mean+5, mean+1)", "(mean+5, mean+3)", "(mean+5, mean+5)")

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
