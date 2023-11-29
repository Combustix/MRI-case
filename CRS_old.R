
#Define the working hours and time slot duration
start_time <- 8  
end_time <- 17   
time_slot_duration <- 30
working_hours <- end_time - start_time  

#Simulate patient arrivals for the next day (poisson distribution)
lambda <- 1 # Average arrival rate per hour

#Simulate patient arrivals for the next day within working hours
num_appointments <- sum(rpois(working_hours , lambda))

#Simulate appointment durations (normal distribution in minutes)
mean <- 30  
sd <- 5     

appointment_durations <- rnorm(num_appointments, mean, sd)

appointment_durations <- ceiling(appointment_durations) #rounds appointment durations up to the nearest integer for convenience of planning

#Create a data frame of appointments that need to be assigned for next day
patient_data <- data.frame(
  Patient = 1:num_appointments,
  Duration = appointment_durations
)

start_time <- as.POSIXct("08:00:00", format = "%H:%M:%S")
end_time <- as.POSIXct("21:00:00", format = "%H:%M:%S") #Make sure that end time is late enough so all appointments can fit
time_sequence <- seq(start_time, end_time, by = time_slot_duration * 60)

#Creates an empty data frame with time and patient columns
schedule <- data.frame(Time = format(time_sequence, format = "%H:%M"), Patient = NA)

#Function that assigns patients to schedule with desired timeslots
  assign_appointments <- function(schedule, patient_data) {
    for (i in 1:nrow(patient_data)) {
      patient <- patient_data$Patient[i]
      available_slots <- which(is.na(schedule$Patient))
      
      for (j in available_slots) { 
          schedule$Patient[j:(j)] <- patient
          break
      }
    }
    return(schedule)
  }

# Assign appointments to schedule
schedule <- na.omit(assign_appointments(schedule, patient_data))
patient_data$Time <- schedule$Time


#Creates an empty data frame with time and patient columns by the minute
time <- seq(start_time, end_time, by = "1 min")
realised <- data.frame(Time = format(time, format = "%H:%M"), Patient = NA)

#Function that will depict what will happen when the schedule is actually followed
assign_appointments <- function(realised, patient_data) {
  
  waiting_time <- array(0, dim = c(1,nrow(patient_data))) #Array to keep track of the waiting time
  
  #First patient that calls will always be scheduled at 8:00
  current_time <- "8:00"
  patient <- patient_data$Patient[1]
  duration <- patient_data$Duration[1]
  time_row <- which(realised$Time == patient_data$Time[1])
  realised$Patient[time_row:(time_row + duration -1)] <- patient
  current_time <- realised$Time[time_row + duration]
  
  for (i in 2:nrow(patient_data)) {
    
    if(current_time <= patient_data$Time[i]){ #If the previous patient finishes before the next patient arrives the next patient does not have waiting time
      patient <- patient_data$Patient[i]
      duration <- patient_data$Duration[i]
      time_row <- which(realised$Time == patient_data$Time[i])
      realised$Patient[time_row:(time_row + duration -1)] <- patient
      
      current_time <- realised$Time[time_row + duration] #updates the time with when the patient finishes
    }else{ #Else the next patient has to wait until the previous patient finishes
      patient <- patient_data$Patient[i]
      duration <- patient_data$Duration[i]
      current_time_row <- which(realised$Time == current_time)
      realised$Patient[current_time_row:(current_time_row + duration -1)] <- patient
      
      #fill in waiting time for next patient into the array
      time_row <- which(realised$Time == patient_data$Time[i])
      waiting_time[i] <- current_time_row - time_row
      current_time <- realised$Time[current_time_row + duration] #updates the time with when the patient finishes
    }
  }
  return(list(realised, current_time, waiting_time))
}
list <- assign_appointments(realised, patient_data)
names(list) <- c("realised", "current_time", "waiting_time")
realised <- list$realised
  

#Some statistics

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
number_patients <- nrow(patient_data)

