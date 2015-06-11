# Calculate the number of Sundays that fell on the 1st of the month during the 
# 20th century (1 Jan 1901 to 31 Dec 2000)
# 1 Jan 1900 was a Monday
# Leap years happen if year is divisible by 4, but for a century (e.g. 1900) only 
# if divisble by 400 (on this range of dates, divisible by 4 is acceptable)

days_per_month <- c(31,28,31,30,31,30,31,31,30,31,30,31)
num_months <- 12

# Mark first of month in format 1:365
first_of_month <- rep(0,num_months) 
first_of_month[1] <- 1
for(i in 2:num_months){
  first_of_month[i] <- first_of_month[i-1] + days_per_month[i-1]
}
# Separate list for leap years
first_leap_year <- first_of_month
first_leap_year[3:num_months] <- first_leap_year[3:num_months] + 1

# Date with known weekday
year <- 1900
initial_weekday <- 1 # One = Monday, Seven = Sunday
target_weekday <- 0

# Forward simulate
start_year <- 1901 # Start counting here
end_year <- 2000

target_found <- 0
while(year <= end_year){
  # IS IT A LEAP YEAR?
  is_leap <- 0
  if((year %% 100) == 0){
    if((year %% 400) ==0){
      is_leap <- 1
    }
  }else if((year %% 4) == 0){
    is_leap <- 1
  }
  if(is_leap == 1){
    target_dates <- first_leap_year
    num_days <- 366
  }else{
    target_dates <- first_of_month
    num_days <- 365
  }
  
  # FIND INITIAL TARGET DATES
  if(year >= start_year){
    target_found <- target_found + sum((target_dates + initial_weekday - 1) %% 7 == target_weekday)
  }
  # INCREMENT FOR NEXT YEAR
  initial_weekday <- (initial_weekday + (num_days %% 7)) %% 7
  year <- year + 1
}
print(target_found)
