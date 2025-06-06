# Import packages
install.packages('queuecomputer')
library(queuecomputer)

#---------------------------------------------------------------------
# CASE 1: SINGLE SERVER

# Generate arrival time and service time
arrival_time <- cumsum(x=rexp(n=400, rate=1))
service_time <- rnorm(n=400, mean=1, sd=0.2)

# Simulation the queue with 1 server
simulation = queue_step(arrivals = arrival_time, service = service_time, servers = 1)

# Print results
simulation$departures_df

# Calculate mean waiting time
mean_waiting_time <- colMeans(simulation$departures_df['waiting'], na.rm = TRUE)
mean_waiting_time

# Run the simulation for 1000 times
waiting_time_1000 <- list()
for (i in 1:1000) {
  arrival_time <- cumsum(x=rexp(n=400, rate=1))
  service_time <- rnorm(n=400, mean=1, sd=0.2)
  simulation = queue_step(arrivals=arrival_time, service=service_time, servers=1)
  mean_waiting_time <- colMeans(simulation$departures_df['waiting'], na.rm=TRUE)
  waiting_time_1000 <- append(average_sim_1000, mean_waiting_time)
}

# Plot the histogram of the mean waiting time in 1000 runs
hist(unlist(waiting_time_1000))

# Mean and median of the mean waiting time in 1000 runs
mean(unlist(waiting_time_1000))
median(unlist(waiting_time_1000))

#---------------------------------------------------------------------
# CASE 2: MULTIPLE SERVERS

# Generate arrival time and service time
arrival_time = cumsum(x=rexp(n=1000, rate=40/60))
service_time = rexp(n=1000, rate=1/7)

# Simulate the queue with 5 servers
simulation = queue_step(arrivals=arrival_time, service=service_time, servers=5)

# Calculate mean waiting time
mean_waiting_time <- colMeans(simulation$departures_df['waiting'], na.rm=TRUE)
mean_waiting_time

# Loop from 1 to 9 servers to find the best option
n_servers <- 1:9
waiting_time_list <- list()

for (i in n_servers) {
  simulation <- queue_step(arrivals=arrival_time, service=service_time, servers=i)
  mean_waiting_time <- colMeans(simulation$departures_df['waiting'], na.rm=TRUE)
  waiting_time_list <- append(waiting_list, mean_waiting_time)
}

# Plot and print results
plot(x = n_servers, y = waiting_list, type = 'l')
list(mapply(c, n_servers, waiting_list, SIMPLIFY = TRUE))

#---------------------------------------------------------------------
# CASE 3: MULTIPLE SERVERS AND STAGES

# Generate arrival time and service time of the registration stage
arrival_registration <- cumsum(x=rexp(n=1000, rate=150/60))
service_registration <- rexp(n=1000, rate=1/0.5)

# Simulate the queue at the registration stage
simulation_registration <- queue_step(arrivals=arrival_registration, service=service_registration, servers=1)

# Get the departure time from registration stage
departure <- simulation_registration$departures_df['departures']

# Generate arrival time and service time at the teller service
arrival_teller <- unlist(departure)[runif(1000) <= 0.65]
service_teller <- rexp(n=length(arrival_teller), rate=1/10)

# Generate arrival time and service time at the cs service
arrival_cs <- unlist(departure)[runif(1000) > 0.65]
service_cs <- rexp(n=length(arrival_cs), rate=1/13)

# Simulate the queue at the teller and cs services
simulation_teller <- queue_step(arrivals=arrival_teller, service=service_teller, servers=15)
simulation_cs <- queue_step(arrivals=arrival_cs, service=service_cs, servers=15)

# Calculate the mean waiting time for the whole system
waiting_registration <- colMeans(simulation_registration$departures_df['waiting'], na.rm=TRUE)
waiting_teller <- colMeans(simulation_teller$departures_df['waiting'], na.rm=TRUE)
waiting_cs <- colMeans(simulation_cs$departures_df['waiting'], na.rm= RUE)

system_mean_waiting_time <- waiting_registration + (waiting_teller + waiting_cs)/2
print(system_mean_waiting_time)











