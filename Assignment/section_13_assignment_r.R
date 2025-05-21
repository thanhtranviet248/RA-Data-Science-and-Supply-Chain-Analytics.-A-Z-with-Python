# Import packages
library(queuecomputer)

# CASE 1: SINGLE STAGE

arrival_rate <- 20/60
service_time_mean <- 10

arrival_time <- cumsum(x=rexp(n=2000, rate=arrival_rate))
service_time <- rexp(n=2000, rate=1/service_time_mean)

mean_waiting_time_list = list()
for (i in 1:20) {
  simulation <- queue_step(arrivals=arrival_time, service=service_time, servers=i)
  mean_waiting_time <- mean(unlist(simulation$departures_df['waiting']), na.rm=TRUE)
  mean_waiting_time_list <- append(mean_waiting_time_list, mean_waiting_time)
}

plot(x = 1:20, y = mean_waiting_time_list, type = 'l')
list(mapply(c, 1:20, mean_waiting_time_list, SIMPLIFY = TRUE))

# CASE 2: MULTIPLE STAGES

arrival_registration <- cumsum(x=rexp(n=5000, rate=150/60))
service_registration <- rexp(n=5000, rate=1/0.5)

simulation_registration <- queue_step(arrivals=arrival_registration, service=service_registration, servers=2)

departure <- simulation_registration$departures_df['departures']

arrival_teller <- unlist(departure)[runif(5000) <= 0.55]
service_teller <- rnorm(n=length(arrival_teller), mean=11, sd=3)

arrival_cs <- unlist(departure)[runif(5000) > 0.55]
service_cs <- rexp(n=length(arrival_cs), rate=1/12)

simulation_teller <- queue_step(arrivals=arrival_teller, service=service_teller, servers=15)
simulation_cs <- queue_step(arrivals=arrival_cs, service=service_cs, servers=15)

waiting_registration <- colMeans(simulation_registration$departures_df['waiting'], na.rm=TRUE)
waiting_teller <- colMeans(simulation_teller$departures_df['waiting'], na.rm=TRUE)
waiting_cs <- colMeans(simulation_cs$departures_df['waiting'], na.rm=TRUE)

system_mean_waiting_time <- waiting_registration + (waiting_teller + waiting_cs)/2
print(system_mean_waiting_time)