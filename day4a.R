file_prefix <- c("temp", "ph", "salinity")
file_suffix <- c(1,2,3,4)

for (i in 1:length(file_prefix)){
  for (j in 1:length(file_suffix)){
    print(paste0(file_prefix[i], "_", file_suffix[j]))
  }
  }

odds <- c(1,3,5)
evens <- c(2,4,6,8)

for (i in seq_along(odds)) {
  for (j in seq_along(evens)) {
    print(odds[i] * evens[j])
  }
}

birddog_sum <- function(bird, dog) {
  pets <- bird + dog
  return(pets)
}

x <- birddog_sum(bird = 2, dog = 5)


double_it <- function(x) {
  print(2*x)
}
double_it(4)
double_it(1:4)


exclaim_age <- function(age) {
  print(paste("I am", age, "years old"))
}
exclaim_age(age = 10)

find_max <- function(val1, val2) {
  if (val1 > val2) {
    return(val1)
  } else if (val2 > val1) {
    return((val2))
  }
}
5 * find_max(7,3)

quarter_splits <- c(1.0, 1.1, 1.2, 1.1, 1.4, 1.5, 1.6, 1.4)

for (i in seq_along(quarter_splits)) {
  print(quarter_splits[i] + quarter_splits[i + 1])
}

animal_age <- function(animal, age) {
  if (animal == "dog") {
    print(age *7)
  } else if (animal == "goat") {
    print(age *4.7)
  }
}

animal_age(animal = "dog", age = 8)
animal_age(animal = "cow", age = 2)
animal_age(animal = "dog", age = "yellow")

dog_choice <- data.frame(dog_name = c("Khora",
                                     "Teddy",
                                     "Waffle",
                                     "Banjo"),
                         food = c("everything", 
                                  "salmon",
                                  "pancakes",
                                  "chicken"))

library(tidyverse)

dog_menu <- function(name) {
  my_sub <- dog_choice %>% dplyr::filter(dog_name == name)
  print(paste("My name is", my_sub$dog_name, "and I like to eat", my_sub$food))
}

dog_menu("Khora")
dog_menu("Teddy")
dog_menu("Waffle")
dog_menu("Banjo")


animal_age <- function(animal, age) {
  
  if(!animal %in% c("dog", "goat")) {
    stop("Oops! Animal must be a dog or a goat")
  }
  if (is.numeric(age) == FALSE) {
    stop("The age must be a number greater than 0")
  }
  
  if (age <= 0) {
    stop("Age must be greater than 0")
  }
  
    if (animal == "dog") {
      print(age *7)
    } else if (animal == "goat") {
      print(age *4.7)
    }
  }

animal_age(animal="dog", age = "yellow")


calc_windpower <- function(rho, radius, windspeed) {
  
  if(windspeed > 130) {
    warning("Wow, that's fast! Are you sure?")
  }
    if(rho > 1.225) {
      warning("That air density is suspicious, are you sure?")
    }
  if(radius < 0) {
    stop("Rotor radius must be a positive value (meters).")
  }
 print(0.3*rho*pi*(radius^2)*(windspeed^3))
}

calc_windpower(2, -1, 50)


gw_rate <- function(site) {
  
  if(!site %in% c("mountain", "prairie", "desert", "beach")) {
    warning("site not included")
  }
  
  gw_depths <- data.frame(sitename= c("mountain",
                                      "prairie",
                                      "desert",
                                      "beach"),
                          depth = c(32, 41, 63, 2),
                          slope = c(11.2, 0.4, 0.8, 2.6))
  
  site_select <- filter(gw_depths, sitename == site)
  
  transport_rate <- 1.4 * site_select$slope + 3.6 * site_select$depth
  
  return(transport_rate)
  
}

gw_rate(site = "beach")


# Matrix practice- PM

rm(list=ls())

logistic_growth <- function(N0, K, r, time ) {
  Nt <- K / (1 +((K -N0)/ N0) * exp(-r * time))
  return(Nt)
}

logistic_growth(N0 = 100, K = 6000, r = 0.27, time = 40)

time_vec <- seq(from = 0, to = 50, by = 0.1)

pop_1 <- logistic_growth(N0 = 100, K = 6000, r = 0.27, time = time_vec)

pop_1_vec <- vector(mode="numeric", length = length(time_vec))


for (i in seq_along(r_seq)) { # outeloop of growth rates
  for (j in seq_along(time_vec)) { #Inner loop time steps
    population <- logistic_growth(N0 = 100, K = 6000, r = r_seq[i], time = time_vec[j])
    out_matrix[j, i] <- population
  }
  
}
for (i in seq_along(time_vec)) {
  population <- logistic_growth(N0 = 100, K = 6000, r = 0.27, time = time_vec[i])
  pop_1_vec[i] <- population
}

pop_time_1 <- data.frame(time_vec, pop_1_vec)

ggplot(data=pop_time_1, aes(x = time_vec, y = pop_1_vec)) +
  geom_line()

# create sequence of growth rates

r_seq <- seq(from = 0.2, to = 0.4, by = 0.01)

out_matrix <- matrix(nrow = length(time_vec), ncol = length(r_seq))

out_df <- data.frame(out_matrix, time = time_vec)

colnames(out_df) <- c(paste0("growth_rate_", r_seq), "time")

out_df_long <- out_df %>%
  pivot_longer(cols = -time, names_to = "growth_rate", values_to = "population_size")

ggplot(data= out_df_long, aes(x= time, y = population_size)) +
  geom_line(aes(color = growth_rate), show.legend = FALSE) +
  theme_minimal()

