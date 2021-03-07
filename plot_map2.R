# Define libraries
library("ggplot2")
library("usmap")
library("tibble")

# Create map of initial state
# using infection data from 7/1/2020
# where infection/population > 1%
state_data_initial <- tibble(fips = c("01", "04", "05", "06", "08", "09", "10",
                              "12", "13", "16", "17", "18", "19", "20", 
                              "21", "22", "23", "24", "25", "26", "27",
                              "28", "29", "30", "31", "32", "33", "34",
                              "35", "36", "37", "38", "39", "40", "41",
                              "42", "44", "45", "46", "47", "48", "49",
                              "50", "51", "53", "54", "55", "56"),
                             trending = c(0, 1, 0, 0, 0, 1, 1,
                                          0, 0, 0, 1, 0, 0, 0,
                                          0, 1, 0, 1, 1, 0, 0,
                                          0, 0, 0, 1, 0, 0, 1,
                                          0, 1, 0, 0, 0, 0, 0,
                                          0, 1, 0, 0, 0, 0, 0,
                                          0, 0, 0, 0, 0, 0))
plot_usmap(
  regions = "states", 
  data = state_data_initial,
  values = "trending",
  exclude = c("02", "15", "60", "66", "69", "72", "78"), 
  color = "black") +
  scale_fill_continuous(low = "white", high = "red") +
  labs(title = "COVID") +
  theme(legend.position = "right")

# Create map of target state
# using infection data from 8/1/2020
# where infection/population > 1%
state_data_target <- tibble(fips = c("01", "04", "05", "06", "08", "09", "10",
                                      "12", "13", "16", "17", "18", "19", "20", 
                                      "21", "22", "23", "24", "25", "26", "27",
                                      "28", "29", "30", "31", "32", "33", "34",
                                      "35", "36", "37", "38", "39", "40", "41",
                                      "42", "44", "45", "46", "47", "48", "49",
                                      "50", "51", "53", "54", "55", "56"),
                            trending = c(1, 1, 1, 1, 0, 1, 1,
                                         1, 1, 1, 1, 1, 1, 0,
                                         0, 1, 0, 1, 1, 0, 1,
                                         1, 0, 0, 1, 1, 0, 1,
                                         1, 1, 1, 0, 0, 1, 0,
                                         0, 1, 1, 1, 1, 1, 1,
                                         0, 1, 0, 0, 1, 0))

plot_usmap(
  regions = "states", 
  data = state_data_target,
  values = "trending",
  exclude = c("02", "15", "60", "66", "69", "72", "78"), 
  color = "black") +
  scale_fill_continuous(low = "white", high = "red") +
  labs(title = "COVID") +
  theme(legend.position = "right")

# Create map of target state
# using infection data from 8/1/2020
# where infection/population > 1%