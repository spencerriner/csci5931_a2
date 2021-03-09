# Define libraries
library("ggplot2")
library("usmap")
library("tibble")
library("foreach")
library("animation")
library("reshape")

library(dplyr)
library(stringr)

states <- c("01", "04", "05", "06", "08", "09", "10",
            "12", "13", "16", "17", "18", "19", "20", 
            "21", "22", "23", "24", "25", "26", "27",
            "28", "29", "30", "31", "32", "33", "34",
            "35", "36", "37", "38", "39", "40", "41",
            "42", "44", "45", "46", "47", "48", "49",
            "50", "51", "53", "54", "55", "56")

initial_data <- c(0, 0, 0, 0, 1, 1, 1,
                  0, 1, 1, 1, 1, 0, 0,
                  0, 1, 0, 1, 1, 1, 0,
                  0, 0, 0, 0, 1, 1, 1,
                  0, 1, 0, 0, 0, 0, 0,
                  1, 1, 0, 0, 1, 0, 0,
                  0, 0, 1, 0, 0, 0)

state_data <- data.frame(fips = states,
                         trend = initial_data
)

# Define state neighbors in comma separated string
neighbor_list <- function(state) {
  case_when(
    state == '01' ~ c('28,47,13,12'),
    state == '04' ~ c('06,32,49,35'),
    state == '05' ~ c('48,40,29,47,28,22'),
    state == '06' ~ c('41,32,04'),
    state == '08' ~ c('49,56,31,20,40,35'),
    state == '09' ~ c('36,25,44'),
    state == '10' ~ c('24,42,34'),
    state == '12' ~ c('01,13'),
    state == '13' ~ c('12,01,47,37,45'),
    state == '16' ~ c('41,53,30,56,49,32'),
    state == '17' ~ c('29,19,27,55,18,21'),
    state == '18' ~ c('21,39,26,17'),
    state == '19' ~ c('29,31,46,27,55,17'),
    state == '20' ~ c('40,08,31,29'),
    state == '21' ~ c('29,17,18,39,54,51,47'),
    state == '22' ~ c('48,05,28'),
    state == '23' ~ c('33'),
    state == '24' ~ c('51,54,42,10'),
    state == '25' ~ c('36,50,33,09,44'),
    state == '26' ~ c('39,18,55'),
    state == '27' ~ c('38,46,19,55') ,
    state == '28' ~ c('22,05,47,01'),
    state == '29' ~ c('05,40,20,31,19,17,21,47'),
    state == '30' ~ c('16,56,46,38'),
    state == '31' ~ c('20,08,56,46,19'),
    state == '32' ~ c('06,41,16,49,04'),
    state == '33' ~ c('50,23,25'),
    state == '34' ~ c('42,36'),
    state == '35' ~ c('04,08,40,48'),
    state == '36' ~ c('50,25,09,34'),
    state == '37' ~ c('45,47,51'),
    state == '38' ~ c('27,46,30'),
    state == '39' ~ c('26,18,21,54,42'),
    state == '40' ~ c('48,35,08,20,29,05'),
    state == '41' ~ c('53,16,32,06'),
    state == '42' ~ c('39,54,24,34,36'),
    state == '44' ~ c('09,25'),
    state == '45' ~ c('37,13'),
    state == '46' ~ c('38,27,19,31,56,30'),
    state == '47' ~ c('13,01,28,05,29,21,51,37'),
    state == '48' ~ c('35,40,28,22'),
    state == '49' ~ c('32,04,08,56,16'),
    state == '50' ~ c('36,25,33'),
    state == '51' ~ c('24,54,37'),
    state == '53' ~ c('41,16'),
    state == '54' ~ c('21,39,42,51'),
    state == '55' ~ c('27,19,17,26'),
    state == '56' ~ c('30,16,49,08,31,46')
  )
}


# Call neighbors, split on comma
str_split(neighbor_list('48'), ",")[[1]]
# Call individual states with array notation
str_split(neighbor_list('48'), ",")[[1]][1]

is_state_alive <- function(st = "01", dat = initial_data){
  
  index <- match(st, states)
  is_alive <- dat[index] == 1
  
}
res <- list()
game_of_life <- function(dat = c(), num_reps = 2){
  
  grid <- list()  
  
  grid[[1]] <- dat   
  dev_null <- foreach(i = seq_len(num_reps) + 1) %do% {     
    grid[[i]] <- grid[[i-1]]
    j <- 1
    for (k in state_data$fips){
      
      # Apply game rules.       
      neighbors <- str_split(neighbor_list(k), ",")
      num_neighbors <- length(neighbors[[1]])
      alive <- grid[[i]]$trend[j] == 1
      num_neighbors_alive <- 0
      
      for (n in neighbors[[1]]){
        temp <- n
        if (is_state_alive(n,grid[[i-1]]$trend)){
          num_neighbors_alive <- num_neighbors_alive + 1
        }
      }
      # Off state rules
      # <2 living neighbors, do nothing
      # 2-4 more living neighbors infects state
      if (!alive && num_neighbors_alive >= 1 && num_neighbors_alive <= 4){
        grid[[i]]$trend[j] <- 1;
      }
      
      ## 5-8 living neighbors leaves state dead (lockdown)

      # On state rules
      # 1-3 living neighbors continues state infection
      
      # 4-8 living neighbors initiates lockdown
      if (alive && num_neighbors_alive >= 4 && num_neighbors_alive <= 8){
        grid[[i]]$trend[j] <- 0;
      }
      
      j <- j + 1
    }  
    
  }   
  grid
  
}

grid_to_usmap_plot <- function(r){
  
  result <- plot_usmap(
    regions = "states", 
    data = r,
    values = "trend",
    exclude = c("02", "15", "60", "66", "69", "72", "78"), 
    color = "black") +
    scale_fill_continuous(low = "white", high = "red", name = "Test", label = scales::comma) +
    labs(title = "COVID") +
    theme(legend.position = "right")
}



res <- game_of_life(dat = state_data, num_reps = 50)

# set clean to FALSE to save individual frames
saveGIF(lapply(lapply(res, grid_to_usmap_plot), print), clean=FALSE)
