library(dplyr)
library(stringr)

# Define state neighbors in comma separated string
neighbor_list <- function(state) {
  case_when(
    state == 'AL' ~ c('MS,TN,GA,FL'),
    state == 'AZ' ~ c('CA,NV,UT,NM'),
    state == 'AR' ~ c('TX,OK,MO,TN,MS,LA'),
    state == 'CA' ~ c('OR,NV,AZ'),
    state == 'CO' ~ c('UT,WY,NE,KS,OK,NM'),
    state == 'CT' ~ c('NY,MA,RI'),
    state == 'DE' ~ c('MD,PA,NJ'),
    state == 'FL' ~ c('AL,GA'),
    state == 'GA' ~ c('FL,AL,TN,NC,SC'),
    state == 'ID' ~ c('OR,WA,MT,WY,UT,NV'),
    state == 'IL' ~ c('MO,IA,MN,WI,IN,KY'),
    state == 'IN' ~ c('KY,OH,MI,IL'),
    state == 'IA' ~ c('MO,NE,SD,MN,WI,IL'),
    state == 'KS' ~ c('OK,CO,NE,MO'),
    state == 'KY' ~ c('MO,IL,IN,OH,WV,VA,TN'),
    state == 'LA' ~ c('TX,AR,MS'),
    state == 'ME' ~ c('NH'),
    state == 'MD' ~ c('VA,WV,PA,DE'),
    state == 'MA' ~ c('NY,VT,NH,CT,RA'),
    state == 'MI' ~ c('OH,IN,WI'),
    state == 'MN' ~ c('ND,SD,IA,WI') ,
    state == 'MS' ~ c('LA,AR,TN,AL'),
    state == 'MO' ~ c('AR,OK,KS,NE,IA,IL,KY,TN'),
    state == 'MT' ~ c('ID,WY,SD,ND'),
    state == 'NE' ~ c('KS,CO,WY,SD,IA'),
    state == 'NV' ~ c('CA,OR,ID,UT,AZ'),
    state == 'NH' ~ c('VT,ME,MA'),
    state == 'NJ' ~ c('PA,NY'),
    state == 'NM' ~ c('AZ,CO,OK,TX'),
    state == 'NY' ~ c('VT,MA,CT,NJ'),
    state == 'NC' ~ c('SC,TN,VA'),
    state == 'ND' ~ c('MN,SD,MT'),
    state == 'OH' ~ c('MI,IN,KY,WV,PA'),
    state == 'OK' ~ c('TX,NM,CO,KS,MO,AR'),
    state == 'OR' ~ c('WA,ID,NV,CA'),
    state == 'PA' ~ c('OH,WV,MD,NJ,NY'),
    state == 'RI' ~ c('CT,MA'),
    state == 'SC' ~ c('NC,GA'),
    state == 'SD' ~ c('ND,MN,IA,NE,WY,MT'),
    state == 'TN' ~ c('GA,AL,MS,AR,MO,KY,VA,NC'),
    state == 'TX' ~ c('NM,OK,MS,LA'),
    state == 'UT' ~ c('NV,AZ,CO,WY,ID'),
    state == 'VT' ~ c('NY,MA,NH'),
    state == 'VA' ~ c('MD,WV,NC'),
    state == 'WA' ~ c('OR,ID'),
    state == 'WV' ~ c('KY,OH,PA,VA'),
    state == 'WI' ~ c('MN,IA,IL,MI'),
    state == 'WY' ~ c('MT,ID,UT,CO,NE,SD')
  )
}

# Call neighbors, split on comma
str_split(neighbor_list('TX'), ",")[[1]]
# Call individual states with array notation
str_split(neighbor_list('TX'), ",")[[1]][1]
