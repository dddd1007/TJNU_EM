mydata <- read.csv(file.choose(), header = TRUE, stringsAsFactors = FALSE)

library(tidyverse)

mydata <- mydata %>% 
  filter(CURRENT_FIX_INTEREST_AREA_ID != ".")

mydata$CURRENT_FIX_INTEREST_AREA_ID_2 <- c(mydata$CURRENT_FIX_INTEREST_AREA_ID[-1], 0)

mydata %>% 
  transform(CURRENT_FIX_INTEREST_AREA_ID = as.numeric(CURRENT_FIX_INTEREST_AREA_ID), CURRENT_FIX_INTEREST_AREA_ID_2 = as.numeric(CURRENT_FIX_INTEREST_AREA_ID_2)) %>% 
  mutate(Cha = CURRENT_FIX_INTEREST_AREA_ID_2 - CURRENT_FIX_INTEREST_AREA_ID) %>% 
  filter(Cha != 0) %>% 
  filter(CURRENT_FIX_INTEREST_AREA_RUN_ID == 1) %>% 
  filter(CURRENT_FIX_INTEREST_AREA_ID == 1 | CURRENT_FIX_INTEREST_AREA_ID == 2) %>% 
  split(.$RECORDING_SESSION_LABEL) -> sub_list

result_list <- list()

for(i in 1 : length(sub_list)){
  temp <- sub_list[[i]]
  
}

temp %>% 
  split(.$sentence) -> a

for(i in 1 : 128){
  print(extract_result(a[[i]]))
}

extract_result <- function(input_table) {
  result <- as.numeric(vector(length = ncol(input_table)))
  if(nrow(input_table) == 1){
    result <- input_table[1,]
  }else{
    mydata %>% 
      filter(RECORDING_SESSION_LABEL == input_table$RECORDING_SESSION_LABEL[1] & sentence == input_table$sentence[1]) %>% 
      filter(CURRENT_FIX_INTEREST_AREA_RUN_ID == 1) %>% 
      filter(CURRENT_FIX_INTEREST_AREA_ID == 2 | CURRENT_FIX_INTEREST_AREA_ID == 3 | CURRENT_FIX_INTEREST_AREA_ID == 4) -> cheak_table
    if(nrow(cheak_table) == 1){
      result <- cheak_table[1,]
    }else{
      position_two <- which(cheak_table$CURRENT_FIX_INTEREST_AREA_ID == 2)
      position_three <- which(cheak_table$CURRENT_FIX_INTEREST_AREA_ID != 2)
     
       if(cheak_table$CURRENT_FIX_INDEX[position_two] > cheak_table$CURRENT_FIX_INDEX[position_three]){
        result <- input_table %>% 
          filter(CURRENT_FIX_INTEREST_AREA_ID == 1)
      }else{
        result <- input_table %>% 
          filter(CURRENT_FIX_INTEREST_AREA_ID == 2)
      }
    }
  }
  return(result)
}