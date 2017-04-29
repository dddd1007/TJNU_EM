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
      if(length(unique(cheak_table$CURRENT_FIX_INTEREST_AREA_ID)) == 2){
      position_two <- which(cheak_table$CURRENT_FIX_INTEREST_AREA_ID == 2)
      position_three <- which(cheak_table$CURRENT_FIX_INTEREST_AREA_ID != 2)
      
      if(cheak_table$CURRENT_FIX_INDEX[position_two[length(position_two)]] > cheak_table$CURRENT_FIX_INDEX[position_three[1]]){
        result <- input_table %>% 
          filter(CURRENT_FIX_INTEREST_AREA_ID == 1)
      }else{
        result <- input_table %>% 
          filter(CURRENT_FIX_INTEREST_AREA_ID == 2)
      }}else{
        position_two <- which(cheak_table$CURRENT_FIX_INTEREST_AREA_ID == 2)
        position_three <- which(cheak_table$CURRENT_FIX_INTEREST_AREA_ID == 3)
        position_four <- which(cheak_table$CURRENT_FIX_INTEREST_AREA_ID == 4)
        
        ans_two_three <- cheak_table$CURRENT_FIX_INDEX[position_two[length(position_two)]] < cheak_table$CURRENT_FIX_INDEX[position_three[1]]
        ans_two_four <- cheak_table$CURRENT_FIX_INDEX[position_two[length(position_two)]] < cheak_table$CURRENT_FIX_INDEX[position_four[1]]
        
        get_sum <- sum(ans_two_four, ans_two_three, na.rm = TRUE)
        
        if(get_sum == 2){
          result <- input_table %>% 
            filter(CURRENT_FIX_INTEREST_AREA_ID == 2)
        }else{
          result <- input_table %>% 
            filter(CURRENT_FIX_INTEREST_AREA_ID == 1)
        }
      }
    }
  }
  return(result)
}