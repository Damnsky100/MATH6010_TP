f_create_training_data_classification <- function(){
  
    merged_df <- read.csv(here("Raw_Data", "classification_data.csv"))
    load(here("Clean_Data", "df_all_trades_naive.rda"))
    
    df_all_trades <- cbind(df_all_trades, as.data.frame(do.call(rbind, strsplit(as.character(df_all_trades$ratio), '/'))))
    
    # Rename the columns as needed
    colnames(df_all_trades)[14:15] <- c("numerator", "denominator")
    
    # Ensure date columns are in datetime format
    df_all_trades$start_date <- as.Date(df_all_trades$start_date)
    merged_df$dates <- as.Date(merged_df$dates)
    
    
    # Perform the inner join based on the condition
    result_df <- merge(df_all_trades, merged_df, by.x = c("start_date", "numerator"), by.y = c("dates", "stock"), all = FALSE)
    
    colnames(result_df)[which(names(result_df) %in% colnames(merged_df))] <- paste0(colnames(result_df)[which(names(result_df) %in% colnames(merged_df))], "_num")
    
    # Drop unnecessary columns
    result_df <- result_df[, !(names(result_df) %in% c("dates_num", "stock_num", "Latest earnings date_num"))]
    
    # Perform another inner join
    result_df <- merge(result_df, merged_df, by.x = c("start_date", "denominator"), by.y = c("dates", "stock"), all = FALSE)
    
    colnames(result_df)[which(names(result_df) %in% colnames(merged_df))] <- paste0(colnames(result_df)[which(names(result_df) %in% colnames(merged_df))], "_den")
    
    # Drop more unnecessary columns
    trade_classification <- result_df[, !(names(result_df) %in% c("dates_den", "stock_den", "Latest earnings date_den", "end_date", "exit_price", "Value_position", "PL_position"))]
  
    rda_file_path <- file.path(here("Clean_Data"), 'trade_classification_is.rda')
    save(trade_classification, file = rda_file_path)
    
}