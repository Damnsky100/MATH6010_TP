f_create_training_data_classification <- function(start_date, end_date){
  
    # Data.frame construit à partir des données Bloomberg
    merged_df <- read.csv(here("Raw_Data", "classification_data.csv"))
    load(here("Clean_Data", "df_all_trades_naive.rda"))
    
    df_all_trades <- df_all_trades[df_all_trades$start_date >= start_date & df_all_trades$start_date <= end_date, ]
    # Data.frame construit à partir des trades faits in sample
    df_all_trades <- cbind(df_all_trades, as.data.frame(do.call(rbind, strsplit(as.character(df_all_trades$ratio), '/'))))
    
    # Nommer les columnes en fonction du nominateur et denominateur de la paire
    colnames(df_all_trades)[14:15] <- c("numerator", "denominator")
    
    # conversion en date
    df_all_trades$start_date <- as.Date(df_all_trades$start_date)
    merged_df$dates <- as.Date(merged_df$dates)
    
    
    # Inner join fait sur la date et le stock
    result_df <- merge(df_all_trades, merged_df, by.x = c("start_date", "numerator"), by.y = c("dates", "stock"), all = FALSE)
    
    colnames(result_df)[which(names(result_df) %in% colnames(merged_df))] <- paste0(colnames(result_df)[which(names(result_df) %in% colnames(merged_df))], "_num")
    
    # suppression des colomnes donnant un forward seeing bias ou inutile
    result_df <- result_df[, !(names(result_df) %in% c("dates_num", "stock_num", "Latest.earnings.date_num"))]
    
    # Autre inner join
    result_df <- merge(result_df, merged_df, by.x = c("start_date", "denominator"), by.y = c("dates", "stock"), all = FALSE)
    
    colnames(result_df)[which(names(result_df) %in% colnames(merged_df))] <- paste0(colnames(result_df)[which(names(result_df) %in% colnames(merged_df))], "_den")
    
    # # suppression des colomnes donnant un forward seeing bias ou inutile
    trade_classification <- result_df[, !(names(result_df) %in% c("dates_den", "stock_den", "Latest.earnings.date_den", "end_date", "exit_price", "Value_position", "PL_position", "day_count"))]
  
    rda_file_path <- file.path(here("Clean_Data"), 'trade_classification_is.rda')
    save(trade_classification, file = rda_file_path)
    
}