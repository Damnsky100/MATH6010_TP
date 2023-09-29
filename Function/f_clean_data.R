library('here')

f_clean_data_clustering <- function() {
  
  ### Cette fonction nettoie les données servant de base au clustering 
  ### effectuer sur les constituants du Dow Jones. La fonction ne prend pas
  ### d'entrée car les données sont directement tirées du le répertoire Raw_Data.
  ### Ces données sont :
  ###    1) dow_constituants.csv : Données sur les constituants du Dow Jones 
  ###       depuis sa création. 
  ###       [Source : S&P Global. "Compustat Daily Updates - Index Constituents" WRDS. https://wrds-www.wharton.upenn.edu/pages/get-data/compustat-capital-iq-standard-poors/compustat/north-america-daily/index-constituents/. 2023.
  ###    2) dow_constituants_fundamentals.csv : Données fondamentales annuelles 
  ###       des constituants du Dow Jones de 2007 à 2022 (C colonnes)
  ###       [Source : Refinitiv. "Worldscope - Fundamentals Annual" WRDS. https://wrds-www.wharton.upenn.edu/pages/get-data/thomson-reuters/worldscope/fundamentals-annual/. 2023.
  ###   3) fundamentals_mapping.txt : Fichier texte permettant de renommr de 
  ###     façon approprié les colonenes dans dow_constituants_fundamentals.csv
  
  
  #  OUTPUTS
  #   df   : [data.frame] (N x C) de données annuelles 'fundamentals' des 
  #          constituants du Dow Jones ainsi que les dates d'entrée et de 
  #          sortie de l'index. (C : nombre de fundamentals choisie + dates)
  
  
  # Importer les données nécéssaires au clustering
  DOW_const    <- read.csv(here("Raw_Data", "dow_constituants.csv"))                   # Constituants Dow Jones
  df_fund      <- read.csv(here("Raw_Data", "dow_constituants_fundamentals.csv"))      # Données sur les constituants
  fund_mapping <- read.csv(here("Raw_Data", "fundamentals_mapping.txt"), header=FALSE) # Mapping des colonnes
  
  
  # Nettoyer les données du Dow Jones
  DOW_const$thru[DOW_const$thru == ''] <- format(Sys.Date(),  "%Y-%m-%d") 
  
  colnames(DOW_const)[colnames(DOW_const) == "co_tic"]  <- "TICKER"
  colnames(DOW_const)[colnames(DOW_const) == "co_conm"] <- "NAME"
  
  DOW_const <- DOW_const[,c("TICKER","NAME","from","thru")]
  DOW_const <- DOW_const[order(DOW_const$TICKER),]
  
  # Nettoyer les données sur les données 'fundamentals' des constituants
  fund_mapping_split  <- data.frame(do.call(rbind, strsplit(fund_mapping$V1, ":", fixed = TRUE)))
  colnames(df_fund)   <- fund_mapping_split$X2
  
  colnames(df_fund)[colnames(df_fund) == "Company Name"] <- "NAME"
  
  df_fund <- subset(df_fund, select = -c(code,freq,CUSIP,NAME))
  
  # Merge les infos des constituants avec les données 'fundamentals'
  df <- merge(df_fund, DOW_const, by = "TICKER")
  
  # Enlever les lignes qui contiennes plus de NA que d'informations
  df <- df[rowSums(is.na(df)) < (ncol(df) / 2),]
  df
  
}
  