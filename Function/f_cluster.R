source(here("Function", "f_clean_data.R"))


f_cluster <- function(K) {
  
  ### Cette fonction permet d'identifier différents groupes de stocks constituants
  ### le NASDAQ100 par approche de K-mean clustering selon des données de l'années. 
  ### 2007. L'analyse repose sur des 'fundamentals' des compagnies. Ces données sont 
  ### importées directement du répertoire Clean_Data.
  
  #  INPUTS 
  #   K : Nombre de groupe qu'on veut former par clustering
  
  #  OUTPUTS
  #   portfolio : [list] (K x 1) donnant les K stocks ayant le plus gros market 
  #   cap. dans leurs cluster.
  
  
  # Importer les données
  load(here('CLean_Data','df_NASDAQ100_fundamentals_clean.rda'))
  
  # Isoler seulement les données pour le clustering et scaler les colonnes
  df_fundamentals <- df_NASDAQ100_fundamentals_clean
  colnames(df_fundamentals) <- gsub("\\.", " ", colnames(df_fundamentals))
  cluster_data <- as.matrix(subset(df_fundamentals, select = -c(Code,Year_,Freq,`TICKER SYMBOL`,CUSIP,`COMPANY NAME`)))
  cluster_data_scaled <- scale(cluster_data)
  
  # K-Means clustering
  K <- 10
  km.out <- kmeans(cluster_data_scaled, centers = K, nstart = 1000)
  df_fundamentals$CLUSTER <- km.out$cluster
  
  # Format des résultats
  df_result <- df_fundamentals[,c("TICKER SYMBOL", "COMPANY NAME", "MARKET CAPITALIZATION (U S $)", "CLUSTER")]
  df_result <- df_result[order(df_result$CLUSTER,-df_result$`MARKET CAPITALIZATION (U S $)`),]
  df_result <- df_result[!duplicated(df_result$CLUSTER),]
  df_result <- df_result[order(df_result$`TICKER SYMBOL`), ]
  portfolio <- df_result[, !(names(df_result) %in% c("MARKET CAPITALIZATION (U S $)", "CLUSTER"))]
  
  portfolio <- as.vector(portfolio)
  portfolio$'TICKER SYMBOL'
}
