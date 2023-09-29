source(here("Function", "f_clean_data.R"))


f_cluster_DOW <- function(K) {
  
  ### Cette fonction permet d'identifier différents groupes de stocks constituants
  ### le Dow Jones par approche de K-mean clustering de 2007 à 2022. L'analyse
  ### repose sur des 'fundamentals' des compagnies. Ces données sont importer par 
  ### la fonction f_clean_data_clustering().
  
  #  INPUTS 
  #   K : Nombre de groupe qu'on veut former par clustering
  
  #  OUTPUTS
  #   portfolio_per_year : [data.frame] (K x 16) donnant les K stocks ayant le
  #   plus gros market cap. dans leurs cluster par années (de 2007 à 2022)
  
  
  # Fonction permettant d'extraire les données nettoyées
  df_clean <- f_clean_data_clustering()
  
  unique_years <- sort(unique(df_clean$YEAR))
  TICKER_list <- list()
  for (i in 1:length(unique_years)) {
    
    # Filter pour l'année voulu
    df_cluster_tmp <- df_clean[df_clean$YEAR == unique_years[i],]
    
    # Filtrer pour les constituants présents du Dow Jones
    from_tmp   <- sapply(df_cluster_tmp$from, function(date_str) as.numeric(substr(date_str, 1, 4)))
    thru_tmp   <- sapply(df_cluster_tmp$thru, function(date_str) as.numeric(substr(date_str, 1, 4)))
    df_cluster <- df_cluster_tmp[(df_cluster_tmp$YEAR > df_cluster_tmp$from) & 
                                   (df_cluster_tmp$YEAR < df_cluster_tmp$thru), ]
    
    # Remplacer les NA par les moyennes des colonnes
    df_cluster[] <- lapply(df_cluster, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))
    
    # Isoler seulement les données pour le clustering et scaler les colonnes
    cluster_data <- as.matrix(subset(df_cluster, select = -c(TICKER,YEAR,NAME,from,thru)))
    cluster_data_scaled <- scale(cluster_data)
    
    # k-mean clustering avec K
    km.out <- kmeans(cluster_data, centers = K, nstart = 100)
    df_cluster$cluster <- km.out$cluster
    km.out$tot.withinss
    
    # Isoler les plus grands market cap par cluster
    df_result <- df_cluster[,c('TICKER','NAME','cluster','Market Capitalization')]
    df_result <- df_result[order(df_result$cluster,-df_result$`Market Capitalization`),]
    df_result <- df_result[!duplicated(df_result$cluster),]
    
    # Populer la liste de stock chosis pour l'année en analyse
    TICKER_list[[as.character(unique_years[i])]] <- list(sort(df_result$TICKER))
    
  }
  
  # Cnnstruction du dataframe identifiant les stocks pour chaque année
  portfolio_per_year  <- as.data.frame(TICKER_list)
  colnames(portfolio_per_year) <- names(TICKER_list)
  
  portfolio_per_year

}
