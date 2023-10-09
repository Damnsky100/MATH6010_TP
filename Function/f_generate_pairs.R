f_generate_pairs <- function(stocks_symbols) {
  
  ### Cette fonction permet de créer une liste ou chaque element 
  ### est une paire de stock.
  
  #  INPUTS 
  #   stocks_symbols : les stocks choisit
  
  #  OUTPUTS
  #   all_pairs : [list] liste contenant toutes les paires comme element.
  
  # Determine all combos and keep them name
  # Store all possible pairs in a matrix
  combo_matrix<- combn(stocks_symbols, 2)
  # Create empty list to store the pairs
  all_pairs <- list()
  # Iterate through the columns of the combinations matrix
  for (i in 1:ncol(combo_matrix)) {
    # Extract the two stocks in the current combination
    stockA <- combo_matrix[1, i]
    stockB <- combo_matrix[2, i]
    
    # Create the combination string and add it to the list
    combo <- paste(stockA, "/", stockB, sep = "")
    all_pairs[[i]] <- combo
  }
  cat("Number of combos created:", length(all_pairs), "\n")
  return(all_pairs)
}