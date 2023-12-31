
# Importation des données

Pour ce TP, différentes sources de données ont été utilisées. Voici les
étapes afin d'accéder à chacune d'entre elles.

# 1 : Constituants du NASDAQ 100

Accéder au lien suivant et se connecter à l'aide du nom d'utilisateur et
du mot de passe : <https://wrds-www.wharton.upenn.edu/pages/get-data/>

Sous 'Third Party Provider', sélectionner 'Compustat - Capital IQ'

Sous 'Compustat', sélectionner 'North America'

Ensuite sélectionner 'Index constituents'

Étape 1 : Sélectionner 2007-12 comme date

Étape 2 : Sélectionner NASDAQ 100 comme index

Étape 3 : Sélectionner les tickers comme query variables

Étape 4 : Slectionner .CSV comme output format

Étape 5 : Lancer la requête

# 2 : Données fondamentales

Accéder au lien suivant et se connecter à l'aide du nom d'utilisateur et
du mot de passe : <https://wrds-www.wharton.upenn.edu/pages/get-data/>

Sous 'Third Party Provider', sélectionner 'Thomson/Refinitiv'

Sous 'World Scope', sélectionner 'Fundamental Annuals'

Étape 1 : Sélectionner 2007 comme date

Étape 2 : Sélectionner les CUSIP code voulu

Étape 3 : Sélectionner les variables utilisées dans le projet (voir
l'Annexe du rapport)

Étape 4 : Slectionner .xslx comme output format

Étape 5 : Lancer la requête

# 3 : Données de volatilités

Accéder au lien suivant et se connecter à l'aide du nom d'utilisateur et
du mot de passe : <https://wrds-www.wharton.upenn.edu/pages/get-data/>

Sous 'Third Party Provider', sélectionner '"OptionsMetrics'

Sous 'Ivy DB Us', sélectionner 'Options' puis sélectionner 'Options'

Étape 1 : Sélectionner 2006-01-01 à 2022-12-31 comme date

Étape 2 : Sélectionner les tickers voulus

Étape 3 : Sélectionner les variables "Ticker", "Days to Expiration",
"Delta", "Interpolated Implied Vol."

Étape 4 : Slectionner .CSV comme output format

Étape 5 : Lancer la requête

# 4 : Données de classification

Accéder à un terminal Bloomberg avec les identifiants fournis par le laboratoire de la salle de marchés

Dans excel, utiliser l'onglet bloomberg en haut de la spread sheet afin de cliquer sur spreadsheet builder

Étape 1 : Sélectionner 2008-01-01 à 2022-12-31 comme date

Étape 2 : Sélectionner les tickers voulus

Étape 3 : Sélectionner les variables "Market Cap", "Bloomberg 1 year distance to default", "Volume total call", "Volume total put", "open int total call", "open int total put", "Latest earnings date", "Stock volume", "Put Call Volume ratio", "Put Call open interest ratio", "Short interest equity float"

Étape 4 : Selectionner .CSV comme output format

Étape 5 : Lancer la requête

# 5 : Données de régression 

Accéder à Yahoo Finance via la librairie "quantmod" et utiliser la fonction getSymbols()

Étape 1 : Sélectionner la date de 2008-01-01 à 2022-12-31

Étape 2 : Sélectionner les tickers voulus

Étape 4 : Sélectionner la source : Yahoo Finance

Étape 5 : Rouler la fontion getSymbols()

Étape 6 : Sélectionner les variables : "Open", "Adjusted Close", "High", "Low", "Volume"


