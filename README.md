# Informations importantes

## MATH6010 – TP A
**David Ardia version 2023-08-27**

- Ce travail vaut 20% de la note finale.
- Ce travail doit être réalisé en équipe de 3-4 personnes.
- L’entretien oral de 10% validera ce travail individuellement.
- Je ne répondrai qu’aux questions posées sur le forum.
- Documentez clairement tout votre code.
- Le nombre total de points est de 100. Les points par question sont indiqués entre crochets.
- Vous pouvez soumettre votre code dans un langage autre que R (par exemple, Python ou MATLAB).
- Vos fichiers doivent être déposés au plus tard à la date limite. 10 % des points seront retirés pour chaque heure de dépassement.

## Objectifs

Votre objectif est de créer une stratégie d’investissement réaliste basée sur plusieurs techniques d’apprentissage automatique vues pendant le cours. La stratégie est de votre choix : il peut s’agir d’une stratégie de sélection de titres ou de market timing, d’une stratégie de rotation géographique/sectorielle, de trading de devises, de crypto trading, de trading de momentum/mean reversion sur les futures, etc.

## Exigences

Vous disposez d’une flexibilité totale pour la création de la stratégie mais vous devez respecter les exigences suivantes :
- Les données doivent être différentes de celles vues en classe. Parcourez le web !
- La dimensions des données doit être au moins 50 (caractéristiques, actifs, etc.). Soyez créatif !
- La période de backtest doit être d’au moins cinq ans à la fréquence mensuelle (vous pouvez utiliser des caractéristiques à une fréquence plus faible, mais le backtest doit être au moins en mensuel).
- Les techniques d’apprentissage automatique doivent comprendre au moins une technique de régression et une technique de classification.

## Livrable

Vous devez soumettre un rapport et le code pour reproduire vos résultats.

### 1. Rapport [50 points]

Vous devez fournir un rapport de trois pages (police Arial, taille 11). Le rapport doit contenir:
- a) Une description générale de la stratégie de trading et de sa performances (1 page) [20 points]. Vous pouvez utiliser des graphiques et des tableaux pour les mesures de performance. Regardez les fiches techniques des fonds !
- b) Une description détaillée des stratégies d’apprentissage automatique utilisées (1 1/2 page) [20 points]. Vous pouvez utiliser des formules mathématiques pour expliquer comment vous construisez les signaux.
- c) Commentaires sur les avantages et les inconvénients de l’approche (1/2 page) [10 points].

### 2. Code [50 points]

Vous devez me fournir un fichier zip contenant votre code et vos données. La structure doit être organisée comme un projet Rstudio et contenir les éléments suivants :
- Un fichier `run_install_packages.R` pour installer toutes les librairies nécessaires.
- Un fichier `run_strategy.R` pour exécuter le backtest.
- Un dossier `data` avec les données brutes et un fichier `README` expliquant d’où elles proviennent (avec les étapes).
- Un fichier `run_data.R` pour traiter les données brutes si nécessaire ou récupérer les données sur le web.
- Un dossier `functions` avec les fichiers contenant vos fonctions.
- Un dossier `outputs` avec tous les tableaux et graphiques générés par `run_strategy.R`.

J’évaluerai votre travail comme suit :
- a) Structure du projet [10 points]
- b) Convention de codage [10 points]
- c) Efficacité du codage [10 points]
- d) Correction du codage [20 points]

Le code doit être documenté et fonctionner sans problème.

## Bonus

- Vous obtenez +5 points si vous utilisez RMarkdown.
- Vous obtenez +5 points si vous utilisez des données de Eikon Thomson Reuters ou Compustat (disponibles au Laced).
- Vous obtenez +5 points si vous utilisez des données d’OptionMetrics (disponibles au Laced).

## Quelques idées

- Relisez attentivement le chapitre R programming du manuel Statistical methods for quantitative finance.
- Utilisez les signaux pour filtrer l’univers.
- Utilisez le double tri.
- Insérer les signaux dans une configuration de la frontière efficiente.
- Insérer les signaux dans des portefeuilles basés sur le risque.
- Utiliser l’approche “portfolio from sorts’ ’.
