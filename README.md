# takuzuGame

**takuzuGame** est un package R qui contient une application Shiny permettant de jouer au jeu Takuzu (également connu sous le nom de Binairo). Ce jeu consiste à remplir une grille avec des 0 et des 1 en respectant certaines règles.

## Installation

Avant de commencer, assurez-vous que vous avez installé R et RStudio sur votre machine.

### Prérequis

- R version 4.0.0 ou supérieure
- Shiny : `install.packages("shiny")`

### Installation de `takuzuGame`

Vous pouvez installer **takuzuGame** directement GitHub.

#### Installation depuis Github

```r
# Installer devtools si ce n'est pas déjà fait
install.packages("devtools")

# Installer le package depuis GitHub
devtools::install_github("votre_nom_utilisateur/takuzuGame")
```
## Utilisation 

Une fois le package installé, vous pouvez lancer l’application Shiny incluse dans takuzuGame.

### Lancer l'application 
1. 	Chargez le package avec la fonction library() :
```r
library(takuzuGame)
```
2.	Lancez l’application Shiny avec la fonction run_App() :
```r
run_app()
```
Cela ouvrira l’application Shiny dans votre navigateur ou dans un panneau intégré de RStudio, selon votre configuration.
## Jouer au jeu 

Une fois l’application lancée, vous pourrez interagir avec la grille de Takuzu en remplissant les cases avec des 0 et des 1. Suivez les règles du jeu pour compléter la grille correctement.

##  Dépannage

•	Si l’application ne se lance pas, vérifiez que vous avez bien installé les dépendances nécessaires (comme Shiny).
	•	Si vous rencontrez une erreur spécifique, consultez la section “Issues” sur GitHub pour des solutions ou posez une question.
 ## Licence

Ce package est sous licence MIT.
