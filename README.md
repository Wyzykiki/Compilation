# Projet de Compilation

Dépot du projet de Compilation du premier semestre de M1.

## Structure du dépot

`src/` contient les fichiers sources.  
`Compiler.sh` compile un fichier VAR vers BTC, il est possible de passer des arguments en ligne de commande.  
`BuildAll.sh` permet de compiler tous les compilateurs avec ocamlbuild.

---
## Progression

### Module 1 : Langage impératif
##### Langage à pile STK
- [x] Contrat de base
- [x] Accumulateur
- [x] Allocation et registres

##### Langage arithmétique ART
- [x] Contrat de base
- [ ] Messages d'erreurs
- [ ] Optimiser l'utilisation des registres

##### Langage impératif structuré IMP
- [x] Contrat de base
- [x] Boucle for  
Syntaxe : `for (test; step) {...}` où la variable de test existe déjà (comme pour la boucle while)
- [x] Instructions Break et Continue  
`continue` ne marche pas (boucle infini) avec les boucles for, en effet l'action "d'étape" qui modifie la variable testé, n'est pas accéssible pour le jump

---
### Module 2 : Fonctions
#### Langage avec fonctions et variables locales VAR  
##### Interpréteur  
- [x] Contrat de base
- [x] Paramètres pour main

##### Compilateur  
- [x] Contrat de base
- [x] Paramètres pour main
- [ ] Fonction primitives
- [x] Déclaration des variables au fil de l'eau  
1.Une variable locale avec le même nom qu'un paramètre continue de le masqué pour toute la fonction  
2.Seulement var locales
- [ ] Appels de fonctions dans des expressions
- [ ] Optimisation des appels terminaux
- [ ] Passage par référence
---
### Module 3 : Types et données
##### Langages TYP, REC et TPL
- [ ] Contrat de base
---
### Module 4 : Objets
