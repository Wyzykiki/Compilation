# Projet de Compilation

Dépot du projet de Compilation du premier semestre de M1.

## Structure du dépot

`src/` contient les fichiers sources.  
`Compiler.sh` compile un fichier VAR vers BTC, il est possible de passer des arguments en ligne de commande.  
`BuildAll.sh` permet de compiler tous les compilateurs avec ocamlbuild. (Pour VAR, c'est la version de base qui est compilée)

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
`VARInterpreter/`
- [x] Contrat de base
- [x] Paramètres pour main

##### Compilateur  
`VAR/`
- [x] Contrat de base  

`VARExt/`

***FEX -> OTF -> REF -> VAR***
- [x] Paramètres pour main (à la compilation)  
Il faut passer les arguments à `VARCompiler` ou à `Compiler.sh`  
Je n'ai pas réussi à modifier la VM pour les donner à l'execution.
- [x] Fonction primitives  
Un appel à la fonction `print` dans VAR sera traduit de FUN vers CLL,
vers un appel à la procedure systeme `_fun_print` qui utilise l'instruction systeme `print`
> Langage OTF (On The Fly)
>- [x] Déclaration des variables au fil de l'eau  
Une variable locale avec le même nom qu'un paramètre, le masque dans toutes la fonction.

> Langage FEX (Function in EXpr)
>- [x] Appels de fonctions dans des expressions
- [ ] Optimisation des appels terminaux
- [x] Passage par référence
---
### Module 3 : Types, données et objets
##### Langages TYP, REC, TPL, CLS et EXT
- [ ] Contrat de base
