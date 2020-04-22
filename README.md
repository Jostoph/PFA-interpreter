# Prog. Fonctionnelle Avancée: Laboratoire 6

> Christoph Rouff Soit Rueff, Tiago Povoa

## Contexte

> Le but de ce laboratoire consiste à jeter les bases d'un interpréteur de langage de programmation simple.

## Fichiers et Modules

- `interpreter.hs` : Module **Interpreter**
- `utils.hs` : Module **Utils**

## Étape 1

```haskell
data Expr = Const Int | Sum Expr Expr | Prod Expr Expr | ...
```

> Le but de ce laboratoire est d'étendre la déclaration de type ci-dessus pour inclure d'autres opérateurs unaires et binaires ainsi que des expressions conditionnelles.

### Implémentation

On utilise une fonction `eval` qui prend une expression `Expr` à évaluer. 

Exemple:

```haskell
eval env (Sum x y) = eval env x + eval env y
```

Dans une expression complexe, des appels récursifs sont faits afin d'évaluer les expressions imbriquées.

Pour la suite, nous avons ajouté un environnement `env` que nous aborderons à l'étape 3.

### Opérateurs implémentés

Voici une liste des opérateurs ayant été implémentés.

#### Unaires

* Not: négation logique
* Fact: effectue la factorielle
* Abs : valeur absolue

#### Binaires

* Sum: addition
* Prod: produit
* Mod: reste de la division entière
* Equal: égalité
* Div: division
* Sub: soustraction
* Greater: strictement supérieur
* Less: strictement inférieur
* Comb: effectue une combinaison, c'est à dire un choix de k objets parmi n, où l'ordre n'est pas important.

#### Lists

- Len :  longueur d'une liste
- Suml : somme des éléments d'une liste
- Prodl : produit des éléments d'une liste

#### Structure d'une liste

Pour l'implémentation de nos listes nous avons fait une structure mutuellement récursive avec les **Expr** où les éléments d'une **List** sont des **Expr**.

```haskell
data Expr =
...
| Len List
| Suml List
| Prodl List

data List = Nil | Concat Expr List
```

### Exemples d'exécution

Dans les exemples qui vont suivre, le premier paramètre de *eval* est l'environnement. Pour l'instant, assumons qu'il est `empty` (Map vide). Nous reviendrons là-dessus à l'étape 3

Le cas le plus simple, évaluer une constante:

```haskell
> eval empty (Const 2)
2
```

Quelques exemples: 

*Note: Avec e=empty*

```haskell
> eval e (Fact (Const 5))
120
```

```haskell
> eval e (Prod (Const 3) (Const 2))
6
```

L'évaluation conditionnelle prend un prédicat, puis évalue le branchement.

```haskell
> eval e (Cond (Greater (Const 3) (Const 2)) (Const 3) (Const 2))
3
```

```haskell
> eval e (Comb (Const 5) (Sub (Const 5) (Const 3)))
10
```

évaluation de fonctions sur listes

```haskell
> eval e (Len (Concat (Const 1) (Concat (Const 2) (Concat (Const 3) Nil))))
3
```



## Étape 2

> Dans un second temps, redéfinissez la fonction show en dérivant explicitement le
> type Expr de la classe Show afin d'afficher des expression de manière lisible.

Comme demandé, nous avons simplement définit show pour chaque `Expr`

```haskell
  instance Show Expr where
    show (Const x) = show x
    show (Sum x y) = show x ++ " + " ++ show y
    ...
  instance Show List where
  	show Nil = "Nil"
  	show (Concat x xs) = "(" ++ show x ++ " : " ++ show xs ++ ")"
```

Nous avons choisit de séparer l'évaluation (*eval*) des fonctions de leur affichage (*show*) dans l'optique où ça rend le code un peu plus maintenable et lisible. Il y a donc un peu de redondance dans le *Show* avec les opérateurs (+, -, %, etc...) mais c'est un choix d'implémentation que nous avons décidé de faire. (Nous aurions pu partager nos **Expr** en *Binary* et *Unary* par exemple)

Voici des exemple d'exécution:

```haskell
> Comb (Const 5) (Sub (Const 5) (Const 2))
C(5, 5 - 2)
```

```haskell
> Cond (Greater (Const 3) (Const 2)) (Const 3) (Const 2)
if 3 > 2 then 3 else 2
```

```haskell
> Suml (Concat (Const 1) (Concat (Sum (Const 1) (Const 2)) Nil))
sum (1 : (1 + 2 : Nil))
```

```haskell
> (Let "c" (Fact (Const 4)) (Sum (Const 3) (Var "c")))
let c = factorial(4) in 3 + c
```



## Étape 3

> Finalement, introduisez la possibilité de définir les variables locales qui peuvent
> être utilisées par leur nom, une fois leur valeur définie par une expression.

Pour cette partie, nous avons choisis d'utiliser une Map avec l'import suivant:

``` haskell
import Data.Map.Strict as Map
```

Cela nous permet de faire un **lookup** par nom et donc d'avoir cette notion d'environnement qui possède des symboles liés à des valeurs concrètes.

Exemple:

On commence par définir un environnement. Ici **m**

```haskell
m=Map.fromList [("a", (Const 5))]
```

Maintenant, on peut vérifier le fonctionnement ainsi:

```haskell
> eval m (Var "a")
5
```

*Note: Ici le type (clé, valeur) est (String, Expr). Ainsi, on peut conserver n'importe quelle expression dans l'environnement, pas seulement des constantes.*

Et finalement, une démonstration avec **Let** :

```haskell
> eval empty (Let "b" (Const 2) (Sum (Const 3) (Var "b")))
5
```

Ici avec une expression plus compliquée qu'une constante:

```haskell
> eval empty (Let "c" (Fact (Const 4)) (Sum (Const 3) (Var "c")))
27
```

## Gestion des erreurs

Nous avons utilisé la fonction `error String` pour renvoyer un message d'erreur personnalisé quand cela était nécessaire. Par exemple quand on essaye d'utiliser une variable qui n'est pas définie dans l'environnement.

```haskell
error ("The variable " ++ name ++ "was not found in this environment.")
```

