# Prog. Fonctionnelle Avancée: Laboratoire 6

> Christoph Rouff Soit Rueff, Tiago Povoa

## Contexte

> Le but de ce laboratoire consiste à jeter les bases d'un interpréteur de langage de programmation simple.

## Étape 1

```haskell
data Expr = Const Int | Sum Expr Expr | Prod Expr Expr | ...
```

> Le but de ce laboratoire est d'étendre la déclaration de type ci-dessus pour inclure d'autres opérateurs unaires et binaires ainsi que des expressions conditionnelles.

### Implémentation

On utilise une fonction `eval` qui prend une expression `Expr` à évaluer. 

Exemple:

Dans une expression complexe, des appels récursifs sont faits afin d'évaluer les expressions imbriquées.

Pourr la suite, nous avons ajouté un environnement `env` que nous aborderons à l'étape 3.

### Opérateurs implementés

Voici une liste des opérateurs ayant été implementés.

#### Unaires

* Not: négation logique
* Fact: effectue la factorielle

#### Binaires

* Sum: addition
* Prod: produit
* Mod: reste de la division entière
* Equal: égalité
* Div: division
* Sub: soustraction
* Greater: strictement supérieur
* Abs: valeur absolue
* Less: strictement inférieur
* Comb: effectue une combinaison, c'est à dire un choix de k objets parmi n, où l'ordre n'est pas important.

### Exemples d'exécution

Dans les exemples qui vont suivre, le premier paramètre de eval est l'environnement. Pour l'instant, assumons qu'il est `empty`. Nous reviendrons là-dessus à l'étape 3

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

## Étape 2

> Dans un second temps, redéfinissez la fonction show en dérivant explicitement le
> type Expr de la classe Show afin d'afficher des expression de manière lisible.

Comme demandé, nous avons simplement définit show pour chaque `Expr`

```haskell
  instance Show Expr where
    show (Const x) = show x
    show (Sum x y) = show x ++ " + " ++ show y
    ...
```

Nous avons choisit de séparer l'implémentation des fonctions de leur affichage dans l'optique où ça rend le code un peu plus maintenable et lisible. 

Voici des exemple d'exécution:

```haskell
> show (Comb (Const 5) (Sub (Const 5) (Const 2)))
"C(5, 5 - 2)"
```

```haskell
show (Cond (Greater (Const 3) (Const 2)) (Const 3) (Const 2))
"if 3 > 2 then 3 else 2"
```

## Étape 3

> Finalement, introduisez la possibilité de définir les variables locales qui peuvent
> être utilisées par leur nom, une fois leur valeur définie par une expression.

Pour cette partie, nous avons choisis d'utiliser une Map avec l'import suivant:

``` haskell
import Data.Map.Strict as Map
```

Cela nous permet de faire un lookup par nom et donc d'avoir cette notion d'environnement qui possède des symboles liés à des valeurs concrètes.

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

