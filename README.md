# Rapport

## Avant-propos

Le repo est récent mais j'ai bien sûr commencé à travailler avant (en témoigne le gros premier commit). Je n'avais pas vu qu'il en était demandé un.

J'ai suivi un cours d'interprétation abstraite ce semestre et c'était intéressant d'en voir une application (simple mais concrètement utile).

Je ne connaissais pas la librairie Fix et elle est cool.
L'interface est vraiment agréable à utiliser.

J'ai laissé les commentaires "TODO" (en replacant "TODO" par "DONE") pour que ce soit plus simple de retrouver le code que j'ai écrit.


## Première tâche

Dans `uninitialized_places.ml`, pour la fonction `foreach_successor`, c'est (je trouve) assez subtile mais l'on peut supposer que lorsque l'on réalise une affectation la rvalue est bien initialisée.

C'est parce que l'on vérifiera ensuite pour chaque ligne que ce dont on a besoin est bien initialisé. En particulier ce qu'on aura supposé serait contredit si ce n'était pas le cas, et on obtiendra donc bien une erreur, avant même que notre supposition serait utile (elle ne le sera que dans les prochaines lignes).

J'avais au début oublié de compter les arguments de la fonction comme bien initialisés. C'est arrivé, je pense, parce que je n'avais pas tout à fait compris au début que l'on analysait une fonction, donc je n'avais pas de raison de penser aux arguments.

## Seconde tâche

J'ai mis un peu de temps à comprendre ce qui était attendu, pour la seconde assertion. Une fois comprise il n'y avait pas grand chose à faire.


## Troisième tâche

### Contraintes de survie

Les lifetimes étant associé à des types, il est pertinent d'écrire une fonction d'unification qui justement unifie les lifetimes de deux types qui, effacés, sont les mêmes. Elle s'appelle `unify_lft_in_typ` 


J'ai écrit une fonction auxiliaire permettant de calculer le type d'une rvalue. Il faudra instancier de nouvelles durées de vies et imposer les bnonnes contraintes dessus.
-- En fait, on ne calcule pas le véritable type mais une approximation de celui-ci. Il se trouve que l'on se fiche de si les valeurs concrètes sont des entiers, des booléens ou autre; la seule chose qui nous intéresse sont les références.

On se donne encore une fonction pour faire attention aux reborrows (borrow de la forme `&*e`, et il peut y avoir d'autres déréferencements dans `e`). La fonction `beware_reberrow : lft -> place -> ()` s'occupe d'ajouter les contraintes nécessaires.

Finalement on itère sur l'ensemble des instructions pour faire les unifications nécessaires.
Il n'y rien à faire pour la plupart des instructions.

note: à chaque appelle de fonction on se donne un type frais pour cette fonction, de sorte à pouvoir unifier le type de ses arguments et de retour (de la même manière que pour du polymorphisme plus conventionnel).

### Contraintes de vie

C'est plus simple. On écrit ce qui est demandé

## Quatrième tâche

On écrit une fonction `check : lft -> ()` qui vérifie si les contraintes sont bien respectés pour un lifetime générique.
Bien sûr je me suis trompé dans le sens de la relation au début.

## Extension

J'avais commencé à ajouter des types algébriques mais j'ai pas eu le temps de faire beaucoup de chose (branche extension).


Il faut définir de nouvelles expressions : les constructeurs et le pattern-matching, dont il faudra prendre en compte dans la vérification des types effacés.

Il faut aussi penser à la traduction en minimir. On peut utiliser une instruction dédiée pour le match, inspiré de celle pour le `if` (`pl -> label list`) (ou probablement une autre structure qu'une liste pour des questions d'efficacité), qui ressemblerait à celle des fonctions dans le sens où elle doit introduire des nouveaux arguments et de nouveaux lifetimes. Il faudra alors compléter le code un peu partout pour prendre en compte ce nouveau cas. 
On a aussi une nouvelle sorte de rvalue, les constructeurs (qui eux se traitent vis-à-vis du borrow checker comme la création de structures)

