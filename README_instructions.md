# Chess Game : Group 10 (Einstein, Toky, Marie)

This is a chess game for Pharo based on Bloc, Toplo and Myg.

## What is this repository really about

The goal of this repository is not to be a complete full blown game, but a good enough implementation to practice software engineering skills:
 - testing
 - reading existing code
 - refactorings
 - profiling
 - debugging

## Installation instructions
### 1. Download and install Pharo
- Go to https://pharo.org/download
- Download the latest Pharo launcher for your operating system (Windows, macOS, or Linux)
- Install and open Pharo

### 2. Load the project baseline
- In a new Pharo image, open the Playground (Ctrl + W) and execute:

```smalltalk
Metacello new
	repository: 'github.com/TokySandratra/Chess:main';
	baseline: 'MygChess';
	onConflictUseLoaded;
	load.
```
- This will automatically load the project code, dependencies, and tests.

### 3. Verify installation
- In the Pharo browser, search for the package by filtering with:  `chess`
- You should see the classes and tests in your image.

### 4. You can open the chess game using the following expression:

```smalltalk
board := MyChessGame freshGame.
board size: 800@600.
space := BlSpace new.
space root addChild: board.
space pulse.
space resizable: true.
space show.
```

## Katas
- Each member of the group 10 have decided to take one Kata per person : 
    - Fix pawn moves : Toky
    - Remove nil checks : Einstein
    - Refactor piece rendering : Marie


# Fix pawn moves (Toky) ♟️

## Toky Sandratra 

### What I've done :
> - Implemented pawn forward movement :  doubleSquareForward in initialRank
> - Added diagonal captures
> - Integrated enPAssant logic
- I was using TDD: Test-Driven-Developement while coding.
------
### 1- Implemented pawn forward movement :  doubleSquareForward in initialRank
> First I created `MyPawnTest` inside the `Myg-Chess-Tests` package and I added some testCases for TDD
> To solve the problem doubleSquareForward I added : `testWhiteIsAtInitialRank`, `testBlackIsAtInitialRank` in the first
```smalltalk
testWhiteIsAtInitialRank 
	|pawn board|
	board := MyChessBoard empty .
	board at: 'e2' put: (pawn:= MyPawn white).
	self assert: (pawn isAtInitialRank) equals: true.
```
> Test turned red then I fixed it by adding the method `isAtInitialRank` in the `MyPawn` subclass of `MyPiece` which is  inside `Myg-Chess-Core` package
```smalltalk
isAtInitialRank
	^ self isWhite
		ifTrue: [ square name last = $2 ]
		ifFalse: [ square name last = $7 ]
```
> Then the both test turned green. I added another test though : `testBlackPawnMovesDoubleSquareForward` and `testWhitePawnMovesDoubleSquareForward`
```smalltalk
testBlackPawnMovesDoubleSquareForward
	| pawn board moves|
	board := MyChessBoard empty.
	board at: 'e7' put: (pawn := MyPawn black).
	moves := (pawn targetSquaresLegal: true) collect: [ :s | s name ].
	self assert: (moves includes: 'e5').
```
> Tests turned red then I modified the `targetSquaresLegal` methods in  the `MyPawn` subclass of `MyPiece` which is  inside `Myg-Chess-Core` package
```smalltalk
targetSquaresLegal: aBoolean
	| moves forwardSquares |
	moves := OrderedCollection new.

	"Normal Move One Square forward"
	forwardSquares := self isWhite
		ifTrue: [ { square up } ]
		ifFalse: [ { square down } ].

	forwardSquares do: [ :s |
		(s notNil and: [ s hasPiece not ]) ifTrue: [ moves add: s ] ].

	"If the pawn is at his initialRank"
	self isAtInitialRank ifTrue: [
		| doubleStep |
		doubleStep := self isWhite ifTrue: [ square up up ] ifFalse: [ square down down ].
		(doubleStep notNil and: [ doubleStep hasPiece not ]) ifTrue: [ moves add: doubleStep ] ].
    ^ moves asArray
```
> The tests turned green after that and I run the game. Then the pawns move double squares forward at the initial rank as we expected
---
### 2- Added diagonal captures
> To solve this I added this part of code inside of  `targetSquaresLegal`

```smalltalk
	"Classical diagonal captured"
	captureSquares := self isWhite
		ifTrue: [ { square up left . square up right} ]
		ifFalse: [ {square down left . square down right} ].

	captureSquares do: [ :s |
		(s notNil and: [ s hasPiece and: [ s contents color ~= color ] ])
			ifTrue: [ moves add: s ] ].
```
### 3- Integrated enPAssant logic 
> This part is the big challenge I have faced
> First I tested this :
```smalltalk
testEnPassant
	| board pawn moves |
	board := MyChessBoard empty.
	board enPassantTargetSquare: (board at: 'd6').
	pawn := MyPawn white.
	board at:'e5' put: pawn.
	moves := (pawn targetSquaresLegal:true) collect: [ :s | s name ].
	self assert: (moves includes: 'd6').
```
> And it failed so I added some methods and attributes to solve this red test :
- Adding `enPassantTargetSquare` attributes in `MyChessBoard`:
```smalltalk
BlElement << #MyChessBoard
	slots: { #squareMatrix . #grid . #selected . #state . #whiteColor . #blackColor . #game . #enPassantTargetSquare };
	package: 'Myg-Chess-Core'

enPassantTargetSquare
	^ enPassantTargetSquare

enPassantTargetSquare: aSquare
	 enPassantTargetSquare := aSquare
```
- The test was still red then I need to complete the `targetSquaresLegal` to handle the enPassant problem with : 
-  `addEnPassantTargets` method in `MyPawn` class to capture the diagonal square from current square 
```smalltalk 
addEnPassantTargets: targets
	self isWhite 
		ifTrue: [ 
				{ square up left . square up right } do: [ :diag |
		(diag notNil and: [ diag = self board enPassantTargetSquare ])
			ifTrue: [ targets  add: diag ] ].
		] 
		ifFalse: [ 
			{ square down left . square down right } do: [ :diag |
		(diag notNil and: [ diag = self board enPassantTargetSquare ])
			ifTrue: [ targets add: diag] ].
		 ]
```
- I send this message (calling the right method) in the `targetSquaresLegal`
- Then the `targetSquaresLegal` became:
```smalltalk
targetSquaresLegal: aBoolean
	| moves forwardSquares captureSquares epSquare |
	moves := OrderedCollection new.

	"Normal Move One Square forward"
	forwardSquares := self isWhite
		ifTrue: [ { square up } ]
		ifFalse: [ { square down } ].

	forwardSquares do: [ :s |
		(s notNil and: [ s hasPiece not ]) ifTrue: [ moves add: s ] ].

	"If the pawn is at his initialRank"
	self isAtInitialRank ifTrue: [
		| doubleStep |
		doubleStep := self isWhite ifTrue: [ square up up ] ifFalse: [ square down down ].
		(doubleStep notNil and: [ doubleStep hasPiece not ]) ifTrue: [ moves add: doubleStep ] ].

	"Classical diagonal captured"
	captureSquares := self isWhite
		ifTrue: [ { square up left . square up right} ]
		ifFalse: [ {square down left . square down right} ].

	captureSquares do: [ :s |
		(s notNil and: [ s hasPiece and: [ s contents color ~= color ] ])
			ifTrue: [ moves add: s ] ].

	"EnPassant"
	"epSquare := self board enPassantTargetSquare.
	epSquare ifNotNil: [
		captureSquares do: [ :s |
			(s = epSquare) ifTrue: [ moves add: s ] ].
		]."
		self addEnPassantTargets: moves .

	^ moves asArray
```
- Then the test turn green and the game is running well but the pawn didn't capture the enemy whose we were passing by
- Then I need to add something inside `moveTo:` method in superclass `MyPiece` 
```smalltalk
moveTo: aSquare
	| delta targetFile targetRank |
	delta := (self square name last digitValue - aSquare name last digitValue) abs.  "Compute the difference between current and targetSquare"
	(delta = 2)  " if it is 2 then there was a doubleMoveForward"
		ifTrue: [ 
			targetFile := self square name first asString. 
			targetRank := (((self square name last digitValue) + (aSquare name last digitValue)) // 2) asString. 
			self board enPassantTargetSquare: (self board at: targetFile , targetRank).  " save the enPAssantTargetSquare and use it to capture the enemy"
		].
    		
	(self legalTargetSquares includes: aSquare) ifFalse: [ ^ self ].

	square emptyContents.
	square := aSquare.
	aSquare contents: self.
	
    "this part capture the pawn we was passing by"
	(self board enPassantTargetSquare = aSquare) ifTrue:  
	[ 
		self isWhite ifTrue: [ square down emptyContents ]
		ifFalse: [ square up emptyContents ]
	 ]
	
		
```
### Difficulties encountered
- Debugging indexable object errors when trying to access board positions and concatenate two objects (especially in` moveTo:`method ) in this part :
```smalltalk
    targetFile := self square name first asString. 
    targetRank := (((self square name last digitValue) + (aSquare name last digitValue)) // 2) asString. 
    self board enPassantTargetSquare: (self board at: targetFile , targetRank).  " save the enPAssantTargetSquare and use it to capture the enemy"
```
- Compute two objects $2 + $3 for example :
```smalltalk
    delta := (self square name last digitValue - aSquare name last digitValue) abs.   "digitValue was the solution"
```
- Understanding the structure of the game and the code to find out the right place needed to change to resolve the problem ( `reverse enginneering`) 
> - What I understood (this my notebook I used to understand the structure as reverse enginneer):
> - BaselineOfMygChess (config du projet avec Metacello : gestion dependance comme pip ,npm maven) , ( versions git, composant interne à charger) 
> - Myg-Chess-Core :  coeur logique du jeu ( metier) : classes( board, square), les pièces , les règles et autres fonctionnalité
> - Myg-Chess-Importers : import des données ou état plateau depuis API, chargement ressources ( images ..) 
> - Myg-Chess-Tests : verification et validation ( deplacement valides , regles respectées, bon resultats, importations fonctionne bien )

## Design decision
#### Why the code is structured this way ?
- Each piece (Pawn, Rook, Knight, etc.) encapsulates its own movement logic especially here `MyPawn`
- The Pawn class is responsible for special rules like en passant so that we should adapt the structure with it
### Priorities
- Added comprehensive tests before optimizing performance. (TDD)
- Focused first on correctness of pawn moves.
- Then ensured clarity and readability of the methods.
### Design patterns used
- Template Method pattern: used for defining general movement behavior in Piece and refining it in Pawn.
- Hook methods: allow subclasses to customize behavior without modifying the main algorithm.
- Polymorphism: all pieces share the same interface for possibleMoves, enabling easy extension.( all pieces can use the method `up`, `down`, `left`, `right`)
---
## Summary :
- During this kata, I know a lot of new things :
> - Structure of game (Chess)
> - How strong is the reverse enginneering to understand a code a solve specifics problem
> - How important is the polymorphism in development 
> - TDD is very useful and a good way to master all tasks and make you focused on microtasks to solve a big problem without any errors in the end

---
---


# Remove nil checks (branche : feature/removeNil)
## fait par Brunine Einstein POINTE-JOUR 

## 1.  Contexte du kata

L’objectif du Kata est de supprimer toutes les utilisations de nil dans la logique du jeu d’échecs Chess.
Le code d’origine utilisait nil pour :
- une case vide,
- une sélection absente,
- une case hors échiquier.

Cela rendait le projet fragile : beaucoup de `ifNil:` et `ifNotNil:`, beaucoup de risques d’erreurs, et du code plus compliqué, diffficile à comprendre.

Pour résoudre ce problème, j’ai appliqué plusieurs notions vues en cours :
- OOP
- Polymorphisme
- Null Object Pattern
- envoi de message
- Reverse engineering pour comprendre le fonctionnement d’un code existant
- TDD (ajout de tests pour sécuriser les changements)
- Refactoring propre

## 2. Reverse Engineering
J’ai commencé par analyser les fichiers du projet pour comprendre où nil apparaissait. 
J’ai dû lire le code existant pour comprendre comment :
- les pièces se déplacent,
- les cases sont stockées,
- les interactions se font dans `MySelectedState`.

J’ai inspecté en particulier :
- `MyChessBoard.class.st`
- `MyPiece`+ sous-classes
- `MySelectedState.class.st` / `MyUnselectedState.class.st`
- `MyChessSquare.class.st`
- les tests dans `Myg-Chess-Tests`

Cela m’a permis d’identifier les zones problématiques :
déplacements hors plateau,  sélection vide, contenus de cases nil etc.

## 3. Null Object Pattern - Où j’ai fait les changements
- MyEmptyPiece
Représente une pièce vide dans une case.

```smalltalk
MyEmptyPiece >> isEmpty
    ^ true
```

- MyNoSquare
Represente une case hors échiquier.
Il répond à toutes les directions sans planter :

```smalltalk
MyNoSquare >> up
    ^ self
```

### 4. Polymorphisme mis en place
Au lieu de tester si une case est vide, je laisse la place a l’objet pour répondre :

Avant :
```smalltalk
square contents ifNotNil: [...]
```

Après :
```smalltalk
square contents isEmpty ifFalse: [...]
```

L’objet MyEmptyPiece contient déjà la logique. c’est lui  qui décide. `isEmpty` n’existait pas avantje l'ai ajouté dans `MyEmptyPiece` uniquement.


### 5. Refactoring du plateau (MyChessBoard)
Fichier : Chess/src/Myg-Chess-Core/MyChessBoard.class.st
La méthode la plus importante modifiée est :

```smalltalk
MyChessBoard >> at: coordinate
    ^ grid at: coordinate ifAbsent: [ MyNoSquare new ]
```

Avant, un déplacement hors plateau renvoyait `nil`. Maintenant, il renvoie un objet fiable.

ce changement impacte automatiquement :
- les mouvements du Roi,
- les mouvements du Cavalier,
- les mouvements diagonaux ,
- les déplacements successifs like `square up left`.



### 6. Refactoring des cases (MyChessSquare)
Ancienne façon d’afficher une pièce vide :

```smalltalk
contents ifNil: [ 'z' ]
```

Apres
```smalltalk
text := (contents isEmpty)
    ifTrue: [ color isBlack ifFalse: [ 'z' ] ifTrue: [ 'x' ] ]
    ifFalse: [ contents renderPieceOn: self ].
```

La case est maintenant toujours cohérente.
`emptyContents` installe explicitement un `MyEmptyPiece` :

```smalltalk
self contents: MyEmptyPiece default
```

### 7.  Exemples de comportements avant / apres
Avant
- `square up up left` pouvait renvoyer `nil`.
- Un roi en coin renvoyait des résultats partiels.
- Le rendu visuel plantait si `contents = nil`.

Aprs
- s isOnBoard contrôle si la case est valide.
- Les mouvements ne plantent plus.
- Le code est plus court et plus clair.

Exemple avec le roi :
```smalltalk
targets := king basicTargetSquares select: [:s | s isOnBoard ].
```

### MyNoSelection – remplace les nil dans la sélection

Fichier : Chess/src/Myg-Chess-Core/MyNoSelection.class.st
Avant :
```smalltalk
selected := nil.
selected ifNotNil: [ selected unselect ]

```
Apres
```smalltalk
selected := MyNoSelection new.
```

### Refactoring dans MyChessSquare
Fichier : Chess/src/Myg-Chess-Core/MyChessSquare.class.st
avant :
```smalltalk
contents ifNil: [ ... ]
```
Apres
```smalltalk
text := (contents isEmpty)
            ifTrue: [...]
            ifFalse: [ contents renderPieceOn: self ].
```

### 8. Tests (TDD)
J’ai ajouté des tests pour vérifier :
- cases vides,
- mouvement en bordure,
- placeholders,
- comportements sans nil.

### MyEdgePieceTest Vérifie que les déplacements en bordure utilisent bien les Null Objects.

Fichier : Chess/src/Myg-Chess-Tests/MyEdgePieceTest.class.st

Exemple :
```smalltalk
self assert: (names includes: 'b1').
self assert: (names includes: 'a2').
self assert: (names includes: 'b2').

```

### MySquareTest 
Fichier : Chess/src/Myg-Chess-Tests/MySquareTest.class.st

```smalltalk
self assert: (square contents isEmpty) equals: true.
placeholder := square contents renderPieceOn: square.
self assert: (placeholder size = 1).

```
Vérifie que `MyEmptyPiece` fonctionne.
Ces tests garantissent que plus aucun `nil` n’apparaît dans le flux logique.

## Problèmes rencontrés
- Je devais comprendre où et comment nil circulait dans le projet.
- Adapter les déplacements du Roi et du Cavalier sans casser les règles.
- Comprendre l’architecture MygChess avant de modifier (reverse engineering).
- Fallait faire attention à ne pas casser les règles de déplacement.


### 9. Design Decisions
Cette section explique les choix techniques que j’ai faits durant le kata, pourquoi je les ai faits, et comment ils influencent la structure du projet. 
Ces décisions ne sont pas toujours visibles directement dans le code, mais elles guident tout le refactoring.

### 10.1 Pourquoi le code est-il organisé de cette façon ?
Le code a été structuré pour éliminer totalement la propagation de `nil` dans le système.
Dans la version d’origine, 	`nil` pouvait apparaître à plusieurs niveaux :
- quand une case était vide,
- quand une case sortait du plateau,
- quand aucune case n’était sélectionnée,
- dans les déplacements successifs (up, down, right…).

Chaque apparition de `nil` obligeait à ajouter des conditions (`ifNil:`) partout.
Cela rendait le code fragile et difficile à maintenir.

J’ai choisi d’utiliser le `Null Object Pattern` pour remplacer chaque `nil` par un objet valide :
- `MyEmptyPiece` pour les cases vides,
- `MyNoSquare` pour les déplacements hors-échiquier,
- `MyNoSelection` pour l’absence de sélection.

Ce choix permet au code de rester orienté objet :
au lieu de vérifier les valeurs, on délègue les comportements aux objets eux-memes.


### 10.2 Pourquoi certaines parties du code sont plus testées que d’autres ?
J’ai priorisé les tests sur les zones les plus fragiles dans le code d’origine :
1. Les déplacements en bordure
		- Ce sont les endroits où `nil` apparaissait le plus souvent.
		- Exemple : square up up left renvoyait nil.
		- J’ai ajouté des tests dans MyEdgePieceTest pour sécuriser ces comportements.

2.  Les cases vides
	- Le rendu visuel plantait quand contents = nil.
	J’ai créé `MySquareTest` pour vérifier que `MyEmptyPiece` fonctionne.

3. L’affichage d’une pièce vide
J’ai testé le placeholder pour assurer un comportement cohérent.

Les pièces (Roi, Cavalier ..) étaient déjà testées, donc j’ai complété seulement ce qui était indispensable pour garantir la stabilité du refactoring.


### 10.3 Où j’ai mis les priorités ?

Mes priorités snt les suivantes :
1. Supprimer toutes les erreurs liées à nil
 priorité absolue, car elles impactaient les mouvements, l’UI et les règles du jeu.
2. Ne jamais casser les règles des pièces
chaque refactoring devait respecter les mouvements légaux.
3. Maintenir la compatibilité avec l’architecture existante
je n’ai pas modifié le design global, seulement les endroits critiques.
5. Écrire des tests minimaux mais suffisants
couverture ciblée, utile, pas de tests inutiles.
6. Rendre le code plus lisible et plus déclaratif
passer de conditions répétitives à du polymorphisme propr


### 10.4 Utilisation des Design Patterns et pourquoi

Utilisation de Null Object Pattern pour remplacer toutes les valeurs `nil`.
- MyEmptyPiece
- MyNoSquare
- MyNoSelection

Pourquoi ?
Parce que je veux  transformer les `nil` en objets capables de répondre aux mêmes messages.
Cela supprime les `ifNil:` et simplifie le flux logique.

### State Pattern (dà existant dans le projet)
- MySelectedState
- MyUnselectedState
Je n’ai pas modifié ce pattern, mais mon travail devait le respecter.
L’introduction de `MyNoSelection` permet justement d’avoir un état cohérent même quand rien n’est sélectionné.

### Polymorphisme
Exemple concret :

```smalltalk
contents isEmpty ifFalse: [...]
```
Avant: on devrait  tester si contents était `nil`.
Après : chaque objet sait dire s’il est vide.

### Message Sending (principe fondamentale du Smalltalk)

J’ai réécrit plusieurs méthodes pour éviter des conditions inutiles et laisser les objets répondre eux-memes aux messages.
Exemple :
```smalltalk
square up right
```
renvoie soit un vrai Square soit un `MyNoSquare`, mais jamais nil.


## Résultat final
- Le code est plus clair, plus sûr et plus cohérent.
- Projet plus aligné avec les notions vues en cours :
	OOP, polymorphisme, Null Object Pattern, TDD, refactoring, reverse engineering.
- Plus aucun test `ifNil:` dans la logique du jeu.
- tests plus riches et robustes.
- Mouvement des pièces fiable, même en bordure du plateau.


### Conclusion
Ce kata a permis d’améliorer en profondeur la qualité du projet Chess.
En remplaçant complètement l’usage de nil par des objets dédiés (Null Object Pattern), j’ai pu rendre le code plus robuste, plus cohérent et beaucoup plus simple à comprendre. 
Grâce au polymorphisme, chaque objet (case vide, case hors plateau, sélection absente) sait maintenant quoi faire, ce qui élimine les conditions complexes et les risques d’erreurs.

Ce travail m’a aussi permis d’appliquer plusieurs notions essentielles vues en cours :
reverse engineering pour comprendre un code existant, refactoring structuré, conception orientée objet, 
TDD pour sécuriser les modifications, et design pattern.

Le résultat final est un projet plus stable, plus lisible et mieux structuré, ou les comportements sont clairement répartis entre les objets. 


---
---
# Refactor piece rendering - branche features/piece-rendering (Marie)

### 1. Contexte du projet
Dans le cadre du kata « Refactor Piece Rendering », l’objectif était d’améliorer l’architecture du code responsable de l’affichage des pièces d’échecs dans le jeu.
La version initiale du code s’appuyait fortement sur des structures conditionnelles (if) pour déterminer, selon la couleur de la pièce et la couleur de la case, quel rendu afficher.
Cette approche rendait le code :
difficile à lire,
difficile à maintenir,
peu évolutive.
Le refactoring visait donc à supprimer ces conditions et à mettre en place un design plus propre et orienté objet, en exploitant les capacités du mécanisme de double dispatch.

### 2. Objectifs du refactoring
Le travail visait à :
- Éliminer les nombreux if utilisés pour tester la couleur de la pièce et celle de la case.
- Introduire deux hiérarchies de classes :
- 	Piece : déclinée en WhitePawn, BlackPawn, WhiteKing, etc.
- 	ChessSquare : déclinée en WhiteChessSquare et BlackChessSquare.
- Permettre un double dispatch :
- 	la case décide de quelle méthode appeler selon sa couleur,
- 	La pièce fournit sa propre implémentation selon son type.

Grâce à ces hiérarchies, chaque combinaison pièce * case est gérée automatiquement par le système d’envoie de messages.

### 3. Implémentation réalisée
Le refactoring repose sur la création de méthodes spécialisées telles que :
### Extrait de code - Pièces
```smalltalk

{ #category : 'rendering' }
BlackPawn >> renderOnBlackSquare [
    ^ 'o'.
]

{ #category : 'rendering' }
BlackPawn >> renderOnWhiteSquare [ 
    ^ 'O'.
]
```

Chaque sous-classe de Piece sait se rendre sur une case noire ou une case blanche, sans que le programme ait besoin de tester la couleur.
### Extrait de code - Cases
```smalltalk

{ #category : 'rendering' }
BlackChessSquare >> renderPawn: aPawn [
    ^ aPawn renderOnBlackSquare .
]

{ #category : 'rendering' }
WhiteChessSquare >> renderPawn: aPawn [
    ^ aPawn renderOnWhiteSquare .
]
```

Ici :
La case choisit la méthode (renderPawn:).
La pièce répond avec son comportement spécifique (renderOnBlackSquare, renderOnWhiteSquare).

Ainsi, le système effectue deux dispatch successifs, l’un sur le type de la case, l’autre sur le type de la pièce.

### 4. Résultat obtenu
Grâce à ce refactoring :
- Les if ont totalement disparu.
- Le code est désormais conforme aux principes SOLID, en particulier :
- 	Single Responsibility Principle
- 	Open/Closed Principle

L’ajout d’une nouvelle pièce ou d’une nouvelle variante du rendu ne nécessite plus de modifier du code existant.
Le comportement est entièrement déterminé par les classes, non par des conditions.

### 5. Problème rencontré
Au cours du travail, j’ai rencontré un problème lié à l’utilisation de Git :
Avant de pousser mes modifications (git push), je n’ai pas effectué un git pull, ce qui aurait permis de récupérer les mises à jour précédentes du dépôt.
Cela a entraîné des conflits entre mes fichiers locaux et ceux déjà présents sur le serveur.
Pour résoudre le problème, j’ai dû :
- analyser les conflits,
- les corriger manuellement avant de finaliser le merge et renvoyer les modifications.
Ce point m’a permis de mieux comprendre l’importance de synchroniser régulièrement son dépôt local avant de pousser des modifications.

### 6. Conclusion
Ce refactoring a permis de transformer un code basé sur des conditions multiples en une architecture orientée objet propre et extensible.
L’utilisation du double dispatch a permis de déléguer la responsabilité du rendu aux objets eux-mêmes, améliorant ainsi la lisibilité, la modularité et la maintenabilité du projet.

Ce travail m’a également permis de mieux comprendre l'importance des refactorings, et m'aidé à comprendre la gestion de version Git.

