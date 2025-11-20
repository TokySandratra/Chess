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

## 2. Approche général (Reverse Engineering)
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

L’objet MyEmptyPiece contient déjà la logique. c’est lui  qui décide. `isEmpty` n’existait pas avantje l'ai ajouté dans `MyEmptyPiece`.


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
`selected := nil.
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


### 9. Pourquoi cette solution ?
- 1 Plus robuste : Plus aucun nil ne se propage dans le système.
- 2 Orientation objet propre : Chaque objet connaît son rôle.
Par exemple, MyNoSquare sait qu’il n’est pas sur le plateau --->> isOnBoard = false.
- 3 Polymorphisme : Plus de branches conditionnelles.
Le code devient déclaratif et simple à lire.


## Problèmes rencontrés
- Je devais comprendre où et comment nil circulait dans le projet.
- Adapter les déplacements du Roi et du Cavalier sans casser les règles.
- Comprendre l’architecture MygChess avant de modifier (reverse engineering).
- Fallait faire attention à ne pas casser les règles de déplacement.

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
# Refactor piece rendering (Marie)

This Pull Request aims at refactoring piece rendering (Kata Refactor Piece rendering ). It introduces two main class hierarchies: Piece and ChessSquare declined into color specific subclasses:

WhiteChessSquare
BlackChessSquare
WhitePawn
BlackPawn
WhiteKing
BlackKing etc..
This separation enables double dispatch and suppress if clauses.

{ #category : 'rendering' }
BlackPawn >> renderOnBlackSquare [

	^ 'o'.
]

{ #category : 'rendering' }
BlackPawn >> renderOnWhiteSquare [ 

	^ 'O'.
]

{ #category : 'rendering' }
BlackChessSquare >> renderPawn: aPawn [

	^ aPawn renderOnBlackSquare .
	
]

{ #category : 'rendering' }
WhiteChessSquare >> renderPawn: aPawn [

	^ aPawn renderOnWhiteSquare .
]
In this implementation, it's no longer necessary to check square and piece color, since each respective class represents a case.
