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

---
## Summary :

---
---
# Remove nil checks (Einstein)

---
---
# Refactor piece rendering (Marie)


