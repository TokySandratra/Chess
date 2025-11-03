Class {
	#name : 'MyPawnTest',
	#superclass : 'TestCase',
	#category : 'Myg-Chess-Tests',
	#package : 'Myg-Chess-Tests'
}

{ #category : 'tests' }
MyPawnTest >> testBlackIsAtInitialRank [ 
	|pawn board|
	board := MyChessBoard empty .
	board at: 'e7' put: (pawn:= MyPawn black ).
	self assert: (pawn isAtInitialRank) equals: true.
]

{ #category : 'tests' }
MyPawnTest >> testBlackPawnMovesDoubleSquareForward [
	| pawn board moves|
	board := MyChessBoard empty.
	board at: 'e7' put: (pawn := MyPawn black).
	moves := (pawn targetSquaresLegal: true) collect: [ :s | s name ].
	self assert: (moves includes: 'e5').
]

{ #category : 'tests' }
MyPawnTest >> testBlackPawnMovesOneSquareForward [
	| pawn board moves|
	board := MyChessBoard empty.
	board at: 'f6' put: (pawn := MyPawn black ).
	moves := (pawn targetSquaresLegal: true) collect: [ :s | s name ].
	self assert: (moves includes: 'f5').
]

{ #category : 'tests' }
MyPawnTest >> testId [

	self assert: MyPawn new id equals: 'P'
]

{ #category : 'tests' }
MyPawnTest >> testWhiteIsAtInitialRank [ 
	|pawn board|
	board := MyChessBoard empty .
	board at: 'e2' put: (pawn:= MyPawn white).
	self assert: (pawn isAtInitialRank) equals: true.
]

{ #category : 'tests' }
MyPawnTest >> testWhitePawnMovesDoubleSquareForward [
	| pawn board moves|
	board := MyChessBoard empty.
	board at: 'e2' put: (pawn := MyPawn white).
	moves := (pawn targetSquaresLegal: true) collect: [ :s | s name ].
	self assert: (moves includes: 'e4').
]

{ #category : 'tests' }
MyPawnTest >> testWhitePawnMovesOneSquareForward [
	| pawn board moves|
	board := MyChessBoard empty.
	board at: 'e3' put: (pawn := MyPawn white).
	moves := (pawn targetSquaresLegal: true) collect: [ :s | s name ].
	self assert: (moves includes: 'e4').
]
