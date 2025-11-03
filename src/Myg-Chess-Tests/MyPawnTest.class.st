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
MyPawnTest >> testBlackPawnMovesOneSquareForward [
	| pawn board moves|
	board := MyChessBoard empty.
	board at: 'f7' put: (pawn := MyPawn black ).
	moves := (pawn targetSquaresLegal: true) collect: [ :s | s name ].
	self assert: (moves includes: 'f6').
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
MyPawnTest >> testWhitePawnMovesOneSquareForward [
	| pawn board moves|
	board := MyChessBoard empty.
	board at: 'e2' put: (pawn := MyPawn white).
	moves := (pawn targetSquaresLegal: true) collect: [ :s | s name ].
	self assert: (moves includes: 'e3').
]
