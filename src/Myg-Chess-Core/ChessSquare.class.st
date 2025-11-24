Class {
	#name : 'ChessSquare',
	#superclass : 'BlElement',
	#instVars : [
		'color',
		'name',
		'label',
		'piece',
		'contents',
		'board',
		'target'
	],
	#category : 'Myg-Chess-Core',
	#package : 'Myg-Chess-Core'
}

{ #category : 'api - text weight' }
ChessSquare class >> black [

	^ self color: Color black
]

{ #category : 'api - text weight' }
ChessSquare class >> color: aColor [

	^ self basicNew
		  color: aColor;
		  initialize;
		  yourself
]

{ #category : 'api - text weight' }
ChessSquare class >> white [

	^ self color: Color white
]

{ #category : 'arithmetic' }
ChessSquare >> + aPoint [

	| column file |
	column := (self column asciiValue + aPoint x) asCharacter asString.
	file := (self file asciiValue + aPoint y) asCharacter asString.

	^ board at: column , file ifAbsent: [ ^ nil ]
]

{ #category : 'accessing' }
ChessSquare >> board [
	^ board
]

{ #category : 'accessing' }
ChessSquare >> board: aMyChessBoard [ 
	
	board := aMyChessBoard
]

{ #category : 'initialization' }
ChessSquare >> click [

	board click: self
]

{ #category : 'accessing' }
ChessSquare >> color [

	^ color
]

{ #category : 'accessing' }
ChessSquare >> color: anObject [

	color := anObject
]

{ #category : 'accessing' }
ChessSquare >> column [
	
	^ name first
]

{ #category : 'accessing' }
ChessSquare >> contents [
	^ contents
]

{ #category : 'accessing' }
ChessSquare >> contents: aPiece [

	| text |
	contents := aPiece.

	text := contents
		        ifNil: [
			        color isBlack
				        ifFalse: [ 'z' ]
				        ifTrue: [ 'x' ] ]
		        ifNotNil: [ contents renderPieceOn: self ].
	piece text: (text asRopedText
			 fontSize: 48;
			 foreground: self foreground;
			 fontName: MyOpenChessDownloadedFont new familyName)
]

{ #category : 'api - configuration' }
ChessSquare >> down [

	^ self + (0 @ -1)
]

{ #category : 'asd' }
ChessSquare >> emptyContents [
	
	self contents: nil
]

{ #category : 'accessing' }
ChessSquare >> file [
	
	^ name second
]

{ #category : 'accessing' }
ChessSquare >> foreground [
			
	^ color isBlack
		ifTrue: [ board blackColor ]
		ifFalse: [ board whiteColor ]
]

{ #category : 'testing' }
ChessSquare >> hasPiece [
	
	^ contents isNil not
]

{ #category : 'highlighting' }
ChessSquare >> highlightTarget [

	self addChild: target
]

{ #category : 'initialization' }
ChessSquare >> initialize [

	super initialize.
	target := BlElement new
		  geometry: BlCircleGeometry new;
		  background: (Color gray alpha: 0.3).

	self layout: BlFrameLayout new.

	label := BlTextElement new.
	label constraintsDo: [ :c | c padding: (BlInsets all: 3) ].

	piece := BlTextElement new.
	piece text: '' asRopedText.
	piece constraintsDo: [ :c |
		c frame horizontal alignCenter.
		c frame vertical alignCenter ].

	self
		addChild: label;
		addChild: piece.
	self
		background: Color white;
		geometry: BlSquareGeometry new.

	self addEventHandlerOn: BlClickEvent do: [ :e | self click ]
]

{ #category : 'accessing' }
ChessSquare >> left [

	^ self + (-1 @ 0)
]

{ #category : 'accessing' }
ChessSquare >> name [
	^ name
]

{ #category : 'accessing' }
ChessSquare >> name: aString [

	name := aString.
	label text: (name asRopedText foreground: color negated)
]

{ #category : 'printing' }
ChessSquare >> printOn: aStream [
	"Generate a string representation of the receiver based on its instance variables."

	super printOn: aStream.
	aStream
		nextPutAll: ' name: ';
		print: name
]

{ #category : 'rendering' }
ChessSquare >> renderBishop: aPiece [

	self subclassResponsibility 
]

{ #category : 'rendering' }
ChessSquare >> renderKing: aPiece [

	self subclassResponsibility .
]

{ #category : 'rendering' }
ChessSquare >> renderKnight: aPiece [

	self subclassResponsibility .
]

{ #category : 'rendering' }
ChessSquare >> renderPawn: aPiece [

	self subclassResponsibility .
]

{ #category : 'rendering' }
ChessSquare >> renderQueen: aPiece [

	self subclassResponsibility .
]

{ #category : 'rendering' }
ChessSquare >> renderRook: aPiece [

	self subclassResponsibility .
]

{ #category : 'accessing' }
ChessSquare >> right [
	
	^ self + (1@0)
]

{ #category : 'initialization' }
ChessSquare >> select [
	
	self border: Color green
]

{ #category : 'highlighting' }
ChessSquare >> unhighlightTarget [

	self removeChild: target
]

{ #category : 'initialization' }
ChessSquare >> unselect [

	self border: BlBorder empty
]

{ #category : 'instance creation' }
ChessSquare >> up [
	
	^ self + (0@1)
]
