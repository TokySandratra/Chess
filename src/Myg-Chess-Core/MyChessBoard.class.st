Class {
	#name : 'MyChessBoard',
	#superclass : 'BlElement',
	#instVars : [
		'squareMatrix',
		'grid',
		'selected',
		'state',
		'whiteColor',
		'blackColor',
		'game'
	],
	#category : 'Myg-Chess-Core',
	#package : 'Myg-Chess-Core'
}

{ #category : 'instance creation' }
MyChessBoard class >> empty [

	^ super new
]

{ #category : 'instance creation' }
MyChessBoard class >> fromFENBoard: aFENBoard [

	^ super new
		  initializeFromFENBoard: aFENBoard;
		  yourself
]

{ #category : 'instance creation' }
MyChessBoard class >> new [

	self error: 'Please use the other constructors'
]

{ #category : 'accessing' }
MyChessBoard >> at: coordinate [

	^ grid at: coordinate ifAbsent: nil
]

{ #category : 'accessing' }
MyChessBoard >> at: aString ifAbsent: aFullBlockClosure [

	^ grid at: aString ifAbsent: aFullBlockClosure
]

{ #category : 'accessing' }
MyChessBoard >> at: coordinate put: aPiece [

	| square |
	square := grid at: coordinate.
	square contents: aPiece.
	aPiece square: square
]

{ #category : 'accessing' }
MyChessBoard >> blackColor [

	^ blackColor
]

{ #category : 'accessing' }
MyChessBoard >> blackColor: anObject [

	blackColor := anObject
]

{ #category : 'event handling' }
MyChessBoard >> click: aMyChessSquare [ 
	
	state := state clickOn: aMyChessSquare
]

{ #category : 'accessing' }
MyChessBoard >> game [

	^ game
]

{ #category : 'accessing' }
MyChessBoard >> game: aGame [

	game := aGame
]

{ #category : 'highlighting' }
MyChessBoard >> highLightTargets: squares [

	squares do: [ :sq | sq ifNotNil: #highlightTarget ]
]

{ #category : 'initialization' }
MyChessBoard >> initialize [

	super initialize.

	self blackColor: Color blue.
	self whiteColor: Color orange.

	state := MyUnselectedState board: self.
	grid := Dictionary new.

	self background: (BlBackground paint: Color veryVeryLightGray).
	self layout: (BlGridLayout vertical
			 cellSpacing: 5;
			 columnCount: 8;
			 rowCount: 8;
			 yourself).
	self constraintsDo: [ :aLayoutConstraints |
		aLayoutConstraints horizontal fitContent.
		aLayoutConstraints vertical fitContent ].

	self initializeSquares
]

{ #category : 'initialization' }
MyChessBoard >> initializeFromFENBoard: aFENBoard [

    aFENBoard keysAndValuesDo: [ :k :v |
        v ~= 'empty' ifTrue: [
            | substrings color piece |
            substrings := v substrings.
            color := substrings first.
            piece := substrings second.
            self
                at: k
                put:
                    ((self class environment at: ( color, piece capitalized) asSymbol)
                         perform: color asLowercase asSymbol) ] ]
]

{ #category : 'initialization' }
MyChessBoard >> initializePieces [

    | black |
    black := false.
    $a asciiValue to: $h asciiValue do: [ :column |
        8 to: 1 by: -1 do: [ :file |
            | squareNotation square |
            squareNotation := column asCharacter asString , file asString.
            square := black
                          ifTrue: [
                              black := false.
                              BlackChessSquare new 
                                  board: self;
                                  yourself ]
                          ifFalse: [
                              black := true.
                              WhiteChessSquare new 
                                  board: self;
                                  yourself ].
            square name: squareNotation.
            grid at: squareNotation put: square.
            self addChild: square ].
        black := black not ].

    self at: 'a1' put: WhiteRook white.
    self at: 'b1' put: WhiteKnight white.
    self at: 'c1' put: WhiteBishop white.
    self at: 'd1' put: WhiteQueen white.
    self at: 'e1' put: WhiteKing white.
    self at: 'f1' put: WhiteBishop white.
    self at: 'g1' put: WhiteKnight white.
    self at: 'h1' put: WhiteRook white.

    $a asciiValue to: $h asciiValue do: [ :column |
        self at: column asCharacter asString , '2' put: WhitePawn white ].

    self at: 'a8' put: BlackRook black.
    self at: 'b8' put: BlackKnight black.
    self at: 'c8' put: BlackBishop black.
    self at: 'd8' put: BlackQueen black.
    self at: 'e8' put: BlackKing black.
    self at: 'f8' put: BlackBishop black.
    self at: 'g8' put: BlackKnight black.
    self at: 'h8' put: BlackRook black.

    $a asciiValue to: $h asciiValue do: [ :column |
        self at: column asCharacter asString , '7' put: BlackPawn black ]
]

{ #category : 'initialization' }
MyChessBoard >> initializeSquares [

    | black |
    black := false.
    $a asciiValue to: $h asciiValue do: [ :column |
        8 to: 1 by: -1 do: [ :file |
            | squareNotation square |
            squareNotation := column asCharacter asString , file asString.
            square := black
                          ifTrue: [
                              black := false.
                              BlackChessSquare new
                                  board: self;
                                  contents: nil;
                                  yourself ]
                          ifFalse: [
                              black := true.
                              WhiteChessSquare new
                                  board: self;
                                  contents: nil;
                                  yourself ].
            square name: squareNotation.
            grid at: squareNotation put: square.
            self addChild: square ].
        black := black not ]
]

{ #category : 'accessing' }
MyChessBoard >> pieces [
	
	^ children collect: [ :square | square contents ]
]

{ #category : 'accessing' }
MyChessBoard >> select: aSquare [ 
	
	state select: aSquare
]

{ #category : 'accessing' }
MyChessBoard >> selected: aMyChessSquare [ 
	
	selected := aMyChessSquare
]

{ #category : 'highlighting' }
MyChessBoard >> unhighLightTargets: squares [

	squares do: [ :sq | sq ifNotNil: #unhighlightTarget ]
]

{ #category : 'selection' }
MyChessBoard >> unselectSelected [
	
	selected ifNil: [ ^ self ].
	selected doUnselect
]

{ #category : 'accessing' }
MyChessBoard >> whiteColor [

	^ whiteColor
]

{ #category : 'accessing' }
MyChessBoard >> whiteColor: anObject [

	whiteColor := anObject
]
