Class {
	#name : 'Piece',
	#superclass : 'Object',
	#instVars : [
		'color',
		'square',
		'nextSquare'
	],
	#category : 'Myg-Chess-Core',
	#package : 'Myg-Chess-Core'
}

{ #category : 'constants' }
Piece class >> black [

	^ self new
		  color: Color black;
		  yourself
]

{ #category : 'constants' }
Piece class >> white [

	^ self new
		  color: Color white;
		  yourself
]

{ #category : 'as yet unclassified' }
Piece >> attackingSquares [

	^ self legalTargetSquares .
]

{ #category : 'path commands' }
Piece >> board [

	^ square board
]

{ #category : 'rendering' }
Piece >> collectSquares: aBlock [

	^ self collectSquares: aBlock legal: false
]

{ #category : 'rendering' }
Piece >> collectSquares: aBlock legal: shouldBeLegal [

	^ self collectSquares: aBlock while: [ :aSquare |
		  aSquare notNil and: [ shouldBeLegal ==> aSquare hasPiece not ] ]
]

{ #category : 'rendering' }
Piece >> collectSquares: collectBlock while: untilBlock [

   | targets next |
    targets := OrderedCollection new.

    "Collect up right"
    next := square.
    [ untilBlock value: (next := collectBlock value: next) ]
        whileTrue: [ targets add: next ].

    "If we can it the next piece, then add it too"
    (next notNil and: [ next contents color ~= color ]) ifTrue: [ targets add: next ].

    ^ targets
]

{ #category : 'accessing' }
Piece >> color [

	^ color
]

{ #category : 'accessing' }
Piece >> color: aColor [

	color := aColor
]

{ #category : 'rendering' }
Piece >> downFile [

	^ self downFileLegal: false
]

{ #category : 'rendering' }
Piece >> downFileLegal: aBoolean [

	^ self collectSquares: [ :aSquare | aSquare down ] legal: aBoolean
]

{ #category : 'rendering' }
Piece >> downLeftDiagonal [

	^ self downLeftDiagonalLegal: false
]

{ #category : 'rendering' }
Piece >> downLeftDiagonalLegal: aBoolean [

	^ self
		  collectSquares: [ :aSquare | aSquare down ifNotNil: #left ]
		  legal: aBoolean
]

{ #category : 'rendering' }
Piece >> downRightDiagonal [

	^ self downRightDiagonalLegal: false
]

{ #category : 'rendering' }
Piece >> downRightDiagonalLegal: aBoolean [

	^ self
		  collectSquares: [ :aSquare | aSquare down ifNotNil: #right ]
		  legal: aBoolean
]

{ #category : 'accessing' }
Piece >> id [

	^ 'Q'
]

{ #category : 'testing' }
Piece >> isKing [
	
	^ false
]

{ #category : 'testing' }
Piece >> isPiece [
	
	^ true
]

{ #category : 'testing' }
Piece >> isWhite [
	
	^ color isBlack not
]

{ #category : 'rendering' }
Piece >> leftFile [

	^ self leftFileLegal: false
]

{ #category : 'rendering' }
Piece >> leftFileLegal: aBoolean [

	^ self collectSquares: [ :aSquare | aSquare left ] legal: aBoolean
]

{ #category : 'rendering' }
Piece >> legalTargetSquares [
^ self targetSquaresLegal: true.
]

{ #category : 'path commands' }
Piece >> moveTo: aSquare [

	(self legalTargetSquares includes: aSquare) ifFalse: [ ^ self ].

	square emptyContents.
	square := aSquare.
	aSquare contents: self
]

{ #category : 'rendering' }
Piece >> nextSquare [
    ^ nextSquare 
]

{ #category : 'rendering' }
Piece >> nextSquare: aSquare [
    nextSquare :=  aSquare 
]

{ #category : 'rendering' }
Piece >> opponentPieces [

    ^ self board pieces select: [ :e |
          e notNil and: [ e color = color negated ] ]
]

{ #category : 'rendering' }
Piece >> renderOnBlackSquare [

	self subclassResponsibility.
]

{ #category : 'rendering' }
Piece >> renderOnWhiteSquare [

	self subclassResponsibility.
]

{ #category : 'rendering' }
Piece >> rightFile [

	^ self rightFileLegal: false
]

{ #category : 'rendering' }
Piece >> rightFileLegal: aBoolean [

	^ self collectSquares: [ :aSquare | aSquare right ] legal: aBoolean
]

{ #category : 'accessing' }
Piece >> square [
	^ square
]

{ #category : 'accessing' }
Piece >> square: aMyChessSquare [ 
	
	square := aMyChessSquare
]

{ #category : 'rendering' }
Piece >> targetSquares [

	^ self targetSquaresLegal: false
]

{ #category : 'rendering' }
Piece >> targetSquaresLegal: aBoolean [

	self subclassResponsibility
]

{ #category : 'rendering' }
Piece >> upFile [

	^ self upFileLegal: false
]

{ #category : 'rendering' }
Piece >> upFileLegal: aBoolean [

	^ self collectSquares: [ :aSquare | aSquare up ] legal: aBoolean
]

{ #category : 'rendering' }
Piece >> upLeftDiagonal [

	^ self upLeftDiagonalLegal: false
]

{ #category : 'rendering' }
Piece >> upLeftDiagonalLegal: aBoolean [

	^ self collectSquares: [ :aSquare | aSquare up ifNotNil: #left ] legal: aBoolean
]

{ #category : 'rendering' }
Piece >> upRightDiagonal [

	^ self upRightDiagonalLegal: false
]

{ #category : 'rendering' }
Piece >> upRightDiagonalLegal: aBoolean [

	^ self
		  collectSquares: [ :aSquare | aSquare up ifNotNil: #right ]
		  legal: aBoolean
]
