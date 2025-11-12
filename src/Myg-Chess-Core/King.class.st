Class {
	#name : 'King',
	#superclass : 'Piece',
	#category : 'Myg-Chess-Core',
	#package : 'Myg-Chess-Core'
}

{ #category : 'as yet unclassified' }
King >> attackingSquares [

	^ self basicTargetSquares
]

{ #category : 'rendering' }
King >> basicTargetSquares [

	"The king can move one square on each direction including diagonals"
	^ {
		square ifNotNil: #right.
		square up ifNotNil: #right.
		square ifNotNil: #up.
		square up ifNotNil: #left.
		square ifNotNil: #left.
		square left ifNotNil: #down.
		square ifNotNil: #down.
		square down ifNotNil: #right
	}
]

{ #category : 'accessing' }
King >> id [
	
	^ 'K'
]

{ #category : 'testing' }
King >> isCheckMated [
	"We check if the opponent pieces target all my potential movements and myself"

	| threatenedSquares |
	threatenedSquares := self opponentPieces flatCollect: [ :e |
		                     e attackingSquares ].

	^ self legalTargetSquares isEmpty and: [
		  threatenedSquares includes: self square ]
]

{ #category : 'testing' }
King >> isInCheck [
	"We check if the opponent pieces target all my potential movements and myself"

	| threatenedSquares |
	threatenedSquares := self opponentPieces flatCollect: [ :e |
		                     e attackingSquares ].

	^ threatenedSquares includes: self square
]

{ #category : 'testing' }
King >> isKing [
	
	^ true
]

{ #category : 'accessing' }
King >> opponentPieces [

	^ self board pieces select: [ :e |
		  e notNil and: [ e color = color negated ] ]
]

{ #category : 'rendering' }
King >> renderOnBlackSquare [

	self subclassResponsibility.
]

{ #category : 'rendering' }
King >> renderOnWhiteSquare [ 

	self subclassResponsibility.
]

{ #category : 'rendering' }
King >> renderPieceOn: aSquare [

	^ aSquare renderKing: self
]

{ #category : 'rendering' }
King >> targetSquaresLegal: aBoolean [
	"The king cannot move to threatened squares"

	| threatenedSquares |
	threatenedSquares := self opponentPieces flatCollect: [ :e |
		                     e attackingSquares ].

	^ self basicTargetSquares select: [ :s |
		  s notNil and: [
			  (threatenedSquares includes: s) not and: [
				  s hasPiece not or: [ 
					"The square has an opponent piece"
					s contents color ~= color and: [ 
						"Let's hope the piece is not defended"
						threatenedSquares includes: s  ] ] ] ] ]
]
