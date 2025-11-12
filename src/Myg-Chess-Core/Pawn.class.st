Class {
	#name : 'Pawn',
	#superclass : 'Piece',
	#category : 'Myg-Chess-Core',
	#package : 'Myg-Chess-Core'
}

{ #category : 'accessing' }
Pawn >> id [

	^ 'P'
]

{ #category : 'rendering' }
Pawn >> renderOnBlackSquare [

	self subclassResponsibility.
]

{ #category : 'rendering' }
Pawn >> renderOnWhiteSquare [ 

	self subclassResponsibility.
]

{ #category : 'rendering' }
Pawn >> renderPieceOn: aSquare [ 
	
	^ aSquare renderPawn: self
]

{ #category : 'rendering' }
Pawn >> targetSquaresLegal: aBoolean [

	^ (self isWhite
		   ifTrue: [ { square up } ]
		   ifFalse: [ { square down } ]) select: [ :s |
		  s notNil and: [
			  s hasPiece not or: [ s contents color ~= color ] ] ]
]
