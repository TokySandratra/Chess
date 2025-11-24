Class {
	#name : 'Knight',
	#superclass : 'Piece',
	#category : 'Myg-Chess-Core',
	#package : 'Myg-Chess-Core'
}

{ #category : 'accessing' }
Knight >> id [
	
	^ 'N'
]

{ #category : 'rendering' }
Knight >> renderOnBlackSquare [

	self subclassResponsibility.
]

{ #category : 'rendering' }
Knight >> renderOnWhiteSquare [ 

	self subclassResponsibility.
]

{ #category : 'rendering' }
Knight >> renderPieceOn: aSquare [

	^ aSquare renderKnight: self
]

{ #category : 'rendering' }
Knight >> targetSquaresLegal: aBoolean [

	^ {
		  (square right ifNotNil: [ :p | p right ifNotNil: #up ]).
		  (square right ifNotNil: [ :p | p right ifNotNil: #down ]).
		  (square left ifNotNil: [ :p | p left ifNotNil: #up ]).
		  (square left ifNotNil: [ :p | p left ifNotNil: #down ]).
		  (square up ifNotNil: [ :p | p up ifNotNil: #left ]).
		  (square up ifNotNil: [ :p | p up ifNotNil: #right ]).
		  (square down ifNotNil: [ :p | p down ifNotNil: #left ]).
		  (square down ifNotNil: [ :p | p down ifNotNil: #right ]) }
		  select: [ :s |
			  s notNil and: [ s hasPiece not or: [ s contents color ~= color ] ] ]
]
