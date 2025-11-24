Class {
	#name : 'Bishop',
	#superclass : 'Piece',
	#category : 'Myg-Chess-Core',
	#package : 'Myg-Chess-Core'
}

{ #category : 'accessing' }
Bishop >> id [
	
	^ 'B'
]

{ #category : 'rendering' }
Bishop >> renderOnBlackSquare [

	self subclassResponsibility.
]

{ #category : 'rendering' }
Bishop >> renderOnWhiteSquare [ 

	self subclassResponsibility.
]

{ #category : 'rendering' }
Bishop >> renderPieceOn: aSquare [

	^ aSquare renderBishop: self
]

{ #category : 'rendering' }
Bishop >> targetSquaresLegal: aBoolean [

	^ (self upRightDiagonalLegal: aBoolean)
	  , (self upLeftDiagonalLegal: aBoolean)
	  , (self downRightDiagonalLegal: aBoolean)
	  , (self downLeftDiagonalLegal: aBoolean)
]
