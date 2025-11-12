Class {
	#name : 'Rook',
	#superclass : 'Piece',
	#category : 'Myg-Chess-Core',
	#package : 'Myg-Chess-Core'
}

{ #category : 'accessing' }
Rook >> id [
	^ 'R'
]

{ #category : 'rendering' }
Rook >> renderOnBlackSquare [

	self subclassResponsibility.
]

{ #category : 'rendering' }
Rook >> renderOnWhiteSquare [

	self subclassResponsibility.
]

{ #category : 'rendering' }
Rook >> renderPieceOn: aSquare [

	^ aSquare renderRook: self
]

{ #category : 'rendering' }
Rook >> targetSquares [

	^ self upFile , self downFile , self leftFile , self rightFile
]

{ #category : 'rendering' }
Rook >> targetSquaresLegal: aBoolean [

	^ (self upFileLegal: aBoolean) , (self downFileLegal: aBoolean)
	  , (self leftFileLegal: aBoolean) , (self rightFileLegal: aBoolean)
]
