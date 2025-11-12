Class {
	#name : 'BlackRook',
	#superclass : 'Rook',
	#category : 'Myg-Chess-Core-Downloaded',
	#package : 'Myg-Chess-Core',
	#tag : 'Downloaded'
}

{ #category : 'rendering' }
BlackRook >> renderOnBlackSquare [

	^ 't'.
]

{ #category : 'rendering' }
BlackRook >> renderOnWhiteSquare [ 

	^ 'T'.
]
