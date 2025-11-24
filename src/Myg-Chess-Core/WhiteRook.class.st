Class {
	#name : 'WhiteRook',
	#superclass : 'Rook',
	#category : 'Myg-Chess-Core-Downloaded',
	#package : 'Myg-Chess-Core',
	#tag : 'Downloaded'
}

{ #category : 'rendering' }
WhiteRook >> renderOnBlackSquare [

	^ 'r'.
]

{ #category : 'rendering' }
WhiteRook >> renderOnWhiteSquare [ 

	^ 'R'.
]
