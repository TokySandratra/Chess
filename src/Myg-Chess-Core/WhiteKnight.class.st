Class {
	#name : 'WhiteKnight',
	#superclass : 'Knight',
	#category : 'Myg-Chess-Core-Downloaded',
	#package : 'Myg-Chess-Core',
	#tag : 'Downloaded'
}

{ #category : 'rendering' }
WhiteKnight >> renderOnBlackSquare [

	^ 'n'.
]

{ #category : 'rendering' }
WhiteKnight >> renderOnWhiteSquare [ 

	^ 'N'.
]
