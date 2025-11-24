Class {
	#name : 'BlackQueen',
	#superclass : 'Queen',
	#category : 'Myg-Chess-Core-Downloaded',
	#package : 'Myg-Chess-Core',
	#tag : 'Downloaded'
}

{ #category : 'rendering' }
BlackQueen >> renderOnBlackSquare [

	^ 'w'.
]

{ #category : 'rendering' }
BlackQueen >> renderOnWhiteSquare [ 

	^ 'W'.
]
