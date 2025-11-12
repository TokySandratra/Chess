Class {
	#name : 'BlackKnight',
	#superclass : 'Knight',
	#category : 'Myg-Chess-Core-Downloaded',
	#package : 'Myg-Chess-Core',
	#tag : 'Downloaded'
}

{ #category : 'rendering' }
BlackKnight >> renderOnBlackSquare [

	^ 'm'.
]

{ #category : 'rendering' }
BlackKnight >> renderOnWhiteSquare [ 

	^ 'M'.
]
