Class {
	#name : 'BlackBishop',
	#superclass : 'Bishop',
	#category : 'Myg-Chess-Core-Downloaded',
	#package : 'Myg-Chess-Core',
	#tag : 'Downloaded'
}

{ #category : 'rendering' }
BlackBishop >> renderOnBlackSquare [

	^ 'v'.
]

{ #category : 'rendering' }
BlackBishop >> renderOnWhiteSquare [ 

	^ 'V'.
]
