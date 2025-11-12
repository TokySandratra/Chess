Class {
	#name : 'WhiteBishop',
	#superclass : 'Bishop',
	#category : 'Myg-Chess-Core-Downloaded',
	#package : 'Myg-Chess-Core',
	#tag : 'Downloaded'
}

{ #category : 'rendering' }
WhiteBishop >> renderOnBlackSquare [

	^ 'b'.
]

{ #category : 'rendering' }
WhiteBishop >> renderOnWhiteSquare [ 

	^ 'B'.
]
