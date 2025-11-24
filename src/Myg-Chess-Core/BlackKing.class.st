Class {
	#name : 'BlackKing',
	#superclass : 'King',
	#category : 'Myg-Chess-Core-Downloaded',
	#package : 'Myg-Chess-Core',
	#tag : 'Downloaded'
}

{ #category : 'rendering' }
BlackKing >> renderOnBlackSquare [

	^ 'l'.
]

{ #category : 'rendering' }
BlackKing >> renderOnWhiteSquare [ 

	^ 'L'.
]
