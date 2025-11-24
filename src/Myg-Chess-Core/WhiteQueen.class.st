Class {
	#name : 'WhiteQueen',
	#superclass : 'Queen',
	#category : 'Myg-Chess-Core-Downloaded',
	#package : 'Myg-Chess-Core',
	#tag : 'Downloaded'
}

{ #category : 'rendering' }
WhiteQueen >> renderOnBlackSquare [

	^ 'q'.
]

{ #category : 'rendering' }
WhiteQueen >> renderOnWhiteSquare [ 

	^ 'Q'.
]
