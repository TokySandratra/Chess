Class {
	#name : 'WhiteKing',
	#superclass : 'King',
	#category : 'Myg-Chess-Core-Downloaded',
	#package : 'Myg-Chess-Core',
	#tag : 'Downloaded'
}

{ #category : 'rendering' }
WhiteKing >> renderOnBlackSquare [

	^ 'k'.
]

{ #category : 'rendering' }
WhiteKing >> renderOnWhiteSquare [ 

	^ 'K'.
]
