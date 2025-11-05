Class {
	#name : 'WhitePawn',
	#superclass : 'Pawn',
	#category : 'Myg-Chess-Core-Downloaded',
	#package : 'Myg-Chess-Core',
	#tag : 'Downloaded'
}

{ #category : 'rendering' }
WhitePawn >> renderOnBlackSquare [

	^ 'p'.
]

{ #category : 'rendering' }
WhitePawn >> renderOnWhiteSquare [ 

	^ 'P'.
]
