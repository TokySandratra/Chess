Class {
	#name : 'BlackPawn',
	#superclass : 'Pawn',
	#category : 'Myg-Chess-Core-Downloaded',
	#package : 'Myg-Chess-Core',
	#tag : 'Downloaded'
}

{ #category : 'rendering' }
BlackPawn >> renderOnBlackSquare [

	^ 'o'.
]

{ #category : 'rendering' }
BlackPawn >> renderOnWhiteSquare [ 

	^ 'O'.
]
