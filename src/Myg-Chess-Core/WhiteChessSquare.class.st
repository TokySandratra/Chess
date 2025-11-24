Class {
	#name : 'WhiteChessSquare',
	#superclass : 'ChessSquare',
	#category : 'Myg-Chess-Core-Downloaded',
	#package : 'Myg-Chess-Core',
	#tag : 'Downloaded'
}

{ #category : 'instance creation' }
WhiteChessSquare class >> new [ 
^ self basicNew
		  color: Color white ;
		  initialize;
		  yourself
]

{ #category : 'accessing' }
WhiteChessSquare >> contents: aPiece [

	| text |
	contents := aPiece.

	text := contents
		        ifNil: ['z']
		        ifNotNil: [ contents renderOnWhiteSquare ].
	piece text: (text asRopedText
			 fontSize: 48;
			 foreground: self foreground;
			 fontName: MyOpenChessDownloadedFont new familyName)
]

{ #category : 'rendering' }
WhiteChessSquare >> renderBishop: aBishop [

	^ aBishop renderOnWhiteSquare . 
]

{ #category : 'rendering' }
WhiteChessSquare >> renderKing: aKing [

	^ aKing renderOnWhiteSquare .
]

{ #category : 'rendering' }
WhiteChessSquare >> renderKnight: aKnight [

	^ aKnight renderOnWhiteSquare .
]

{ #category : 'rendering' }
WhiteChessSquare >> renderPawn: aPawn [

	^ aPawn renderOnWhiteSquare .
]

{ #category : 'rendering' }
WhiteChessSquare >> renderQueen: aQueen [

	^ aQueen renderOnWhiteSquare .
]

{ #category : 'rendering' }
WhiteChessSquare >> renderRook: aRook [

	^ aRook renderOnWhiteSquare .
]
