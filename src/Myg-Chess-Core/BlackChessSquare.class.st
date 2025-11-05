Class {
	#name : 'BlackChessSquare',
	#superclass : 'ChessSquare',
	#category : 'Myg-Chess-Core-Downloaded',
	#package : 'Myg-Chess-Core',
	#tag : 'Downloaded'
}

{ #category : 'instance creation' }
BlackChessSquare class >> new [
^ self basicNew
		  color: Color black;
		  initialize;
		  yourself
]

{ #category : 'accessing' }
BlackChessSquare >> contents: aPiece [ 
| text |
	contents := aPiece.

	text := contents
		        ifNil: ['x']
		        ifNotNil: [ contents renderOnBlackSquare ].
	piece text: (text asRopedText
			 fontSize: 48;
			 foreground: self foreground;
			 fontName: MyOpenChessDownloadedFont new familyName)	
]

{ #category : 'rendering' }
BlackChessSquare >> renderBishop: aBishop [

 ^ aBishop renderOnBlackSquare . 
]

{ #category : 'rendering' }
BlackChessSquare >> renderKing: aKing [

	^ aKing renderOnBlackSquare .
]

{ #category : 'rendering' }
BlackChessSquare >> renderKnight: aKnight [

	^ aKnight renderOnBlackSquare .
]

{ #category : 'rendering' }
BlackChessSquare >> renderPawn: aPawn [

	^ aPawn renderOnBlackSquare .
	
]

{ #category : 'rendering' }
BlackChessSquare >> renderQueen: aQueen [

	^ aQueen renderOnBlackSquare .
]

{ #category : 'rendering' }
BlackChessSquare >> renderRook: aRook [

	^ aRook renderOnBlackSquare .
]
