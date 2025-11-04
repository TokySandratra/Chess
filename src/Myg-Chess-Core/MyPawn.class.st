Class {
	#name : 'MyPawn',
	#superclass : 'MyPiece',
	#category : 'Myg-Chess-Core',
	#package : 'Myg-Chess-Core'
}

{ #category : 'accessing' }
MyPawn >> addEnPassantTargets: targets [
	self isWhite 
		ifTrue: [ 
				{ super upLeftDiagonal. super upRightDiagonal } do: [ :diag |
		(diag notNil and: [ diag = (MyChessBoard empty) enPassantTargetSquare ])
			ifTrue: [ targets add: diag ] ].
		] 
		ifFalse: [ 
			{ super downLeftDiagonal. super downRightDiagonal } do: [ :diag |
		(diag notNil and: [ diag = (MyChessBoard empty) enPassantTargetSquare ])
			ifTrue: [ targets add: diag ] ].
		 ]
	
]

{ #category : 'accessing' }
MyPawn >> id [

	^ 'P'
]

{ #category : 'accessing' }
MyPawn >> isAtInitialRank [
	^ self isWhite
		ifTrue: [ square name last = $2 ]
		ifFalse: [ square name last = $7 ]
]

{ #category : 'rendering' }
MyPawn >> renderPieceOn: aSquare [ 
	
	^ aSquare renderPawn: self
]

{ #category : 'rendering' }
MyPawn >> targetSquaresLegal: aBoolean [
	| moves forwardSquares captureSquares epSquare |
	moves := OrderedCollection new.

	"Normal Move One Square forward"
	forwardSquares := self isWhite
		ifTrue: [ { square up } ]
		ifFalse: [ { square down } ].

	forwardSquares do: [ :s |
		(s notNil and: [ s hasPiece not ]) ifTrue: [ moves add: s ] ].

	"If the pawn is at his initialRank"
	self isAtInitialRank ifTrue: [
		| doubleStep |
		doubleStep := self isWhite ifTrue: [ square up up ] ifFalse: [ square down down ].
		(doubleStep notNil and: [ doubleStep hasPiece not ]) ifTrue: [ moves add: doubleStep ] ].

	"Classical diagonal captured"
	captureSquares := self isWhite
		ifTrue: [ { square up left . square up right} ]
		ifFalse: [ {square down left . square down right} ].

	captureSquares do: [ :s |
		(s notNil and: [ s hasPiece and: [ s contents color ~= color ] ])
			ifTrue: [ moves add: s ] ].

	"EnPassant"
	epSquare := self board enPassantTargetSquare.
	epSquare ifNotNil: [
		captureSquares do: [ :s |
			(s = epSquare) ifTrue: [ moves add: s ] ].
		].

	^ moves asArray

	
]
