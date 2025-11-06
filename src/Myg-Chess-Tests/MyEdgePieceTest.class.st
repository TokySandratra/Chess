Class {
    #name : 'MyEdgePieceTest',
    #superclass : 'TestCase',
    #category : 'Myg-Chess-Tests',
    #package : 'Myg-Chess-Tests'
}

{ #category : 'tests' }
MyEdgePieceTest >> testKingAtCornerTargetsOnBoard [
    | board king targets names |
    board := MyChessBoard empty.
    board at: 'a1' put: (king := MyKing white).

    targets := king basicTargetSquares select: [ :s | s isOnBoard ].
    names := targets collect: [ :s | s name ].

    self assert: (names includes: 'b1').
    self assert: (names includes: 'a2').
    self assert: (names includes: 'b2').
    self assert: (names size = 3).
]

{ #category : 'tests' }
MyEdgePieceTest >> testKnightAtCornerTargetsOnBoard [
    | board knight targets names |
    board := MyChessBoard empty.
    board at: 'a1' put: (knight := MyKnight white).

    targets := (knight targetSquaresLegal: true) collect: [ :s | s name ].

    "From a1 knight can reach b3 and c2"
    self assert: (targets includes: 'b3').
    self assert: (targets includes: 'c2').
]
