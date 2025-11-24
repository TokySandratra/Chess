Class {
    #name :'MyNoSquareTest',
    #superclass: 'TestCase',
    #category: 'Myg-Chess-Tests',
    #package : 'Myg-Chess-Tests'
}

 { #category :'tests' }
    MyNoSquareTest >> testIsOnBoardFalse [
        self assert: (MyNoSquare new isOnBoard) equals: false.
    ]

{ #category :'tests' }
MyNoSquareTest >> testMovementReturnsSelf [
     | ns |
    ns := MyNoSquare new.

    self assert: (ns right = ns).
     self assert: (ns left = ns).
    self assert: (ns up = ns).
    self assert: (ns down = ns).
]


{ #category : 'tests' }
MyNoSquareTest >> testHasPieceAndContentsEmpty [
    | ns |

    ns := MyNoSquare new.
    self assert: ns hasPiece.
    self assert: (ns contents isEmpty) equals: true.
]

{ #category : 'tests' }
MyNoSquareTest >> testRenderPieceReturnsPlaceholder [
     | ns square placeholder |

    ns := MyNoSquare new.
    square := MyChessSquare white.
    placeholder := ns renderPieceOn: square.
    self assert: (placeholder size = 1).
]

 { #category : 'tests' }
MyNoSquareTest >> testHighlightNoOp [
    | ns |

    ns := MyNoSquare new.
    "Shouldn't raise"
    ns highlightTarget.
    ns unhighlightTarget.
    self assert: true.
]
