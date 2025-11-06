Class {
    #name : 'MySquareTest',
    #superclass : 'TestCase',
    #category : 'Myg-Chess-Tests',
    #package : 'Myg-Chess-Tests'
}

{ #category : 'tests' }
MySquareTest >> testEmptySquarePlaceholder [
    | board square placeholder |
    board := MyChessBoard empty.
    square := board at: 'a1'.

    "The square contents should be an empty-piece sentinel"
    self assert: (square contents isEmpty) equals: true.

    "Rendering the empty piece on the square returns a single-character placeholder"
    placeholder := square contents renderPieceOn: square.
    self assert: (placeholder size = 1).
]
