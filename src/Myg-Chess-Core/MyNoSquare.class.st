Class {
    #name : 'MyNoSquare',
    #superclass : 'Object',
    #category : 'Myg-Chess-Core',
    #package : 'Myg-Chess-Core'
}

{ #category : 'query' }
MyNoSquare >> isOnBoard [
    ^ false
]

{ #category : 'path commands' }
MyNoSquare >> right [
    ^ self
]

{ #category : 'path commands' }
MyNoSquare >> left [
    ^ self
]

{ #category : 'path commands' }
MyNoSquare >> up [
    ^ self
]

{ #category : 'path commands' }
MyNoSquare >> down [
    ^ self
]

{ #category : 'accessing' }
MyNoSquare >> hasPiece [
    "Treat off-board as blocking: behaves like occupied"
    ^ true
]

{ #category : 'accessing' }
MyNoSquare >> contents [
    ^ MyEmptyPiece default
]

{ #category : 'rendering' }
MyNoSquare >> renderPieceOn: aSquare [
    ^ aSquare color isBlack ifFalse: [ 'z' ] ifTrue: [ 'x' ]
]

{ #category : 'highlighting' }
MyNoSquare >> highlightTarget [
    "No-op for off-board sentinel"
]

{ #category : 'highlighting' }
MyNoSquare >> unhighlightTarget [
    "No-op"
]
