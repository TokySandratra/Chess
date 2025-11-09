Class {
    #name : 'MyEmptyPiece',
    #superclass : 'MyPiece',
    #category : 'Myg-Chess-Core',
    #package : 'Myg-Chess-Core'
}

{ #category : 'testing' }
MyEmptyPiece >> isEmpty [

    ^ true
]

{ #category : 'testing' }
MyEmptyPiece >> isPiece [

    ^ false
]

{ #category : 'rendering' }
MyEmptyPiece >> renderPieceOn: aSquare [
    "Return the placeholder text for an empty square. This preserves the previous
    short placeholder behavior used when contents were nil ("z" / "x")."

    ^ aSquare color isBlack
        ifFalse: [ 'z' ]
        ifTrue: [ 'x' ]
]

{ #category : 'instance' }
MyEmptyPiece class >> default [
    "Return a shared empty-piece sentinel to avoid allocating many identical objects."
    ^ (Smalltalk at: #MyEmptyPiece defaultInstance ifAbsent: [
        Smalltalk at: #MyEmptyPiece defaultInstance put: (self new).
        Smalltalk at: #MyEmptyPiece defaultInstance ])
]
