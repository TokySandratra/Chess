Class {
	#name : 'WhiteQueenTest',
	#superclass : 'TestCase',
	#category : 'Myg-Chess-Tests',
	#package : 'Myg-Chess-Tests'
}

{ #category : 'tests' }
WhiteQueenTest >> testRenderOnWhiteSquare [

self assert: WhiteQueen new renderOnWhiteSquare equals: 'Q'.
]
