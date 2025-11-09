Class {
    #name : 'MyNoSelection',
    #superclass : 'Object',
    #category : 'Myg-Chess-Core',
    #package : 'Myg-Chess-Core'
}

{ #category : 'testing' }
MyNoSelection >> isEmpty [
    ^ true
]

{ #category : 'actions' }
MyNoSelection >> doUnselect [
    "No-op when nothing is selected"
]
Class {
    #name : 'MyNoSelection',
    #superclass : 'Object',
    #category : 'Myg-Chess-Core',
    #package : 'Myg-Chess-Core'
}

{ #category : 'testing' }
MyNoSelection >> isEmpty [

    ^ true
]

{ #category : 'actions' }
MyNoSelection >> doUnselect [
    "No-op for the empty selection sentinel"

    ^ self
]
