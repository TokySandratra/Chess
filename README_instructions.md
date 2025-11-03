# Chess Game Group 10 (Einstein, Toky, Marie)

This is a chess game for Pharo based on Bloc, Toplo and Myg.

## What is this repository really about

The goal of this repository is not to be a complete full blown game, but a good enough implementation to practice software engineering skills:
 - testing
 - reading existing code
 - refactorings
 - profiling
 - debugging

## Getting started

### Getting the code

This code has been tested in Pharo 12. You can get it by installing the following baseline code:

```smalltalk
Metacello new
	repository: 'github://UnivLille-Meta/Chess:main';
	baseline: 'MygChess';
	onConflictUseLoaded;
	load.
```

### Using it

You can open the chess game using the following expression:

```smalltalk
board := MyChessGame freshGame.
board size: 800@600.
space := BlSpace new.
space root addChild: board.
space pulse.
space resizable: true.
space show.
```

## Relevant Design Points

This repository contains:
 - a chess model: the board/squares, the pieces, their movements, how they threat each other
 - a UI using Bloc and Toplo: a board is rendered as bloc UI elements. Each square is a UI element that contains a selection, an optional piece. Pieces are rendered using a text element and a special chess font (https://github.com/joshwalters/open-chess-font/tree/master).
 - Textual game importers for the PGN and FEN standards (see https://ia902908.us.archive.org/26/items/pgn-standard-1994-03-12/PGN_standard_1994-03-12.txt and https://www.chessprogramming.org/Forsyth-Edwards_Notation#Samples)

## Katas

These are some ideas of exercises you may try:

### Fix pawn moves! (Toky)

### Remove nil checks (Einstein)

### Refactor piece rendering (Marie)


