# Chess Game : Group 10 (Einstein, Toky, Marie)

This is a chess game for Pharo based on Bloc, Toplo and Myg.

## What is this repository really about

The goal of this repository is not to be a complete full blown game, but a good enough implementation to practice software engineering skills:
 - testing
 - reading existing code
 - refactorings
 - profiling
 - debugging

## Installation instructions
### 1. Download and install Pharo
- Go to https://pharo.org/download
- Download the latest Pharo launcher for your operating system (Windows, macOS, or Linux)
- Install and open Pharo

### 2. Load the project baseline
- In a new Pharo image, open the Playground (Ctrl + W) and execute:

```smalltalk
Metacello new
	repository: 'github.com/TokySandratra/Chess:main';
	baseline: 'MygChess';
	onConflictUseLoaded;
	load.
```
- This will automatically load the project code, dependencies, and tests.

### 3. Verify installation
- In the Pharo browser, search for the package by filtering with:  'chess'
- You should see the classes and tests in your image.

### 4. You can open the chess game using the following expression:

```smalltalk
board := MyChessGame freshGame.
board size: 800@600.
space := BlSpace new.
space root addChild: board.
space pulse.
space resizable: true.
space show.
```

## Katas
- Each member of the group 10 have decided to take one Kata per person : 
    - Fix pawn moves : Toky
    - Remove nil checks : Einstein
    - Refactor piece rendering : Marie
      
### Fix pawn moves! (Toky)

### Remove nil checks (Einstein)

### Refactor piece rendering (Marie)


