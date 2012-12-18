haskell-club
============

The first Rule of Haskell Club is.....

Read http://learnyouahaskell.com/

Homework Schedule:

- Week 0 (10 Dec)
Read Chapters 1, 2 + 3 of http://learnyouahaskell.com/chapters by next class - *Monday 17 December*

- Week 1 (17 Dec)
Read 4 and 5 of http://learnyouahaskell.com/chapters by next session, which will be on *Monday 3 January*

Also, for the next session, we change the format as follows: we announce the problem to be solved ahead of the meeting, everyone has a go at it beforehand, and then we all solve it dojo-style during the session.

Next session's problem is not chosen yet!  It will be one of the following: (add your suggestions, please...)
	
- Implement a Xsort, where X is in Quick, Merge, Bubble, Heap, Shell, Bogo... (but QuickSort is the example in Learn You A Haskell, so maybe not that.)  See http://www.youtube.com/watch?v=XaqR3G_NVoo for MergeSort expressed through the medium of Transylvanian-saxon folk dance.

- Countdown numbers game.  Given a list of 6 integers and a 7th integer, find a way of inserting the arithmetic operators between the elements of the list such that when the resulting expression is evaluated, the result is the 7th integer.  E.g. given [1,2,5,8,9,25] and 272, a solution is 25×(5+2+1)+8×9.  This problem is a special case of problem 93 of 99 problems: http://www.haskell.org/haskellwiki/99_questions/90_to_94

- Chess board problem.  Given an NxM chess board, and a collection of standard chess pieces, enumerate the ways in which the pieces can be placed on the board such that no piece is in a position to take another.  Ignore the colour of the pieces, and do not include pawns in the collection.  E.g. for input N=3, M=3, collection of 2 kings and 1 rook, the output is:

```
+-+-+-+    +-+-+-+
|K| |K|    |K| | |
+-+-+-+    +-+-+-+
| | | |    | | |R|
+-+-+-+    +-+-+-+
| |R| |    |K| | |
+-+-+-+    +-+-+-+
```

Any representation of the output is acceptable; ASCII-art is not required (or even encouraged...)




