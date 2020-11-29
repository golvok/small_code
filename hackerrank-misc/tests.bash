( for TCASE in "aaa" "aaabbb" "aa" "aabb" "aaabbccc"; do echo -n "$TCASE -> "; runghc String_Compression.hs <<< "$TCASE" || exit 1; done )

( for TCASE in "+-++++++++
+-++++++++
+-++++++++
+-----++++
+-+++-++++
+-+++-++++
+++++-++++
++------++
+++++-++++
+++++-++++
LONDON;DELHI;ICELAND;ANKARA" "+-++++++++
+-++++++++
+-------++
+-++++++++
+-++++++++
+------+++
+-+++-++++
+++++-++++
+++++-++++
++++++++++
AGRA;NORWAY;ENGLAND;GWALIOR"; do echo -e "$TCASE\n<<<<>>>> "; runghc Crosswords-101.hs <<< "$TCASE"; echo "=======" ; done )

( for TCASE in "42354
((. X (. . .)) . (X . (. X X)))
6
0 []
2 [><]
0 [><]
0 [<>]
1 [><]
0 [<>]
-------
.
X
X
.
X
X"; do echo -e "$TCASE\n<<<<>>>> "; runghc The_Tree_Of_Life.hs <<< "$TCASE"; echo "=======" ; done )