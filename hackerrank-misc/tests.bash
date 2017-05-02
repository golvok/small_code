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
