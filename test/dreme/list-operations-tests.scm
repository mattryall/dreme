(
(reverseEmpty
    (reverse (quote ()))
    ())
(reverseList
    (reverse (quote (1 2 3)))
    (3 2 1))
(reverseNestedLists
    (reverse (quote (1 (2 3) 4)))
    (4 (2 3) 1))
(appendEmptyToEmpty
    (append (quote ()) (quote ()))
    ())
(appendEmptyToList
    (append (quote (1 2 3)) (quote ()))
    (1 2 3))
(appendListToEmpty
    (append (quote ()) (quote (1 2 3)))
    (1 2 3))
(appendListToList
    (append (quote (1 2 3)) (quote (4 5 6)))
    (1 2 3 4 5 6))
(appendThreeLists
    (append (quote (1 2)) (quote (3 4)) (quote (5 6)))
    (1 2 3 4 5 6))
(appendTwoListsAndEmpty
    (append (quote (1 2)) (quote ()) (quote (3 4)))
    (1 2 3 4))
(appendTwoListsAndAnAtom
    (append (quote (1 2)) 3)
    (1 2 . 3))
)