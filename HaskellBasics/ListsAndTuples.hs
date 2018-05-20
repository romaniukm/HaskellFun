cons8 lst = 8:lst

append8 lst = lst ++ [8]

myCons lst thing = thing:lst

headAndTail lst = (head lst, tail lst)

fifthElem lst = head (tail (tail (tail (tail lst))))
