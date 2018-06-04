type Name = String

data Date = Date Int Int Int  -- year, month, day

data Anniversary = Birthday Name Date        -- name, year, date
                 | Wedding Name Name Date  -- spouse name 1, spouse name 2, date

type AnniversaryBook = [Anniversary]

johnSmith :: Anniversary
johnSmith = Birthday "John Smith" (Date 1968 7 3)

smithWedding :: Anniversary
smithWedding = Wedding "John Smith" "Jane Smith" (Date 1987 3 4)

anniversariesOfJohnSmith :: AnniversaryBook
anniversariesOfJohnSmith = [johnSmith, smithWedding]

showDate :: Date -> String
showDate (Date y m d) = show y ++ "-" ++ show m ++ "-" ++ show d

showAnniversary :: Anniversary -> String
showAnniversary (Birthday name date) =
    name ++ " born " ++ showDate date
showAnniversary (Wedding name1 name2 date) = 
    name1 ++ " married " ++ name2 ++ " on " ++ showDate date
