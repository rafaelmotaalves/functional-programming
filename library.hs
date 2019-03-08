type Person = String
type Book = String
type Database = [(Person, Book)]

exDb :: Database
exDb = [("Sergio", "O Senhor dos Aneis"), ("Andre", "Duna") ,("Fernando", "Jonathan Strange & Mr.Norrell"),("Fernando", "Duna") ]

books :: Database -> Person -> [Book]
books db person = [snd x | x <- db, fst x == person]

borrowedBooks :: Database -> Book -> [Person]
borrowedBooks db book = [fst x | x <- db, snd x == book]

isBorrowed :: Database -> Book -> Bool
isBorrowed [] _ = False
isBorrowed (head:tail) book
 | (snd head) == book = True
 | otherwise = isBorrowed tail book 

borrowedQuantity :: Database -> Person -> Int
borrowedQuantity [] _ = 0
borrowedQuantity (head:tail) person
 | (fst head) == person = 1 + borrowedQuantity tail person
 | otherwise = borrowedQuantity tail person

borrow :: Database -> Person -> Book -> Database
borrow db person book = (person, book) : db

giveBack :: Database -> Person -> Book -> Database
giveBack db person book = [ x | x <- db, (fst x) /= person || (snd x) /= book ]