-- file: Ch03/bookStore.hs

data BookInfo = Book Int String [String]
                deriving (Show)

data MagazineInfo = Magazine Int String [String]
                    deriving (Show)

myInfo = Book 9780135072455 "Algebra of Programming"
         ["Richard Bird", "Oege de Moor"]

type CustomerID = Int
type ReviewBody = String

-- This definition says that the type named BookReview has a value constructor
-- that is also named BookReview. Not only is it legal for a value constructor
-- to hace the same name as its type constructor, it's normal: you'll see this
-- all the time in regular Haskell Code
data BookReview = BookReview BookInfo CustomerID String

data BetterReview = BetterReview BookInfo CustomerID ReviewBody

type BookRecord = (BookInfo, BookReview)

type CardHolder = String
type CardNumber = String
type Address = [String]

data BillingInfo = CreditCard CardNumber CardHolder Address
                 | CashOnDelivery
                 | Invoice CustomerID
                   deriving (Show)

bookID       (Book id title authors) = id
bookTitle    (Book id title authors) = title
bookAuthors  (Book id title authors) = authors

nicerID      (Book id _  _) = id
nicerTitle   (Book _ title _) = title
nicerAuthors (Book _ _ authors) = authors

data Customer = Customer {
    customerID       :: CustomerID,
    customerName     :: String,
    customerAddress  :: Address
    } deriving (Show)

-- Using the regular application syntax
customer1 = Customer 134256 "D.Z. Knutz"
            ["235 Turnt Ave",
             "L City, NY 10039",
             "USA"]

-- Using Record syntax. More verbose but easier to read because we see what
-- each field stands for/ represents! We also get accessor functions for free!
customer2 = Customer {
            customerID = 234567,
            customerAddress = ["2343 Fucked Nigga St",
                               "The Hood, CA 34576",
                               "USA"],
            customerName = "LaFonda Touches"
            }

-- Also note that using record syntax we can vary the order of the field! as
-- long as they're correctly labelled the compiler will be gucci
-- (you can't mess up other in application syntax!!)











