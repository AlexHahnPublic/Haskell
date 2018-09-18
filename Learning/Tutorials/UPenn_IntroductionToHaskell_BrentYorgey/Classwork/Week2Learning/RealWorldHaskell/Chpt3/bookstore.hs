data BookInfo = Book Int String [String]
    deriving (Show)

data MagazineInfo = Magazine Int String [String]
    deriving (Show)

myInfo = Book 23462346 "MyTitle" ["FN LN", "FF, LL"]

type CustomerID = Int

type ReviewBody = String

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

-- Extracting values from an algebraic data type is easy with pattern matching
bookID (Book id title authors) = id
bookTitle (Book id title authors) = title
bookAuthors (Book id title authors) = authors

-- With wildcards
nicerID (Book id _ _ ) = id
nicerTitle (Book _ title _) = title
nicerAuthors (Book _ _ authors) = authors

data Customer = Customer {
    customerID :: CustomerID,
    customerName :: String,
    customerAddress :: Address
    } deriving (Show)

customer1 = Customer 2345 "cust name" ["234 street","place"]

-- using record syntax, may be more readable at the cost of verbosity
customer2 = Customer {
    customerID = 5436,
    customerAddress = ["234","street","place"],
    customerName = "yonathan"
    }


