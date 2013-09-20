
module CV.Paper where

type Key   = String
type Name  = String
type Title = String
type URL   = String
type Year  = Int

data Kind = Journal | Chapter | Conference | Workshop | Thesis
          | Report | Consortium | DraftPaper
  deriving (Eq,Enum,Show)

data Status = Appeared | Accepted | Submitted | Draft
  deriving (Eq,Enum,Show)

data Author = Author {
  _firstName :: Name,
  _lastName  :: Name
} deriving (Eq,Show)

data Pages = Pages   Int Int
           | PagesIn Int Int Int
  deriving (Eq,Show)


-- ** Papers

data Paper = Paper {
  _status  :: Status,
  _kind    :: Kind,
  _key     :: Key,
  _authors :: [Author],
  _title   :: Title,
  _year    :: Year,
  _venue   :: Maybe Venue,
  _pages   :: Maybe Pages,
  _url     :: Maybe URL,
  _note    :: Maybe String
} deriving (Eq,Show)

-- Minimum definition.
paper s k e a t y = Paper s k e a t y Nothing Nothing Nothing Nothing

draft     = paper Draft DraftPaper 
appeared  = paper Appeared
accepted  = paper Accepted
submitted = paper Submitted

journal    = appeared Journal
chapter    = appeared Chapter
conference = appeared Conference
workshop   = appeared Workshop
thesis     = appeared Thesis


-- Optional field setters.
infix 3 @@
p @@ a       = p { _venue = Just a }
onPages  p a = p { _pages = Just a }
atURL    p a = p { _url   = Just a }
withNote p a = p { _note  = Just a }


-- ** Venues

data Venue = Venue {
  _longName  :: Name,
  _shortName :: Maybe Name,
  _publisher :: Maybe Name,
  _editors   :: Maybe [Author],
  _volume    :: Maybe Int,
  _number    :: Maybe Int,
  _series    :: Maybe (Name,Int)
} deriving (Eq,Show)

-- Minimum definition.
venue l = Venue l Nothing Nothing Nothing Nothing Nothing Nothing
short s l = venue l `setShortName` s

-- Optional field setters.
setShortName v a = v { _shortName = Just a }
setPublisher v a = v { _publisher = Just a }
setEditor    v a = v { _editors   = Just a }
setVolume    v a = v { _volume    = Just a }
setNumber    v a = v { _number    = Just a }
setSeries    v a = v { _series    = Just a }
