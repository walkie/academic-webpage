
module CV.Paper where

type Key   = String
type Name  = String
type Title = String
type URL   = String
type Year  = Int

data Paper
  = Paper {
      key     :: Key,
      authors :: [Author],
      title   :: Title,
      year    :: Year,
      venue   :: Maybe Venue,
      pages   :: Maybe Pages,
      url     :: Maybe URL,
      note    :: Maybe String
    }
  deriving (Eq,Show)

data Author
  = Author {
      firstName :: Name,
      lastName  :: Name
    }
  deriving (Eq,Show)

data Venue
  = Venue {
      venueName :: Name,
      venueKind :: VenueKind
    }
  deriving (Eq,Show)

data VenueKind 
  = Conference {
      conferenceAbbr   :: Name,
      conferenceSeries :: Maybe (Name,Int)
    }
  | Journal {
      journalAbbr    :: Name,
      journalVolume  :: Maybe Int,
      journalNumber  :: Maybe Int
    }
  | Chapter {
      bookEditors   :: [Author],
      bookPublisher :: Name
    }
  | PhdThesis
  | MsThesis
  deriving (Eq,Show)

data Pages
  = Pages   Int Int
  | PagesIn Int Int Int
  deriving (Eq,Show)


-- ** Smart constructors

-- | A paper with the minimum properties set.
paper :: Key -> [Author] -> Title -> Year -> Paper
paper k as t y = Paper k as t y Nothing Nothing Nothing Nothing

-- | Set the venue of a paper.
atVenue :: Paper -> Venue -> Paper
atVenue p v = p { venue = Just v }

-- | Set the pages of a paper.
onPages :: Paper -> Pages -> Paper
onPages p pp = p { pages = Just pp }

-- | Set the URL of a paper.
atURL :: Paper -> URL -> Paper
atURL p u = p { url = Just u }

-- | Set the note of a paper.
withNote :: Paper -> String -> Paper
withNote p n = p { note = Just n }

-- | Draft paper.
draft :: Key -> [Author] -> Title -> Year -> Paper
draft k a t y = paper k a t y `withNote` "Draft paper"

-- | Paper under review.
submitted :: Key -> [Author] -> Title -> Year -> Venue -> Paper
submitted k a t y v = paper k a t y `atVenue` v `withNote` "Under review"

-- | Accepted but not yet appeared.
accepted :: String -> [Author] -> Title -> Year -> Venue -> Paper
accepted k a t y v = paper k a t y `atVenue` v `withNote` "To appear"

-- | Paper that has already appeared, with no page numbers.
appearedNoPages :: String -> [Author] -> Title -> Year -> Venue -> Paper
appearedNoPages k a t y v = paper k a t y `atVenue` v

-- | Paper that has already appeared.
appeared :: String -> [Author] -> Title -> Year -> Venue -> Pages -> Paper
appeared k a t y v p = appearedNoPages k a t y v `onPages` p

