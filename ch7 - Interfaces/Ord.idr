record Album where
  constructor MkAlbum
  artist : String
  title : String
  year : Integer

Eq Album where
  (==) (MkAlbum artist title year) (MkAlbum artist' title' year') =
    artist == artist' && title == title' && year == year'

Ord Album where
  compare (MkAlbum artist title year) (MkAlbum artist' title' year') =
    case compare artist artist' of
         EQ => case compare year year' of
                    EQ => case compare title title' of
                               EQ => compare year year'
                               d => d
                    d => d
         d => d

Show Album where
  show (MkAlbum artist title year) =
    title ++ " by " ++ artist ++ " (released " ++ show year ++ ")"

help : Album
help = MkAlbum "The Beatles" "Help" 1965

rubbersoul : Album
rubbersoul = MkAlbum "Joni Mitchell" "Rubber Soul" 1969

clouds : Album
clouds = MkAlbum "David Bowie" "Clouds" 1971

collection : List Album
collection = [help, rubbersoul, clouds]

