record Album where
  constructor MkAlbum
  artist : String
  title : String
  year : Integer

help : Album
help = MkAlbum "The Beatles" "Help" 1965

rubbersoul : Album
rubbersoul = MkAlbum "The Beatles" "Rubber Soul" 1965

clouds : Album
clouds = MkAlbum "Joni Mitchell" "Clouds" 1969

hunkydory : Album
hunkydory = MkAlbum "David Bowie" "Hunky Dory" 1971

heroes : Album
heroes = MkAlbum "David Bowie" "Heroes" 1977

collection : List Album
collection = [help, rubbersoul, clouds, hunkydory, heroes]

Eq Album where
  (==) (MkAlbum artist1 title1 year1) (MkAlbum artist2 title2 year2)
        = artist1 == artist2 && title1 == title2 && year1 == year2

Ord Album where
  compare (MkAlbum artist1 title1 year1) (MkAlbum artist2 title2 year2)
            = case compare artist1 artist2 of
                   EQ => case compare title1 title2 of
                              EQ => compare year1 year2
                              diff_title => diff_title
                   diff_artist => diff_artist
