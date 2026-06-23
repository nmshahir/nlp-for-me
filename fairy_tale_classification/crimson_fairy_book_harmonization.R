################################################################
# Name: crimson_fairy_book_harmonization.R
# Purpose: Joining fairy books stories to their
# country of origin and AT number
# Creator: N. M. Shahir
# Github: nmshahir
# Data source: University of Missouri Libraries
# https://libraryguides.missouri.edu/c.php?g=1078942&p=7861219
# Data Obtained: 2026.03.10
# Date Created: 2026.06.23
#################################################################

crimson_fairy_origins <- tibble::tribble(
  ~Tale                                         , ~AT                       , ~Country       ,
  "Boy Who Could Keep a Secret"                 , "NA"                      , "Hungarian"    ,
  "Clever Maria"                                , "NA"                      , "Portuguese"   ,
  "Colony of Cats"                              , "NA"                      , "Unattributed" ,
  "Cottager and His Cat"                        , "1651"                    , "Icelandic"    ,
  "Crab and Money"                              , "NA"                      , "Japanese"     ,
  "Death of Abu Nowas and of his Wife"          , "NA"                      , "African"      ,
  "Eisenkopf"                                   , "NA"                      , "Hungarian"    ,
  "Gifts of the Magician"                       , "NA"                      , "Finnish"      ,
  "Gold-Bearded Man"                            , "502"                     , "Hungarian"    ,
  "Hairy Man"                                   , "502"                     , "Russiann"     ,
  "Horse Gullfaxi and Sword Gunnfoder"          , "NA"                      , "Icelandic"    ,
  "How the Beggar Turned into Count Piro"       , "545B"                    , "Italian"      ,
  "How the Wicked Tanuki was Punished"          , "NA"                      , "Japanese"     ,
  "How to Find Out a True Friend"               , "NA"                      , "Italian"      ,
  "Language of Beasts"                          , "670"                     , "Unattributed" ,
  "Little Wildrose"                             , "NA"                      , "Romanian"     ,
  "Lovely Ilonka"                               , "408"                     , "Hungarian"    ,
  "Lucky Luck"                                  , "460B"                    , "Hungarian"    ,
  "Magic Kettle"                                , "NA"                      , "Japanese"     ,
  "Motiratika"                                  , "NA"                      , "African"      ,
  "Niels and the Giants"                        , "304"                     , "Unattributed" ,
  "Paperarelloo"                                , "314"                     , "Italian"      ,
  "Prince and Dragon"                           , "302"                     , "Serbian"      ,
  "Prince Who Would Seek Immortality"           , "NA"                      , "Hungarian"    ,
  "Rogues and Herdsman"                         , "NA"                      , "Icelandic"    ,
  "Shepherd Paul"                               , "301"                     , "Hungarian"    ,
  "Six Hungry Beasts"                           , "20, 20B, 37, 56A, 1310C" , "Finnish"      ,
  "Stone-Cutter"                                , "555"                     , "Japanese"     ,
  "Story of Seven Simons"                       , "513B"                    , "Hungarian"    ,
  "Story of the Sham Prince or Ambitous Tailor" , "NA"                      , "Unattributed" ,
  "Strong Prince"                               , "590"                     , "Hungarian"    ,
  "Three Robes"                                 , "403"                     , "Icelandic"    ,
  "Tiidu The Piper"                             , "566"                     , "Estonian"     ,
  "To Your Good Health!"                        , "858"                     , "Russiann"     ,
  "Treasure Seeker"                             , "NA"                      , "Unattributed" ,
  "Tritill, Litill, and the Birds"              , "401A"                    , "Hungarian"
)

#Get Countries of Origins - 12 but Russian is spelled "Russiann"
unique(crimson_fairy_origins$Country)
crimson_origin_titles <- unique(crimson_fairy_origins$Tale)

#Ten Countries at the moment including unattributed?
crimson_fairy_stories_only <- readRDS(file.choose())

crimson_fairy_tales <- unique(crimson_fairy_stories_only$story)
#See which ones are due to captialization vs actual character mismatch

setdiff(crimson_origin_titles, crimson_fairy_tales)

#  [1] "Boy Who Could Keep a Secret"                 "Colony of Cats"                              "Cottager and His Cat"
#  [4] "Crab and Money"                              "Death of Abu Nowas and of his Wife"          "Gifts of the Magician"
#  [7] "Gold-Bearded Man"                            "Hairy Man"                                   "Horse Gullfaxi and Sword Gunnfoder"
# [10] "How the Beggar Turned into Count Piro"       "How the Wicked Tanuki was Punished"          "How to Find Out a True Friend"
# [13] "Language of Beasts"                          "Magic Kettle"                                "Niels and the Giants"
# [16] "Prince and Dragon"                           "Prince Who Would Seek Immortality"           "Rogues and Herdsman"
# [19] "Six Hungry Beasts"                           "Stone-Cutter"                                "Story of Seven Simons"
# [22] "Story of the Sham Prince or Ambitous Tailor" "Strong Prince"                               "Three Robes"
# [25] "Treasure Seeker"                             "Tritill, Litill, and the Birds"
#See which ones are due to captialization vs actual character mismatch
setdiff(toupper(crimson_origin_titles), toupper(crimson_fairy_tales))

#  [1] "BOY WHO COULD KEEP A SECRET"                 "COLONY OF CATS"                              "COTTAGER AND HIS CAT"
#  [4] "CRAB AND MONEY"                              "DEATH OF ABU NOWAS AND OF HIS WIFE"          "GIFTS OF THE MAGICIAN"
#  [7] "GOLD-BEARDED MAN"                            "HAIRY MAN"                                   "HORSE GULLFAXI AND SWORD GUNNFODER"
# [10] "HOW THE BEGGAR TURNED INTO COUNT PIRO"       "LANGUAGE OF BEASTS"                          "MAGIC KETTLE"
# [13] "PRINCE AND DRAGON"                           "PRINCE WHO WOULD SEEK IMMORTALITY"           "ROGUES AND HERDSMAN"
# [16] "SIX HUNGRY BEASTS"                           "STONE-CUTTER"                                "STORY OF SEVEN SIMONS"
# [19] "STORY OF THE SHAM PRINCE OR AMBITOUS TAILOR" "STRONG PRINCE"                               "THREE ROBES"
# [22] "TREASURE SEEKER"

#Editing the table but making a copy instead of tweaking the original

crimson_fairy_origins_edits <- tibble::tribble(
  ~Tale                                                   , ~AT                       , ~Country       ,
  "The Boy Who Could Keep a Secret"                       , "NA"                      , "Hungarian"    ,
  "Clever Maria"                                          , "NA"                      , "Portuguese"   ,
  "The Colony of Cats"                                    , "NA"                      , "Unattributed" ,
  "The Cottager and His Cat"                              , "1651"                    , "Icelandic"    ,
  "The Crab and the Monkey"                               , "NA"                      , "Japanese"     ,
  "The Death of Abu Nowas and of his Wife"                , "NA"                      , "African"      ,
  "Eisenkopf"                                             , "NA"                      , "Hungarian"    ,
  "The Gifts of the Magician"                             , "NA"                      , "Finnish"      ,
  "The Gold-Bearded Man"                                  , "502"                     , "Hungarian"    ,
  "The Hairy Man"                                         , "502"                     , "Russiann"     ,
  "The Horse Gullfaxi and the Sword Gunnfoder"            , "NA"                      , "Icelandic"    ,
  "How the Beggar Boy Turned into Count Piro"             , "545B"                    , "Italian"      ,
  "How the Wicked Tanuki was Punished"                    , "NA"                      , "Japanese"     ,
  "How to Find Out a True Friend"                         , "NA"                      , "Italian"      ,
  "The Language of Beasts"                                , "670"                     , "Unattributed" ,
  "Little Wildrose"                                       , "NA"                      , "Romanian"     ,
  "Lovely Ilonka"                                         , "408"                     , "Hungarian"    ,
  "Lucky Luck"                                            , "460B"                    , "Hungarian"    ,
  "The Magic Kettle"                                      , "NA"                      , "Japanese"     ,
  "Motiratika"                                            , "NA"                      , "African"      ,
  "Niels and the Giants"                                  , "304"                     , "Unattributed" ,
  "Paperarelloo"                                          , "314"                     , "Italian"      ,
  "The Prince and the Dragon"                             , "302"                     , "Serbian"      ,
  "The Prince Who Would Seek Immortality"                 , "NA"                      , "Hungarian"    ,
  "The Rogue and the Herdsman"                            , "NA"                      , "Icelandic"    ,
  "Shepherd Paul"                                         , "301"                     , "Hungarian"    ,
  "The Six Hungry Beasts"                                 , "20, 20B, 37, 56A, 1310C" , "Finnish"      ,
  "The Stone-Cutter"                                      , "555"                     , "Japanese"     ,
  "The Story of the Seven Simons"                         , "513B"                    , "Hungarian"    ,
  "The Story of the Sham Prince, or the Ambitious Tailor" , "NA"                      , "Unattributed" ,
  "The Strong Prince"                                     , "590"                     , "Hungarian"    ,
  "The Three Robes"                                       , "403"                     , "Icelandic"    ,
  "Tiidu The Piper"                                       , "566"                     , "Estonian"     ,
  "To Your Good Health!"                                  , "858"                     , "Russiann"     ,
  "The Treasure Seeker"                                   , "NA"                      , "Unattributed" ,
  "Tritill, Litill, and the Birds"                        , "401A"                    , "Hungarian"
)

crimson_origin_titles <- unique(crimson_fairy_origins_edits$Tale)
setdiff(toupper(crimson_origin_titles), toupper(crimson_fairy_tales))

crimson_fairy_book_origins <- saveRDS(
  crimson_fairy_origins_edits,
  "crimson_fairy_book_origins.rds"
)
