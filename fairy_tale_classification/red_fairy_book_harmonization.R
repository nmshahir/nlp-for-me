################################################################
# Name: red_fairy_book_harmonization.R
# Purpose: Joining fairy books stories to their
# country of origin and AT number
# Creator: N. M. Shahir
# Github: nmshahir
# Data source: University of Missouri Libraries
# https://libraryguides.missouri.edu/c.php?g=1078942&p=7861219
# Data Obtained: 2026.03.10
# Date Created: 2026.06.23
#################################################################

library(datapasta)
library(dplyr)
library(tidyverse)

red_fairy_origins <- tibble::tribble(
  ~Tale                                , ~AT                 , ~Culture       ,
  "Black Thief and Knight of Glen"     , "NA"                , "Irish"        ,
  "Brother and Sister"                 , "450"               , "German"       ,
  "Bushy Bride"                        , "403"               , "Norwegian"    ,
  "Dapplegrim"                         , "NA"                , "Norwegian"    ,
  "Death of Koshchei thee Deathless"   , "552"               , "Slavic"       ,
  "Drakestail"                         , "715"               , "French"       ,
  "Enchanted Canary"                   , "NA"                , "French"       ,
  "Enchanted Pig"                      , "425A"              , "Romanian"     ,
  "Farmer WeatherBeard"                , "325"               , "Norwegian"    ,
  "Golden Branch"                      , "NA"                , "French"       ,
  "Golden Goose"                       , "571"               , "German"       ,
  "Graciosa and Percinet"              , "NA"                , "French"       ,
  "Jack and the Beanstalk"             , "328"               , "Unattributed" ,
  "Kari Woodengown"                    , "510A"              , "Norwegian"    ,
  "Little Good Mouse"                  , "NA"                , "French"       ,
  "Marvellous Musician"                , "151"               , "German"       ,
  "Minnikin"                           , "NA"                , "Norwegian"    ,
  "Mother Holle"                       , "480"               , "German"       ,
  "Nettle Spinner"                     , "NA"                , "French"       ,
  "Norka"                              , "301"               , "Unattributed" ,
  "Princess Mayblossom"                , "NA"                , "French"       ,
  "Princess Rosette"                   , "NA"                , "French"       ,
  "Rapunzel"                           , "310"               , "German"       ,
  "Ratcatcher"                         , "570*"              , "French"       ,
  "Seven Foals"                        , "NA"                , "Norwegian"    ,
  "Six Sillies"                        , "1229*, 1384, 1450" , "Belgian?"     ,
  "Snowdrop"                           , "709"               , "German"       ,
  "Soria Moria Castle"                 , "NA"                , "Norwegian"    ,
  "Story of Sigurd"                    , "NA"                , "Icelandic"    ,
  "The Master Thief"                   , "1525A"             , "Norwegian"    ,
  "Three Dwarfs"                       , "403B"              , "German"       ,
  "Three Princesses of Whiteland"      , "NA"                , "Norwegian"    ,
  "True History of Little Golden Hood" , "333"               , "French"       ,
  "Twelve Brothers"                    , "451"               , "German"       ,
  "Twelve Dancing Princesses"          , "306"               , "Unattributed" ,
  "Voice of Death"                     , "NA"                , "Romanian"     ,
  "Wonderful Birch"                    , "NA"                , "Russian"
)
