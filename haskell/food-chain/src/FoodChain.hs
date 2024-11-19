module FoodChain where

import Control.Monad
import Data.Char
import Data.List
import Text.Printf

data Animal = Fly | Spider | Bird | Cat | Dog | Goat | Cow | Horse deriving (Eq, Ord, Enum, Show)

iKnowAnOldLady :: Animal -> String
iKnowAnOldLady animal = printf "I know an old lady who swallowed a %s." (map toLower (show animal))

react :: Animal -> [String]
react Fly = []
react Spider = ["It wriggled and jiggled and tickled inside her."]
react Bird = ["How absurd to swallow a bird!"]
react Cat = ["Imagine that, to swallow a cat!"]
react Dog = ["What a hog, to swallow a dog!"]
react Goat = ["Just opened her throat and swallowed a goat!"]
react Cow = ["I don't know how she swallowed a cow!"]
react Horse = []

swallows :: Animal -> [String]
swallows Fly = ["I don't know why she swallowed the fly. Perhaps she'll die."]
swallows Spider = "She swallowed the spider to catch the fly." : swallows Fly
swallows Bird = "She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her." : swallows Spider
swallows Cat = "She swallowed the cat to catch the bird." : swallows Bird
swallows Dog = "She swallowed the dog to catch the cat." : swallows Cat
swallows Goat = "She swallowed the goat to catch the dog." : swallows Dog
swallows Cow = "She swallowed the cow to catch the goat." : swallows Goat
swallows Horse = ["She's dead, of course!"]

paragraph :: Animal -> [String]
paragraph animal = iKnowAnOldLady animal : react animal ++ swallows animal

song :: String
song = join (intersperse "\n\n" $ map (join . intersperse "\n" . paragraph) [Fly .. Horse]) ++ "\n"
