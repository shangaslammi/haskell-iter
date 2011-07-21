
module Data.Iter.Text where

import Data.Text

type IText = Iter Text

ilines :: IText -> Iter IText
ilines = undefined

iwords :: IText -> Iter IText
iwords = undefined

ilines' :: IText -> IText
ilines' = undefined

iwords' :: IText -> IText
iwords' = undefined


toText :: IText -> IO Text
toText = undefined
