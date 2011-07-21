{-# LANGUAGE OverloadedStrings #-}
module Data.Iter.Text where

import Data.Iter

import Data.Text (Text)
import qualified Data.Text as T

type IText = Iter Text

ilines :: IText -> Iter IText
ilines = undefined

iwords :: IText -> Iter IText
iwords = undefined

ilines' :: IText -> IText
ilines' = undefined

iwords' :: IText -> IText
iwords' = undefined

isplitOn :: Text -> IText -> Iter IText
isplitOn sep = go (return T.empty) where
    split   = T.splitOn sep
    go rest = ifEmpty (return rest) $ \i -> do
        (a, i') <- next i
        let (xs,x) = buncons $ split a
        if null xs
            then go (rest +++ return x) i'
            else do
                let (x':xs') = xs
                (rest +++ return x') !::
                    iterList (map return xs') +++ go (return x) i'

toText :: IText -> IO Text
toText = fmap (T.concat) . toList

buncons :: [a] -> ([a], a)
buncons [x] = ([], x)
buncons (x:xs) = let (xs', x') = buncons xs in (x:xs', x')
