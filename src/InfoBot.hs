{-# LANGUAGE OverloadedStrings #-}

module InfoBot
    ( nameFromListing
    ) where

import           Data.Monoid

import qualified Data.Text             as T
import qualified EuphApi.Types         as E
import qualified EuphApi.Utils.Listing as EL

count :: (E.SessionView -> Bool) -> EL.Listing -> Int
count f = length . filter f . EL.toList

hasNick :: E.SessionView -> Bool
hasNick = not . T.null . E.sessName

isPeople :: E.SessionView -> Bool
isPeople s =
  case E.userType $ E.sessID s of
    E.Bot -> False
    _     -> True

people :: EL.Listing -> Int
people = count (\s -> hasNick s && isPeople s)

bots :: EL.Listing -> Int
bots = count (\s -> hasNick s && not (isPeople s))

lurkers :: EL.Listing -> Int
lurkers = count (\s -> not (hasNick s) && isPeople s)

botLurkers :: EL.Listing -> Int
botLurkers = count (\s -> not (hasNick s) && not (isPeople s))

nameFromListing :: EL.Listing -> T.Text
nameFromListing listing =
  let tshow = T.pack . show
      format f s = if f listing > 0 then [tshow (f listing) <> s] else []
      p = format people     "P"
      b = [tshow (bots listing + 1) <> "B"]
      l = format lurkers    "L"
      n = format botLurkers "N"
      info = p ++ b ++ l ++ n
  in  "\SOH(" <> (T.intercalate " " info) <> ")"
