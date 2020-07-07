module Main where

import Prelude

import Data.Enum (toEnum)
import Data.List (List(..), fromFoldable, (:))
import Data.Maybe (fromJust)
import Data.Time (Time(..), adjust)
import Data.Time.Duration
import Data.Traversable (scanr, scanl)
import Data.Tuple (Tuple(..))

import Effect (Effect)
import Effect.Console (log)

import Partial.Unsafe (unsafePartial)
import Data.Generic.Rep
import Data.Generic.Rep.Show

main :: Effect Unit
main = do
  let start9am = hhmm 9 0
  let talks = fromFoldable
        [Detailed Talk { description: "Talk1", duration: (Minutes 20.0)}
        ,Detailed Panel { description: "Panel", duration: (Minutes 40.0)}
        ,Detailed Talk { description: "Talk2", duration: (Minutes 20.0)}]
  log $ show $ schedule talks start9am

data Segment = Window 
             | Break

data Session = Talk
             | Panel

derive instance genericSession :: Generic Session _
instance showSession :: Show Session where
  show = genericShow

data Detailed a = Detailed a { description :: String
                             , duration :: Minutes }

derive instance genericDetailed :: Generic (Detailed a) _
instance showDetailed :: Show a => Show (Detailed a) where
  show = genericShow

details (Detailed _ d) = d

data Scheduled a = Scheduled (Detailed a) { startTime :: Time }

derive instance genericScheduled :: Generic (Scheduled a) _
instance showScheduled :: Show a => Show (Scheduled a) where
  show = genericShow

duration (Scheduled d _) = (details d).duration
startTime (Scheduled _ t) = t.startTime

{- "Segments" are the structure of a track day. e.g. "Morning 1" section, or "Lunch break".
   "Sessions" are the talks or other content that needs to be slotted into these Segments (into the Windows specifically)
   
Both these types can be scheduled. e.g.:

  Track (starting 9am)
   -> Window (3 hours)
   -> Break (1 hour)
   -> Window (3 hours)

After scheduling, the Segments have times (9-12, 12-1, 1-4pm)

  Window (1-4pm)
    -> Talk (1 hour)
    -> Panel (30 mins)
    -> Talk (1 hour)

After scheduling, the Sessions have times (1-2pm, 2-2:30pm, 2:30-3:30pm, with a buffer of 30 minutes)

   -}

hhmm hh mm =
  let t = do
            hh' <- toEnum hh
            mm' <- toEnum mm
            ss  <- toEnum 0
            ms  <- toEnum 0
            pure $ Time hh' mm' ss ms 
  in unsafePartial $ fromJust t

wrapTime :: forall a. Detailed a -> Time -> Scheduled a
wrapTime d st = Scheduled d { startTime: st }

addTime :: forall a. Scheduled a -> Detailed a -> Scheduled a
addTime l r = 
  let d = duration l
      lst = startTime l
      Tuple _ rst = adjust d lst
  in wrapTime r rst

schedule :: forall a. List (Detailed a) -> Time -> List (Scheduled a)
schedule ds st =
  case ds of 
    Nil -> Nil
    (d:ds') -> let init = wrapTime d st
               in init : scanl addTime init ds'
