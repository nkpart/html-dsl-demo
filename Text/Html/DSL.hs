{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Text.Html.DSL (
  -- Tag creation methods
  Tags,
  tag,
  empty,
  text,
  -- Printer
  dumpTags,
  -- Example
  test1
  ) where

import Control.Monad.Writer

-- | This is the monad that lets us combine things that write out stuff
newtype TagsM a = TagsM (Writer String a) deriving (Monad)
type Tags = TagsM ()

-- | Dumps the contents of written tags to a string
dumpTags :: Tags -> String
dumpTags (TagsM t) = snd . runWriter $ t -- When you run a writer, you get a pair of (value, writerState).
                                 -- We don't care about the value, and just want the tags that were
                                 -- written to the state.

-- | Create a tag from a name and children
tag :: String -> Tags -> Tags
tag tagName (TagsM children) = TagsM $ do
  -- This uses special methods written for the "Writer" monad, `tell` adds
  -- output to the backing state
  tell $ "<" ++ tagName ++ ">"
  children -- this sequences children between the open and close tag `tells`
  tell $ "</" ++ tagName ++ ">"

-- | A doing nothing tag 
empty :: Tags
empty = return () -- we've not 'tell'ed anything so nothing gets written 

-- | A tag that writes text
text :: String -> Tags
text = TagsM . tell -- writing text is just a straight tell

-- | The example
test1 :: Tags
test1 = tag "html" $ do
  tag "p" $ do
    tag "strong" $ text "Hi guys" 
  tag "div" $ text "more things"


