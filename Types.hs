module Types (
    Item(..),
    PageData(..)
) where

import Data.Text.Lazy (Text)

data Item = DirItem String | FileItem String
data PageData = BrowserPage FilePath [Item] | FilePage FilePath Text [Item]