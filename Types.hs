{-# LANGUAGE InstanceSigs #-}
module Types (
    Item(..),
    PageData(..),
    PageAssets(..)
) where

import Data.Text.Lazy (Text)

data Item = DirItem String | FileItem String
data PageData = BrowserPage FilePath [Item] | FilePage FilePath Text [Item]

data PageAssets = PageAssets { headTags :: [Text], bodyTags :: [Text] }
    deriving (Show, Eq)

instance Semigroup PageAssets where
    (<>) :: PageAssets -> PageAssets -> PageAssets
    (PageAssets h1 b1) <> (PageAssets h2 b2) = PageAssets (h1 <> h2) (b1 <> b2)

instance Monoid PageAssets where
    mempty :: PageAssets
    mempty = PageAssets [] []