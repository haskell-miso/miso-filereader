-----------------------------------------------------------------------------
{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Main where
----------------------------------------------------------------------------
import           Control.Monad      (forM_)
----------------------------------------------------------------------------
import           Miso
import           Miso.Lens
import qualified Miso.Html.Element  as H
import           Miso.Html.Event    (onClick, onChangeWith)
import qualified Miso.Html.Property as P
import qualified Miso.String        as S
----------------------------------------------------------------------------
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
----------------------------------------------------------------------------
-- | What we managed to extract from a file.
data Preview
  = TextPreview MisoString  -- ^ first part of a text file
  | ImagePreview MisoString -- ^ data: URL for an image
  deriving (Eq, Show)
----------------------------------------------------------------------------
data Entry = Entry
  { entryName :: MisoString
  , entrySize :: Int
  , entryMime :: MisoString
  , entryPreview :: Preview
  } deriving (Eq, Show)
----------------------------------------------------------------------------
newtype Model = Model
  { _entries :: [Entry]
  } deriving (Eq, Show)
----------------------------------------------------------------------------
entries :: Lens Model [Entry]
entries = lens _entries $ \m x -> m { _entries = x }
----------------------------------------------------------------------------
data Action
  = ReadFiles DOMRef
  | Loaded Entry
  | Clear
----------------------------------------------------------------------------
main :: IO ()
main = startApp defaultEvents app
----------------------------------------------------------------------------
app :: App Model Action
app = component (Model []) updateModel viewModel
----------------------------------------------------------------------------
-- | How much of a text file to show.
previewChars :: Int
previewChars = 20000
----------------------------------------------------------------------------
updateModel :: Action -> Effect context props Model Action
updateModel = \case
  ReadFiles input ->
    withSink $ \sink -> do
      picked <- files input
      forM_ picked $ \file -> do
        name <- fromJSValUnchecked =<< file ! "name"
        size <- fromJSValUnchecked =<< file ! "size"
        mime <- fromJSValUnchecked =<< file ! "type"
        reader@(FileReader r) <- newFileReader
        cb <- asyncCallback $ do
          payload <- fromJSValUnchecked =<< r ! "result"
          let preview
                | isImage mime = ImagePreview payload
                | otherwise    = TextPreview (S.take previewChars payload)
          sink $ Loaded Entry
            { entryName = name
            , entrySize = size
            , entryMime = mime
            , entryPreview = preview
            }
        setField r "onload" cb
        _ <- if isImage mime
          then reader # "readAsDataURL" $ [ file ]
          else reader # "readAsText" $ [ file ]
        pure ()
  Loaded e ->
    entries %= (e :)
  Clear ->
    entries .= []
----------------------------------------------------------------------------
isImage :: MisoString -> Bool
isImage = S.isPrefixOf "image/"
----------------------------------------------------------------------------
viewModel :: () -> () -> Model -> View () Model Action
viewModel _ _ m =
  H.div_
  [ P.class_ "app" ]
  [ H.header_
    [ P.class_ "hero" ]
    [ H.h1_ [] [ "🍜 📄 ", H.a_ [ P.href_ repoUrl ] [ "miso-filereader" ] ]
    , H.p_ [ P.class_ "tagline" ]
      [ "The browser FileReader API from Haskell: pick files and they are "
      , "read client-side — text gets previewed, images are decoded to data "
      , "URLs. Nothing is uploaded anywhere."
      ]
    , H.a_ [ P.class_ "gh", P.href_ repoUrl ] [ "View source on GitHub" ]
    ]
  , H.main_
    []
    ( [ H.label_
        [ P.class_ "dropzone" ]
        [ H.span_ [ P.class_ "dz-icon" ] [ "📂" ]
        , H.span_ [] [ H.strong_ [] [ "Choose files" ], " — text or images" ]
        , H.input_
          [ P.type_ "file"
          , P.multiple_ True
          , P.class_ "hidden-input"
          , onChangeWith (\_ domRef -> ReadFiles domRef)
          ]
        ]
      ]
      ++ [ H.div_
           [ P.class_ "toolbar" ]
           [ H.span_ [ P.class_ "count" ]
             [ text (ms n <> if n == 1 then " file" else " files") ]
           , H.button_ [ P.class_ "btn", onClick Clear ] [ "Clear" ]
           ]
         | n /= 0
         ]
      ++ map entryCard (m ^. entries)
    )
  , H.footer_
    [ P.class_ "foot" ]
    [ H.p_ []
      [ "Built with "
      , H.a_ [ P.href_ "https://github.com/dmjio/miso" ] [ "miso" ]
      , ", a Haskell web framework — compiled to WebAssembly."
      ]
    ]
  ]
  where
    n = length (m ^. entries)
    repoUrl = "https://github.com/haskell-miso/miso-filereader"
----------------------------------------------------------------------------
entryCard :: Entry -> View () Model Action
entryCard Entry {..} =
  H.section_
  [ P.class_ "card" ]
  [ H.div_
    [ P.class_ "card-head" ]
    [ H.h2_ [] [ text entryName ]
    , H.span_ [ P.class_ "meta" ]
      [ text (prettySize entrySize)
      , text (if entryMime == "" then "" else " · " <> entryMime)
      ]
    ]
  , case entryPreview of
      ImagePreview url ->
        H.img_ [ P.class_ "img-preview", P.src_ url, P.alt_ entryName ]
      TextPreview t
        | t == "" ->
            H.p_ [ P.class_ "empty" ] [ "(empty or binary file)" ]
        | otherwise ->
            H.pre_ [ P.class_ "text-preview" ] [ text t ]
  ]
----------------------------------------------------------------------------
prettySize :: Int -> MisoString
prettySize n
  | n < 1024        = ms n <> " B"
  | n < 1048576     = one (fromIntegral n / 1024) <> " KB"
  | otherwise       = one (fromIntegral n / 1048576) <> " MB"
  where
    one :: Double -> MisoString
    one x = ms (fromIntegral (round (x * 10) :: Int) / 10 :: Double)
----------------------------------------------------------------------------
