{-# LANGUAGE OverloadedStrings #-}

module Lib where

import qualified Text.HTML.TagSoup as TGS
import Lucid
import Data.Maybe
import Data.Text (strip, pack)

type TGSTag = TGS.Tag String 

data Status = Open | Closed deriving (Show)

data Tag = Tag TGSTag [Tag] Status deriving (Show)

getDescription :: TGSTag -> Status -> String
getDescription tgst@(TGS.TagOpen n attr) st = n
getDescription tgst@(TGS.TagText n) st = "text#" ++ n
getDescription tgst@(TGS.TagComment n) st = "Comment tag"
getDescription (TGS.TagWarning _) st = error "Unsupported tag type in document"
getDescription (TGS.TagPosition _ _) st = error "Unsupported tag type in document"

prettyPrint :: Tag -> IO ()
prettyPrint tag = prettyPrint' 0 tag
  where
    prettyPrint' :: Int -> Tag -> IO ()
    prettyPrint' offset (Tag tgst cs status) = do
      putStrLn $ (take offset $ repeat ' ') ++ (getDescription tgst status)
      mapM_ (prettyPrint' (offset + 2)) cs 

convert :: String -> IO ()
convert html = return ()

makeLucidFunction :: Tag -> String
makeLucidFunction (Tag (TGS.TagOpen n _) _ _) = n ++ "_"

convertToLucid :: Tag -> String
convertToLucid tag = convertToLucidWithOffset 0 tag
  where
  convertToLucidWithOffset :: Int -> Tag -> String
  convertToLucidWithOffset offset tag@(Tag (TGS.TagText content) [] _) = (take offset $ repeat ' ') ++ show content ++ "\n"
  convertToLucidWithOffset offset tag@(Tag (TGS.TagOpen _ _) [] _) =
    (take offset $ repeat ' ') ++ (makeLucidFunction tag)  ++ "\n"
  convertToLucidWithOffset offset tag@((Tag (TGS.TagOpen name attr) ((Tag (TGS.TagText content) [] _):children) _)) =
    (take offset $ repeat ' ') ++ (makeLucidFunction tag) ++ " " ++ show content ++ "\n" ++ (concat $ convertToLucidWithOffset (offset + 2) <$> children)
  convertToLucidWithOffset offset tag@(Tag (TGS.TagOpen _ _) children _) =
    (take offset $ repeat ' ') ++ (makeLucidFunction tag) ++ " $ do \n" ++ (concat $ convertToLucidWithOffset (offset + 2) <$> children)

makeDocFromHtml :: String -> Tag
makeDocFromHtml html = buildHtmlTree $ TGS.parseTags html

buildHtmlTree :: [TGSTag] -> Tag
buildHtmlTree (x:xs) = reverseChildren $ addTags (makeTag x) $ filterNewlines xs
  where
    filterNewlines :: [TGSTag] -> [TGSTag]
    filterNewlines tx = filter (not.isNewLine) tx
      where
      isNewLine :: TGSTag -> Bool
      isNewLine (TGS.TagText t) = (strip(pack t) == "")
      isNewLine _ = False
    reverseChildren :: Tag -> Tag
    reverseChildren (Tag t children st) = Tag t (reverseChildren <$> (reverse children)) st
    makeTag :: TGSTag -> Tag
    makeTag tgst@(TGS.TagOpen n attr) = Tag tgst [] Open
    makeTag tgst@(TGS.TagText n) = Tag tgst [] Closed
    makeTag tgst@(TGS.TagComment n) = Tag tgst [] Closed
    makeTag (TGS.TagWarning _) = error "Unsupported tag type in document"
    makeTag (TGS.TagPosition _ _) = error "Unsupported tag type in document"
    addTags :: Tag -> [TGSTag] -> Tag
    addTags tag [] = tag
    addTags t (x:xs) = addTags (fromJust $ addTag t x) xs
    addTag :: Tag -> TGSTag -> Maybe Tag
    addTag (Tag _ _ Closed) _ = Nothing 
    addTag tg@(Tag td ts Open) (TGS.TagClose _) = closeTag tg
    addTag (Tag td [] Open) tgst = Just $ Tag td [ makeTag tgst ] Open
    addTag (Tag td ex@(t:ts) Open) tgst = case addTag t tgst of
      Just nt -> Just $ Tag td (nt:ts) Open
      Nothing -> Just $ Tag td ((makeTag tgst):ex) Open
    closeTag :: Tag -> Maybe Tag
    closeTag (Tag _ _ Closed) = Nothing
    closeTag (Tag td [] Open) = Just (Tag td [] Closed)
    closeTag (Tag td ex@(t:ts) Open) = case closeTag t of
      Nothing -> Just $ Tag td ex Closed
      Just x -> Just $ Tag td (x:ts) Open
