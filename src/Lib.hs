{-# LANGUAGE OverloadedStrings #-}

module Lib (
  makeTemplate
) where

import qualified Text.HTML.TagSoup as TGS
import Data.Maybe
import Data.Text (Text, strip, pack, unpack, intercalate, concat)
import Cases

type TGSTag = TGS.Tag String 

data Status = Open | Closed deriving (Show)

data Tag = Tag TGSTag [Tag] Status deriving (Show)

getDescription :: TGSTag -> Status -> String
getDescription (TGS.TagOpen n _) _ = n
getDescription (TGS.TagText n) _ = "text#" ++ n
getDescription (TGS.TagComment _) _ = "Comment tag"
getDescription (TGS.TagWarning _) _ = error "Unsupported tag type in document"
getDescription (TGS.TagPosition _ _) _ = error "Unsupported tag type in document"

prettyPrint :: Tag -> IO ()
prettyPrint = prettyPrint' 0 
  where
    prettyPrint' :: Int -> Tag -> IO ()
    prettyPrint' offset (Tag tgst cs status) = do
      putStrLn $ indent offset ++ getDescription tgst status
      mapM_ (prettyPrint' (offset + 2)) cs 

makeLucidFunction :: Tag -> String
makeLucidFunction (Tag (TGS.TagOpen n []) [] _) = n ++ "_ []"
makeLucidFunction (Tag (TGS.TagOpen n []) _  _) = n ++ "_ "
makeLucidFunction (Tag (TGS.TagOpen n xs) _ _) =  n ++ "_" ++ " [" ++  unpack (intercalate "," $ makeAttibute <$> xs) ++ "]"
  where
    makeAttibute :: (String, String) -> Text
    makeAttibute (a, v) = Data.Text.concat [camelize $ pack a, "_ " , pack $ show v]

indent :: Int -> String
indent offset = replicate offset ' '

makeTemplate :: String -> String
makeTemplate = convertToLucid.makeDocFromHtml

convertToLucid :: Tag -> String
convertToLucid = convertToLucidWithOffset 0
  where
  convertToLucidWithOffset :: Int -> Tag -> String
  convertToLucidWithOffset offset (Tag (TGS.TagText content) [] _) = indent offset ++ show content ++ "\n"
  convertToLucidWithOffset offset tag@(Tag (TGS.TagOpen _ _) [] _) =
    indent offset ++ makeLucidFunction tag  ++ "\n"
  convertToLucidWithOffset offset tag@(Tag (TGS.TagOpen _ _) [Tag (TGS.TagText content) [] _] _) =
    indent offset ++ makeLucidFunction tag ++ " " ++ show content ++ "\n"
  convertToLucidWithOffset offset tag@(Tag (TGS.TagOpen _ _) children _) =
    indent offset ++ makeLucidFunction tag ++ " " ++ generateChildCode (offset+2) children 
  generateChildCode :: Int -> [Tag] -> String
  generateChildCode _ [] = ""
  generateChildCode offset children = " $ do \n" ++ Prelude.concat (convertToLucidWithOffset offset <$> children)

makeDocFromHtml :: String -> Tag
makeDocFromHtml html = buildHtmlTree $ TGS.parseTags html

buildHtmlTree :: [TGSTag] -> Tag
buildHtmlTree (x:xs) = reverseChildren $ addTags (makeTag x) $ filterNewlines xs
  where
    filterNewlines :: [TGSTag] -> [TGSTag]
    filterNewlines = filter (not.isNewLine) 
      where
      isNewLine :: TGSTag -> Bool
      isNewLine (TGS.TagText t) = strip(pack t) == ""
      isNewLine _ = False
    reverseChildren :: Tag -> Tag
    reverseChildren (Tag t children st) = Tag t (reverseChildren <$> reverse children) st
    makeTag :: TGSTag -> Tag
    makeTag tgst@(TGS.TagOpen _ _) = Tag tgst [] Open
    makeTag tgst@(TGS.TagText _) = Tag tgst [] Closed
    makeTag tgst@(TGS.TagComment _) = Tag tgst [] Closed
    makeTag (TGS.TagWarning _) = error "Unsupported tag type in document"
    makeTag (TGS.TagPosition _ _) = error "Unsupported tag type in document"
    addTags :: Tag -> [TGSTag] -> Tag
    addTags tag [] = tag
    addTags t (y:ys) = addTags (fromJust $ addTag t y) ys
    addTag :: Tag -> TGSTag -> Maybe Tag
    addTag (Tag _ _ Closed) _ = Nothing 
    addTag tg@(Tag _ _ Open) (TGS.TagClose _) = closeTag tg
    addTag (Tag td [] Open) tgst = Just $ Tag td [ makeTag tgst ] Open
    addTag (Tag td ex@(t:ts) Open) tgst = case addTag t tgst of
      Just nt -> Just $ Tag td (nt:ts) Open
      Nothing -> Just $ Tag td (makeTag tgst:ex) Open
    closeTag :: Tag -> Maybe Tag
    closeTag (Tag _ _ Closed) = Nothing
    closeTag (Tag td [] Open) = Just (Tag td [] Closed)
    closeTag (Tag td ex@(t:ts) Open) = case closeTag t of
      Nothing -> Just $ Tag td ex Closed
      Just y -> Just $ Tag td (y:ts) Open
