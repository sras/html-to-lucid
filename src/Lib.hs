{-# LANGUAGE TemplateHaskell #-}

module Lib where

import qualified Text.HTML.TagSoup as TGS
import Lucid
import Data.Maybe
import Language.Haskell.TH

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

convertToLucid :: Tag -> Q [Dec]
convertToLucid tag = do
    exTag <- makeExpressionForTag tag
    return $ [FunD (mkName "toLucid") [Clause [] (NormalB exTag) []]]
  where
    makeExpressionForTag :: Tag -> Q Exp
    makeExpressionForTag (Tag (TGS.TagOpen name attr) [] _) = [e| name (toHtml "") |]
    makeExpressionForTag (Tag (TGS.TagOpen name attr) [Tag (TGS.TagText content) [] _] _) = [e| name (toHtml content) |]
    makeExpressionForTag (Tag (TGS.TagOpen name attr) children _) = do
      exp <- mapM makeExpressionForTag children
      return $ foldl AppE (VarE $ mkName (name ++ "_")) exp 

makeDocFromHtml :: String -> Tag
makeDocFromHtml html = buildHtmlTree $ TGS.parseTags html

buildHtmlTree :: [TGSTag] -> Tag
buildHtmlTree (x:xs) = addTags (makeTag x) xs
  where
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
