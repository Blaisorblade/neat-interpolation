module NeatInterpolation.String where

import BasePrelude


normalizeQQInput :: Bool -> [Char] -> [Char]
normalizeQQInput normalizeNL =
  if normalizeNL then
    filter (/= '\r') . body
  else
    body
  where
    body :: [Char] -> [Char]
    body = trim . unindent' . tabsToSpaces
    unindent' :: [Char] -> [Char]
    unindent' s =
      case lines s of
        head:tail -> 
          let 
            unindentedHead = dropWhile (== ' ') head 
            minimumTailIndent = minimumIndent . unlines $ tail
            unindentedTail = case minimumTailIndent of
              Just indent -> map (drop indent) tail
              Nothing -> tail
          in unlines $ unindentedHead : unindentedTail
        [] -> []

trim :: [Char] -> [Char]
trim = dropWhileRev isSpace . dropWhile isSpace

dropWhileRev :: (a -> Bool) -> [a] -> [a]
dropWhileRev p = foldr (\x xs -> if p x && null xs then [] else x:xs) []

unindent :: [Char] -> [Char]
unindent s =
  case minimumIndent s of
    Just indent -> unlines . map (drop indent) . lines $ s
    Nothing -> s

tabsToSpaces :: [Char] -> [Char]
tabsToSpaces ('\t':tail) = "    " ++ tabsToSpaces tail
tabsToSpaces (head:tail) = head : tabsToSpaces tail
tabsToSpaces [] = []

minimumIndent :: [Char] -> Maybe Int
minimumIndent = 
  listToMaybe . sort . map lineIndent 
    . filter (not . null . dropWhile isSpace) . lines

-- | Amount of preceding spaces on first line
lineIndent :: [Char] -> Int
lineIndent = length . takeWhile (== ' ')
