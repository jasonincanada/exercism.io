module ProteinTranslation (proteins) where

toAmino :: String -> String
toAmino s
  | s == "AUG"              = "Methionine"
  | s == "UGG"              = "Tryptophan"
  | s `elem` ["UUU", "UUC"] = "Phenylalanine"
  | s `elem` ["UUA", "UUG"] = "Leucine"
  | s `elem` ["UAU", "UAC"] = "Tyrosine"
  | s `elem` ["UGU", "UGC"] = "Cysteine"
  | s `elem` ["UCU", "UCC", 
              "UCA", "UCG"] = "Serine"

isStop :: String -> Bool
isStop = flip elem ["UAA", "UAG", "UGA"]

proteins :: String -> Maybe [String]
proteins str = Just aminos
  where aminos    = map toAmino untilStop
        untilStop = takeWhile (not . isStop) chunks
        chunks    = chunksOf 3 str

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n list = take n list : chunksOf n (drop n list)
