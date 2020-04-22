module Stats where


import Text.CSV
import Data.Maybe
import Data.List
import Data.Ord
import Database.HDBC
import Database.HDBC.Sqlite3


bbEither = either (const []) (filter (\row -> 2 <= length row))

getIndex :: Either a CSV -> Int -> [String]
getIndex csv index = map (!! index) (bbEither csv)

readIndex :: Read cell => Either a CSV -> Int -> [cell]
readIndex csv index = map read (getIndex csv index)


range :: Ord a => [a] -> Maybe (a, a)
range [] = Nothing
range [x] = Just (x, x)
range xs = Just (minimum xs, maximum xs)

medie :: Real a => [a] -> Maybe Double
medie [] = Nothing
medie [x] = Just $ realToFrac x
medie xs = Just $ (realToFrac . sum) xs / (fromIntegral . length) xs


abatereaStandard :: Real a => [a] -> Maybe Double
abatereaStandard [] = Nothing
abatereaStandard xs = Just abatere
                where
                    abatere  = sqrt $ sum abateri / len
                    calcAbat = \x -> (realToFrac x - m) ^^ 2
                    abateri  = map calcAbat xs
                    len   = (fromIntegral . length) xs
                    (Just m) = medie xs


distributie :: Real a => Maybe a -> Maybe a -> Maybe (a, a)
distributie Nothing _ = Nothing
distributie _ Nothing = Nothing
distributie Nothing Nothing = Nothing
distributie (Just medie) (Just abatere) = Just (minima, maxima)
    where
        minima = medie - abatere
        maxima = medie + abatere



mediana :: (Real a, Fractional a) => [a] -> Maybe a
mediana [] = Nothing
mediana xs = Just (med modulo mid)
    where
        xsSort      = sort xs
        modulo      = (length xs `mod` 2) == 0
        mid         = floor (realToFrac (length xs) / fromIntegral 2)
        med True m  = realToFrac((xsSort !! m-1) + (xsSort !! m )) / fromIntegral 2
        med False m = xsSort !! m



runLengthEncoding :: Ord a => [a] -> [(a, Integer)]
runLengthEncoding = map (\xs -> (head xs, genericLength xs)) . group


modPred :: Real a => Maybe (a, a) -> (a, a) -> Maybe (a, a)
modPred Nothing (v, f) = Just (v, f)
modPred (Just (v, f)) (v1, f1) = if f > f1 then Just (v, f) else Just (v1, f1)






