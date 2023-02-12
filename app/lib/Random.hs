module Random where

import qualified Crypto.Hash.SHA256 as SHA
import qualified Data.ByteString as BS
import qualified Data.List as L
import qualified Data.Text as TL
import qualified Data.Text.Encoding as TSE
import qualified Simple as MRG
import System.Random
import System.Random.TF
import qualified Text.Printf as TP

newtype MRGen = MRGen MRG.State -- wrapper type for MRG32k3a generator
  deriving (Show)

instance RandomGen MRGen where
  genRange =
    let mrg32k3a_m1 = ((2 :: Integer) ^ 32 - 209)
     in const (0 :: Int, fromIntegral (mrg32k3a_m1 - 1))

  next (MRGen g0) =
    let (v, g1) = MRG.next g0
     in ((fromIntegral v) :: Int, MRGen g1)

  split (MRGen g0) =
    let g1 = MRG.advance ((2 :: Integer) ^ 96) g0
     in (MRGen g0, MRGen g1)

mkMRGen :: Int -> MRGen
mkMRGen userSeed =
  let longSeed = hashSeed userSeed
      g0 = MRG.seed longSeed
   in MRGen g0

ranSeek :: MRGen -> Integer -> MRGen
ranSeek (MRGen g0) count = let g1 = (MRG.advance count g0) in MRGen g1

hashSeed :: Int -> Integer
hashSeed userSeed =
  let str = "MRG32k3a:" ++ (TP.printf "0x%x" userSeed)
      bytes = (TSE.encodeUtf8 . TL.pack) $ str
      ints = (map (fromIntegral) $ BS.unpack (SHA.hash bytes)) :: [Integer]
   in L.foldl' (\acc d -> acc * 256 + d) 0 (take 20 ints)

randomDoubleField :: MRGen -> (Int, Int) -> Double
randomDoubleField userSeed (x, y) =
  let k = 1 -- number of needed random values per plane point
  --   g0 = mkMRGen userSeed
      g1 = ranSeek userSeed (fromIntegral (k * cantor (x, y)))
   in fst (random g1)

-- limited to first quadrant, x >= 0 and y >= 0:
cantor1 :: Int -> Int -> Int
cantor1 x y = y + (let s = x + y in div (s * (s + 1)) 2)

-- for all 4 quadrants:
cantor :: (Int, Int) -> Int
cantor (x, y) =
  let quadrant
        | x >= 0 && y >= 0 = 0
        | x < 0 && y >= 0 = 1
        | x < 0 && y < 0 = 2
        | x >= 0 && y < 0 = 3
        | otherwise = error "cantor: internal error #1"
      cant1
        | x >= 0 && y >= 0 = cantor1 x y
        | x < 0 && y >= 0 = cantor1 (-1 - x) y
        | x < 0 && y < 0 = cantor1 (-1 - x) (-1 - y)
        | x >= 0 && y < 0 = cantor1 x (-1 - y)
        | otherwise = error "cantor: internal error #2"
   in 4 * cant1 + quadrant
