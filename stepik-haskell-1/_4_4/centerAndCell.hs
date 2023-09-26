data Coord a = Coord a a deriving (Show, Read)

getCenter :: Double -> Coord Int -> Coord Double
getCenter size (Coord x y) = Coord (size * (fromIntegral x) + size / 2) (size * (fromIntegral y) + size / 2)

getCell :: Double -> Coord Double -> Coord Int
getCell size (Coord x y) = Coord (floor $ x / size) (floor $ y / size)  


