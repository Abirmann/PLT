data Color = Red | Green | Blue

instance Show Color where
    show Red = "Red"
    show Green = "Green"
    show Blue = "Blue"
 
-- stringToColor :: String -> Color
-- stringToColor show color = color 
