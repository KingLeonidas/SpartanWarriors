
data Geometry = Point Int Int    -- x y
              | Circle Int Int Int -- x y r
              | Rectangle Int Int Int Int -- t l b r
              | Triangle Int Int Int Int Int Int -- x1 y1 x2 y2 x3 y3
              | Group [Geometry] deriving (Show)

-- Area computed for each figure using varying formulas for area 
-- based on coordinates. The area of a group of figures is simply
-- summed. 
area :: Geometry -> Float
area (Point x y) = 0
area (Circle x y r) = pi * fromIntegral (r ^ 2)
area (Rectangle x1 y1 x2 y2) = fromIntegral((abs (x2 - x1))) * fromIntegral((abs (y2 - y1)))
area (Triangle x1 y1 x2 y2 x3 y3) = fromIntegral(abs ((x1*(y2-y3) + x2*(y3-y1) + x3*(y1-y2)) `div` 2))
area (Group xs) = sum [area x | x <- xs]

-- Builds the bounding box based on varying criteria. 
-- If a empty Group, return a point.
-- If a Point, return said point.
-- If a Circle, return the box with sides tangent to the circle. 
-- The rectangle is aligned to both axis.
-- If a Rectangle, return said rectangle.
-- If a Triangle, build the box using the greatest and lowest y values,
-- along with the greatest and loweset x values for the two corners in (top left) (bottom right) fashion.
-- If a Group, use the same method as in Triangle.
bbox :: Geometry -> Geometry
bbox (Group []) = Rectangle 0 0 0 0
bbox (Point x y) = Rectangle x y x y
bbox (Circle x y r) = Rectangle (x-r) (y+r) (x+r) (y-r)
bbox (Rectangle x1 y1 x2 y2) = Rectangle x1 y1 x2 y2
bbox (Triangle x1 y1 x2 y2 x3 y3) = Rectangle (minimum [x1,x2,x3]) (maximum [y1,y2,y3]) (maximum [x1,x2,x3]) (minimum [y1,y2,y3])
bbox (Group xs) = Rectangle (minimum xvalues) (maximum yvalues) (maximum xvalues) (minimum yvalues)
        where xvalues = xvals (Group xs)
              yvalues = yvals (Group xs)

-- Helper function to extract x coordinate values from a group of figures.
xvals :: Geometry -> [Int]
xvals (Group []) = []
xvals (Point x y) = [x]
xvals (Circle x y r) = [x+r,x-r]
xvals (Rectangle x1 y1 x2 y2) = [x1,x2]
xvals (Triangle x1 y1 x2 y2 x3 y3) = [x1,x2,x3]
xvals (Group xs) = xvals (head xs) ++ xvals (Group (tail xs))

-- Helper function to extract y coordiante values from a group of figures. 
yvals :: Geometry -> [Int]
yvals (Group []) = []
yvals (Point x y) = [y]
yvals (Circle x y r) = [y+r,y-r]
yvals (Rectangle x1 y1 x2 y2) = [y1,y2]
yvals (Triangle x1 y1 x2 y2 x3 y3) = [y1,y2,y3]
yvals (Group xs) = yvals (head xs) ++ yvals (Group (tail xs)) 
