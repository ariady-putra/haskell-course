-- This homework is around creating Haskell types that represent wines from over the world.

-- Question 1
-- Different wines are made from different grapes, there are around 10000 varieties over the world!
-- Create a type synonym called "Grape" for the different grape names as strings.
-- Additionally, use this type synonym for the grapes: "Sangiovese", "Cabernet-sauvignon", "Merlot" and "Garnacha".
type Grape = String

grapes :: [Grape]
grapes =
    [ "Sangiovese"
    , "Cabernet-sauvignon"
    , "Merlot"
    , "Garnacha"
    ]
-- >>> show grapes
-- "[\"Sangiovese\",\"Cabernet-sauvignon\",\"Merlot\",\"Garnacha\"]"

-- Question 2
-- The most famous regions that export wine are located in France, Italy and Spain.
-- Each of these countries is divided up in smaller regions.
-- These smaller regions are known for a certain style, for example the Champagne region in France
-- Create a type synonym called "Region" for wine region given their country and region as a tuple of strings.
-- Additionally, use this type synonym for the regions: Bordeaux in France, Tuscany in Italy and Rioja in Spain.
type Region = (String, String)

regions :: [Region]
regions =
    [ ("Bordeaux", "France")
    , ("Tuscany" , "Italy" )
    , ("Rioja"   , "Spain" )
    ]
-- >>> show regions
-- "[(\"Bordeaux\",\"France\"),(\"Tuscany\",\"Italy\"),(\"Rioja\",\"Spain\")]"

-- Question 3
-- A wine is either one of three kinds, these are red, white or rose wine.
-- Besides its kind, each wine also has a given alcohol level.
-- Create a data type called "Kind" that represents these three kinds, with each capturing the level of alcohol.
-- Additionally, use this data type for the examples: red wine with 14.5% alcohol, white wine with 13% alcohol
-- and Rose wine with 12% alcohol.
type Alcohol = Float

data Wine
    = Red
    | White
    | Rose
    deriving (Show, Eq)

data Kind = Kind Wine Alcohol
instance Show Kind where
    show (Kind w a) = show w ++ " Wine " ++ show a ++ "% alcohol"

kinds :: [Kind]
kinds =
    [ (Kind Red   14.5)
    , (Kind White 13  )
    , (Kind Rose  12  )
    ]
-- >>> show kinds
-- "[Red Wine 14.5% alcohol,White Wine 13.0% alcohol,Rose Wine 12.0% alcohol]"

-- Question 4
-- In the world of wines, bottles display all of the above information for the consumer on its label.
-- Create a record type called "Label" that captures the grapes that are in a whine, the region its from,
-- and it's kind. Notice that some wines are a blended combination of multiple grapes!
-- Additionally, create for each of the described wine below a label.
newtype Label = Label (String, [Grape], Region, Kind)
instance Show Label where
    show (Label (l, g, (r, c), (Kind w a))) =
        l ++ " is a " ++ show w ++ " wine from the region " ++ r ++ " in " ++ c ++ ". " ++
        "It is made from " ++ show g ++ " and has an alcohol level of " ++ show a ++ "%"

-- Larrosa Rose is a rose wine from the region Rioja. It is made from the Garnacha grape and
-- has a alcohol level of 14%.
larrosaRose :: Label
larrosaRose = Label
    ( "Larrosa Rose"
    , ["Garnacha"]
    , ("Rioja", "Spain")
    , (Kind Rose 14)
    )

-- Castiglioni is a red wine from the region of Tuscany. It is made from the grape Sangiovese and
-- has an alcohol level of 12.5%.
castiglioni :: Label
castiglioni = Label
    ( "Castiglioni"
    , ["Sangiovese"]
    , ("Tuscany", "Italy")
    , (Kind Red 12.5)
    )

-- Bordeaux is known for its red wine, these are mainly a blend between Cabernet-sauvignon and Merlot.
-- Create a Label for the wine "Le Petit Haut Lafitte" that has an alcohol percentage 13.5%.
lePetitHaitLafitte :: Label
lePetitHaitLafitte = Label
    ( "Le Petit Haut Lafitte"
    , ["Cabernet-sauvignon", "Merlot"]
    , ("Bordeaux", "France")
    , (Kind Red 13.5)
    )

-- Question 5
-- Write a function `containsGrape` that takes a list of Labels and a Grape and returns a boolean.
-- The function should check if the there exists a wine in the Label that contains this Grape.
containsGrape :: [Label] -> Grape -> Bool
containsGrape ((Label (_, gs, _, _)):ls) g =
    if g `elem` gs
        then True
        else containsGrape ls g
containsGrape _ _ = False

-- This is a test list for the `containsGrape` function with an grape that is not in the list.
grapeList :: [Label]
grapeList = [larrosaRose, castiglioni, lePetitHaitLafitte]
-- >>> show grapeList
-- "[Larrosa Rose is a Rose wine from the region Rioja in Spain. It is made from [\"Garnacha\"] and has an alcohol level of 14.0%,Castiglioni is a Red wine from the region Tuscany in Italy. It is made from [\"Sangiovese\"] and has an alcohol level of 12.5%,Le Petit Haut Lafitte is a Red wine from the region Bordeaux in France. It is made from [\"Cabernet-sauvignon\",\"Merlot\"] and has an alcohol level of 13.5%]"

newGrape :: Grape
newGrape = "Pinot Noir"
-- >>> containsGrape grapeList newGrape
-- False
-- >>> containsGrape grapeList "Sangiovese"
-- >>> containsGrape grapeList "Cabernet-sauvignon"
-- >>> containsGrape grapeList "Merlot"
-- >>> containsGrape grapeList "Garnacha"
-- True
-- True
-- True
-- True

-- >>> getWinesByGrape grapeList newGrape
-- []
-- >>> getWinesByGrape grapeList "Sangiovese"
-- >>> getWinesByGrape grapeList "Cabernet-sauvignon"
-- >>> getWinesByGrape grapeList "Merlot"
-- >>> getWinesByGrape grapeList "Garnacha"
-- ["Castiglioni"]
-- ["Le Petit Haut Lafitte"]
-- ["Le Petit Haut Lafitte"]
-- ["Larrosa Rose"]
getWinesByGrape :: [Label] -> Grape -> [String]
getWinesByGrape ls g = map (\ (Label (l, _, _, _)) -> l) $
    filter (\ (Label (_, gs, _, _)) -> g `elem` gs) ls

-- >>> getInformationByLabel grapeList newGrape
-- "Not found"
-- >>> getInformationByLabel grapeList "Castiglioni"
-- >>> getInformationByLabel grapeList "Le Petit Haut Lafitte"
-- >>> getInformationByLabel grapeList "Larrosa Rose"
-- "Castiglioni is a Red wine from the region Tuscany in Italy. It is made from [\"Sangiovese\"] and has an alcohol level of 12.5%"
-- "Le Petit Haut Lafitte is a Red wine from the region Bordeaux in France. It is made from [\"Cabernet-sauvignon\",\"Merlot\"] and has an alcohol level of 13.5%"
-- "Larrosa Rose is a Rose wine from the region Rioja in Spain. It is made from [\"Garnacha\"] and has an alcohol level of 14.0%"
getInformationByLabel :: [Label] -> String -> String
getInformationByLabel (i@(Label (l, _, _, _)):ls) q =
    if l == q
        then show i
        else getInformationByLabel ls q
getInformationByLabel _ _ = "Not found"
