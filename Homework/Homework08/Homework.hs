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

data Kind
    = Red
    | White
    | Rose
    deriving (Show, Eq)

data Wine = Wine
    { getKind    :: Kind
    , getAlcohol :: Alcohol
    }
instance Show Wine where
    show w =
        show (getKind    w) ++ " Wine " ++
        show (getAlcohol w) ++ "% alcohol"

wines :: [Wine]
wines =
    [ (Wine Red   14.5)
    , (Wine White 13  )
    , (Wine Rose  12  )
    ]
-- >>> show wines
-- "[Red Wine 14.5% alcohol,White Wine 13.0% alcohol,Rose Wine 12.0% alcohol]"

-- Question 4
-- In the world of wines, bottles display all of the above information for the consumer on its label.
-- Create a record type called "Label" that captures the grapes that are in a whine, the region its from,
-- and it's kind. Notice that some wines are a blended combination of multiple grapes!
-- Additionally, create for each of the described wine below a label.
data Label = Label
    { getLabel  :: String
    , getGrapes :: [Grape]
    , getRegion :: Region
    , getWine   :: Wine
    }
instance Show Label where
    show l =
        let
            label   = getLabel l
            kind    = getKind wine
            region  = fst . getRegion $ l
            country = snd . getRegion $ l
            grapes  = getGrapes l
            alcohol = getAlcohol wine
        in
            label ++ " is a " ++ show kind ++ " wine from the region " ++ region ++ " in " ++ country ++ ". " ++
            "It is made from " ++ show grapes ++ " and has an alcohol level of " ++ show alcohol ++ "%"
        where
            wine = getWine l

-- Larrosa Rose is a rose wine from the region Rioja. It is made from the Garnacha grape and
-- has a alcohol level of 14%.
larrosaRose :: Label
larrosaRose = Label
    "Larrosa Rose"
    ["Garnacha"]
    ("Rioja", "Spain")
    (Wine Rose 14)

-- Castiglioni is a red wine from the region of Tuscany. It is made from the grape Sangiovese and
-- has an alcohol level of 12.5%.
castiglioni :: Label
castiglioni = Label
    "Castiglioni"
    ["Sangiovese"]
    ("Tuscany", "Italy")
    (Wine Red 12.5)

-- Bordeaux is known for its red wine, these are mainly a blend between Cabernet-sauvignon and Merlot.
-- Create a Label for the wine "Le Petit Haut Lafitte" that has an alcohol percentage 13.5%.
lePetitHaitLafitte :: Label
lePetitHaitLafitte = Label
    "Le Petit Haut Lafitte"
    ["Cabernet-sauvignon", "Merlot"]
    ("Bordeaux", "France")
    (Wine Red 13.5)

-- Question 5
-- Write a function `containsGrape` that takes a list of Labels and a Grape and returns a boolean.
-- The function should check if the there exists a wine in the Label that contains this Grape.
containsGrape :: [Label] -> Grape -> Bool
-- containsGrape (l:ls) g =
--     if elem g $ getGrapes l
--         then True
--         else containsGrape ls g
-- containsGrape _ _ = False
containsGrape ls g =
    foldr
        (\ l ls ->
            if elem g $ getGrapes l
                then True
                else ls
        )
        False
        ls

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
getWinesByGrape ls g = map getLabel $
    filter (elem g . getGrapes) ls

-- >>> getInfoByLabel grapeList newGrape
-- "Not found"
-- >>> getInfoByLabel grapeList "Castiglioni"
-- >>> getInfoByLabel grapeList "Le Petit Haut Lafitte"
-- >>> getInfoByLabel grapeList "Larrosa Rose"
-- "Castiglioni is a Red wine from the region Tuscany in Italy. It is made from [\"Sangiovese\"] and has an alcohol level of 12.5%"
-- "Le Petit Haut Lafitte is a Red wine from the region Bordeaux in France. It is made from [\"Cabernet-sauvignon\",\"Merlot\"] and has an alcohol level of 13.5%"
-- "Larrosa Rose is a Rose wine from the region Rioja in Spain. It is made from [\"Garnacha\"] and has an alcohol level of 14.0%"
getInfoByLabel :: [Label] -> String -> String
-- getInfoByLabel (i:is) l =
--     if l == getLabel i
--         then show i
--         else getInfoByLabel is l
-- getInfoByLabel _ _ = "Not found"
getInfoByLabel is l =
    foldr
        (\ i is ->
            if l == getLabel i
                then show i
                else is
        )
        "Not found"
        is
