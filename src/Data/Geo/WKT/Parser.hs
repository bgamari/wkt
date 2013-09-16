module Data.Geo.WKT.Parser where

import Data.Geo.WKT.Types
import Linear
import Control.Applicative
import Text.ParserCombinators.Parsec hiding (many)
import Text.ParserCombinators.Parsec.Number hiding (number)

object :: String -> Parser a -> Parser a
object keyword parser = do
    string keyword
    between (char '[') (char ']') parser
   
quotedString :: Parser String
quotedString = do
    char '"'
    manyTill anyChar (char '"')

fieldSep :: Parser ()
fieldSep = char ',' >> spaces

number :: Parser Double
number = floating2 True

twinAxes = do
    ax1 <- axis
    fieldSep 
    ax2 <- axis
    fieldSep
    return (ax1, ax2)

unit :: Parser Unit
unit = object "UNIT" $ do
    name <- quotedString
    fieldSep
    conv <- number
    auth <- optionMaybe $ fieldSep *> authority
    return $ Unit name conv auth
    
parameter :: Parser Parameter
parameter = object "PARAMETER" $ do
    name <- quotedString
    fieldSep
    value <- number
    return $ Parameter name value 

authority :: Parser Authority
authority = object "AUTHORITY" $ do
    name <- quotedString
    fieldSep
    code <- quotedString
    return $ Authority name code

axis :: Parser Axis
axis = object "AXIS" $ do
    name <- quotedString
    fieldSep
    dir <- choice [ string "NORTH" >> return North
                  , string "SOUTH" >> return South
                  , string "EAST"  >> return East
                  , string "WEST"  >> return West
                  , string "UP"    >> return Up
                  , string "DOWN"  >> return Down
                  , string "OTHER" >> return Other
                  ]
    return $ Axis name dir

projection :: Parser Projection
projection = object "PROJECTION" $ do
    name <- quotedString
    auth <- optionMaybe $ fieldSep *> authority
    return $ Proj name auth

spheroid :: Parser Spheroid
spheroid = object "SPHEROID" $ do
    name <- quotedString
    fieldSep
    semiMajor <- number
    fieldSep
    invFlat <- number
    auth <- optionMaybe $ fieldSep *> authority
    return $ Spheroid name semiMajor invFlat auth
     
datum :: Parser Datum
datum = object "DATUM" $ do             
    name <- quotedString
    fieldSep
    s <- spheroid
    wgs <- optionMaybe $ fieldSep *> toWGS84
    auth <- optionMaybe $ fieldSep *> authority
    return $ Datum name s wgs auth

primeMeridian :: Parser PrimeMeridian
primeMeridian = object "PRIMEM" $ do
    name <- quotedString
    fieldSep
    long <- number
    auth <- optionMaybe $ fieldSep *> authority
    return $ PrimeMeridian name long auth
    
toWGS84 :: Parser ToWGS84
toWGS84 = object "TOWGS84" $ do
    d <- V3 <$> number <*> number <*> number        
    e <- V3 <$> number <*> number <*> number        
    ppm <- number
    return $ ToWGS84 d e ppm

projectedCS :: Parser ProjectedCS
projectedCS = object "PROJCS" $ do
    name <- quotedString
    fieldSep
    geogcs <- geographicCS
    fieldSep
    proj <- projection
    fieldSep
    params <- many $ parameter <* fieldSep
    linearUnit <- unit
    axes <- optionMaybe $ fieldSep *> twinAxes
    auth <- optionMaybe $ fieldSep *> authority
    return $ ProjCS name geogcs proj params linearUnit axes auth
    
geographicCS :: Parser GeographicCS
geographicCS = object "GEOGCS" $ do
    name <- quotedString
    fieldSep
    dat <- datum
    fieldSep
    primem <- primeMeridian
    fieldSep
    angularUnit <- unit
    axes <- optionMaybe $ fieldSep *> twinAxes
    auth <- optionMaybe $ fieldSep *> authority
    return $ GeogCS name dat primem angularUnit axes auth
    
