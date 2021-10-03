module Resources where

import Data.Maybe

data Component = Component {
    componentName :: [Char],
    quantity :: Float
} deriving (Show)

data Recipe = Recipe {
    recipeName :: [Char],
    -- unit is seconds per item per construction site 
    constructionTime :: Float,
    components :: [Component]
} deriving (Show)

data Site = Site {
    siteName :: [Char],
    toBuild :: Float,
    childSites :: [Site]
} deriving (Show)

matchesRecipeName :: String -> Recipe -> Bool
matchesRecipeName a b = recipeName b == a

firstElement p [] = Nothing
firstElement p (x:xs)
    | p x = Just x
    | otherwise = firstElement p xs

-- This will be used at each recursion level to find the Recipe for a given recipe name
-- The Recipe name may not exist, so we return Maybe
getRecipeByName :: [Recipe] -> String -> Maybe Recipe
getRecipeByName recipeCollection name = firstElement (matchesRecipeName name) recipeCollection

-- Turn a maybe recipe into a maybe site. This might not be needed, I am probably misusing the language here, but I don't know how to do this more elegantly. Yet!
coerceRecipe :: [Recipe] -> Maybe Recipe -> Float -> Maybe Site
coerceRecipe recipeCollection recipe targetRate = case recipe of
    Just value -> Just (getAllSitesForRecipe recipeCollection value targetRate)
    Nothing -> Nothing

-- If I can't find the matching recipe name there is no site
-- Given a specific component, find a recipe by name then calculate its Site tree
getNumberOfSitesForComponent :: [Recipe] -> Float -> Component -> Maybe Site
getNumberOfSitesForComponent recipeCollection targetRate component = coerceRecipe recipeCollection (getRecipeByName recipeCollection (componentName component)) (targetRate * quantity component)

-- Translate the list of components to more Sites with their own dependencies
getComponentSites :: [Recipe] -> Recipe -> Float -> [Site]
getComponentSites recipeCollection recipe targetRate = mapMaybe (getNumberOfSitesForComponent recipeCollection targetRate) (components recipe)

-- Create a Site datatype with a given recipe. Recurses for all dependencies with a multiplier target rate
getAllSitesForRecipe :: [Recipe] -> Recipe -> Float -> Site
getAllSitesForRecipe recipeCollection recipe targetRate = Site (recipeName recipe) (constructionTime recipe * targetRate) (getComponentSites recipeCollection recipe targetRate)

-- Take in a name and a target rate and get a site and all of its dependencies, if it exists within the recipe collection
getAllSites :: [Recipe] -> [Char] -> Float -> Maybe Site
getAllSites recipeCollection name = coerceRecipe recipeCollection (getRecipeByName recipeCollection name)

-- Take in multiple recipes to maximize and output the total sites needed for all of them
-- Uncurry is used to change the parameter type of "getAllSites" to a tuple instead of two arguments
-- mapMaybe is used to remove the "Maybe" elements of the Site list
getMultipleTargets :: [Recipe] -> [([Char], Float)] -> [Site]
getMultipleTargets recipeCollection = mapMaybe (uncurry (getAllSites recipeCollection))

-- Given a single site, create a list of sites that represent all of its dependencies
flattenSite :: Site -> [Site]
flattenSite siteToFlatten = Site (siteName siteToFlatten) (toBuild siteToFlatten) [] : concatMap flattenSite (childSites siteToFlatten)

flattenAllSites :: [Site] -> [Site]
flattenAllSites = concatMap flattenSite

isSiteNameEqual :: [Char] -> Site -> Bool
isSiteNameEqual name site = name == siteName site

mergeProduction :: [Site] -> [Site]
mergeProduction sites = case sites of
    [] -> []
    [x] -> [x]
    (x:xs) ->
        let name = siteName x
            matching = filter (isSiteNameEqual name) sites
            notMatching = filter (not . isSiteNameEqual name) sites
        in Site name (sum (map toBuild matching)) [] : mergeProduction notMatching