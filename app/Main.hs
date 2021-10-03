
{-# LANGUAGE OverloadedStrings #-}

module Main where


import Data.Aeson
import Resources

{-| 

>>> show Component {componentName = "Yes", quantity = 0.5}

-}

getRecipes = [
        Recipe "Iron Ore" 2 [], 
        Recipe "Copper Ore" 2 [], 
        Recipe "Copper Plate" 1.6 [Component "Copper Ore" 1],
        Recipe "Iron Plate" 1.6 [Component "Iron Ore" 1], 
        Recipe "Iron Gear" 0.5 [Component "Iron Plate" 2],
        Recipe "Red Science" 5 [Component "Copper Plate" 1, Component "Iron Gear" 1],
        Recipe "Copper Cable" 0.5 [Component "Copper Plate" 0.5],
        Recipe "Electronic Circuit" 0.5 [Component "Iron Plate" 1, Component "Copper Cable" 3],
        Recipe "Transport Belt" 0.5 [Component "Iron Plate" 1, Component "Iron Gear" 1],
        Recipe "Inserter" 0.5 [Component "Iron Plate" 1, Component "Iron Gear" 1, Component "Electronic Circuit" 1],
        Recipe "Green Science" 6 [Component "Transport Belt" 1, Component "Inserter" 1],
        Recipe "Steel Plate" 16 [Component "Iron Plate" 5]
    ]


main = 
    let 
        consumptionTree = getMultipleTargets getRecipes [("Green Science", 1), ("Red Science", 1), ("Steel Plate", 0.5)]
        totalBuild = mergeProduction (flattenAllSites consumptionTree)
    in
    do 
        putStrLn "Consumption Tree:"
        print consumptionTree
        putStrLn ""
        putStrLn "Total to Build:"
        print totalBuild