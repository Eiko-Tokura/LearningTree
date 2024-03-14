{-# LANGUAGE DataKinds #-}
module Main where
import LearningTree
--import LearningPlan
import AutoInput
--import qualified Data.Map as M

main :: IO ()
main = do
  putStrLn "Pretty print (Show):"
  print exampleProjectTree
  putStrLn "Pretty print (SystemShow):"
  putStrLn $ prettyPrint SystemShow exampleProjectTree
  putStrLn "Pretty print (Concise):"
  putStrLn $ prettyPrint Concise exampleProjectTree
  putStrLn "Pretty print (Verbose):"
  putStrLn $ prettyPrint Verbose exampleProjectTree

  -- test the generic input
  --description <- askUserInput :: IO (Maybe Description)
  projData <- askUserInput :: IO (Maybe ProjectData)
  learningU <- askUserInput :: IO (Maybe (LearningUnit ProjectNode))
  let --Just proj = ( Project <$> learningU <*> projData <*> Just M.empty )
      tree2 = do
        func <- addProject <$> learningU <*> projData
        return $ localChangeTree func exampleProjectTree []
  print $ fmap (prettyPrint Verbose) <$> tree2

  --print $ (readMaybe :: String -> Maybe Day) $ description
