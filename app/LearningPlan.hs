{-# LANGUAGE DataKinds, GADTs, KindSignatures #-}
module LearningPlan where

import Control.Monad
import System.IO
import System.Directory
import LearningTree
import Data.Time
import ProgressBar()
import AutoInput
import qualified Data.Set as S
import qualified Data.Map as M

newtype UserName = UserName String
  deriving (Show, Read)

filePathForLearningPlan = ".learningPlan"

data LearningPlan = LearningPlan 
  { userName :: UserName
  , root     :: ProjectTree -- root is a project, it contains all the projects and tasks
  } deriving (Show, Read)

emptyLearningPlan :: UserName -> LearningPlan
emptyLearningPlan name = 
  LearningPlan name $
    Project (LearningUnit 
      (Data $ Description "Root" "Root")
      (Data $ TimeData 
        (fromGregorian 2024 1 1)
        (fromGregorian 2024 1 1)
        (fromGregorian 2024 1 1)
        (Data $ fromGregorian 2024 1 1)
      )
      (Data S.empty) 
      (Data S.empty) 
      (Data InProgress)
      (Data 0)
    )
    (ProjectData (Priority 0) (Subject "") 0) -- this number increases as we add children
    M.empty

-- what we will consider doing later:
-- 1. add 'constraints' to projects and tasks
--    only tasks can be seen on calendar
--    tasks and subprojects automatically inherit constraints from their parent project when computing the calendar
-- 2*.(Necessary) write parsers to read user input from terminal. Use system's show and read to save and load learning plans from files
--    
-- 3. add 'dependencies' to projects and tasks, a task can only be started when all its dependencies are marked as 'Completed'
--
-- 4. (optional) add resources to tasks and projects, and a way to compute the cost of a project, and the cost of a learning plan
--
-- 5. future plan : write a web interface to manage learning plans, or a GUI or TUI interface
--    We could use the 'brick' library to write a TUI interface, or the 'threepenny-gui' library to write a web interface
--    We could also use the 'servant', 'yesod (prefered!)' or 'scotty' libraries to write a web interface, or the 'gtk' library to write a GUI interface.
--
-- 6. future plan : using my SAT solver package, we could potentially solve the constraints and even dependencies
--    one may also use methods in Operational Research to solve the constraints and dependencies

-- now, we first write a function to parse user input, and a data type to represent actions
newtype ElemInSet a = ElemInSet Int -- used to represent the user's choice in a set, the first Int is the index of the choice in the set

instance AskUserInput (ElemInSet a) where
  askUserInput = do
    putStrLn "Please input the index of the choice you want to make:"
    choice <- safeAskUserInput :: IO Int
    return $ Just $ ElemInSet choice

data ActionType = IOAction | TreeAction

data UserAction (a :: ActionType) where 
  AddProject          :: NodePath -> (LearningUnit ProjectNode) -> ProjectData            -> UserAction TreeAction
  AddTask             :: NodePath -> (LearningUnit TaskNode)    -> UserAction TreeAction
  SetStatus           :: NodePath -> Status                     -> UserAction TreeAction
  SetProgressTracking :: NodePath -> ProgressTracking           -> UserAction TreeAction -- you should only set progress to a task
  ModifyProgress      :: NodePath -> ElemInSet Progress         -> ProgressInt            -> UserAction TreeAction
  SetLU               :: NodePath -> (LearningUnit NoType)      -> UserAction TreeAction
  DisplayDetail       :: NodePath -> UserAction IOAction
  DisplayConcise      :: UserAction IOAction
  Quit                :: UserAction IOAction
                -- | RemoveNode NodePath
                -- | MoveNode NodePath NodePath

-- consider using Generics to derive the 'askUserInput' function for generic types

performTreeAction :: UserAction TreeAction -> LearningPlan -> Either String LearningPlan
performTreeAction (AddProject path lu projData) lp = 
  let newTree = localChangeTree (addProject lu projData) (root lp) path 
  in (\x -> lp { root = x }) <$> newTree
performTreeAction (AddTask path lu) lp =
  let newTree = localChangeTree (addTask lu) (root lp) path
  in (\x -> lp { root = x }) <$> newTree
performTreeAction (SetStatus path status) lp =
  let newTree = localChangeTree (setStatus status) (root lp) path
  in (\x -> lp { root = x }) <$> newTree
performTreeAction (SetProgressTracking path progress) lp =
  let newTree = localChangeTree (setProgress progress) (root lp) path
  in (\x -> lp { root = x }) <$> newTree
performTreeAction (ModifyProgress path (ElemInSet choice) progressInt) lp =
  let tree = getSubTree path (root lp)
  in case tree of
       Left err -> Left err
       Right t  -> let oldProgressSet = unData $ progressTracking $ getUnitN t
                       oldProgress = S.elemAt choice oldProgressSet
                       newProgressSet = S.insert (oldProgress { progress = progressInt }) $ S.deleteAt choice oldProgressSet
                       newTree = localChangeTree (setProgress newProgressSet) (root lp) path
                   in (\x -> lp { root = x }) <$> newTree
performTreeAction (SetLU path lu) lp =
  let newTree = localChangeTree (setLU lu) (root lp) path
  in (\x -> lp { root = x }) <$> newTree

performIOAction :: UserAction IOAction -> LearningPlan -> IO ()
performIOAction (DisplayDetail path) plan = let tree = getSubTree path (root plan) 
                                            in case tree of
                                                 Left err -> putStrLn err
                                                 Right t  -> putStrLn $ prettyPrint Verbose t
performIOAction DisplayConcise plan = displayConcise plan


askAction :: LearningPlan -> IO (Either ErrMsg (Either (UserAction IOAction) (UserAction TreeAction)))
askAction plan = do
  putStrLn "Please input the action you want to perform:"
  putStrLn "0: DisplayConcise  1: AddProject  2: AddTask  3: SetStatus  4: SetProgressTracking\n5: ModifyProgress  6: SetLU  7: DisplayDetail  8: Quit"
  action <- safeAskUserInput :: IO (Finite 0 8)
  case action of
    0 -> return $ Right $ Left DisplayConcise
    1 -> fmap ((Right . Right) .) . AddProject      <$> safeAskUserInput <*> safeAskUserInput <*> safeAskUserInput
    2 -> fmap (Right . Right) . AddTask             <$> safeAskUserInput <*> safeAskUserInput
    3 -> fmap (Right . Right) . SetStatus           <$> safeAskUserInput <*> safeAskUserInput
    4 -> fmap (Right . Right) . SetProgressTracking <$> safeAskUserInput <*> safeAskUserInput
    6 -> fmap (Right . Right) . SetLU               <$> safeAskUserInput <*> safeAskUserInput
    7 -> (Right . Left) . DisplayDetail             <$> safeAskUserInput
    8 -> return $ (Right . Left) Quit
    5 -> do
      path <- safeAskUserInput
      let tree = getSubTree path (root plan)
      case tree of
        Left err -> putStrLn err >> askAction plan
        Right t  -> do
          putStrLn $ prettyPrint Concise t
          putStrLn "Please input the index of the progress you want to modify:"
          choice <- safeAskUserInput :: IO Int
          let oldProgressSet = unData $ progressTracking $ getUnitN t
              oldProgress = S.elemAt choice oldProgressSet
              ProgressInt a b = progress oldProgress
          putStrLn "Please input the new progress:"
          newProgress <- safeAskUserInput :: IO Int
          return $ Right . Right $ ModifyProgress path (ElemInSet choice) (ProgressInt newProgress b)

displayConcise plan = do
  putStrLn $ "Hello " ++ show (userName plan) ++ "!, here is your learning plan: \n"
  let Project _ _ mapProjects = root plan 
  mapM_ putStrLn $ M.foldMapWithKey (\_ v -> [prettyPrint Concise v]) mapProjects

interAct :: LearningPlan -> IO LearningPlan
interAct plan = do
  --displayConcise plan
  action <- askAction plan
  case action of
    Left err -> putStrLn err >> interAct plan
    Right (Left Quit) -> return plan
    Right (Left (DisplayDetail path)) -> performIOAction (DisplayDetail path) plan >> interAct plan
    Right (Left anyAction) -> performIOAction anyAction plan >> interAct plan
    Right (Right action) -> case performTreeAction action plan of
      Left err -> putStrLn err >> interAct plan
      Right newPlan -> interAct $ onProjectTree updateProgressFromTasks newPlan


onProjectTree :: (ProjectTree -> ProjectTree) -> LearningPlan -> LearningPlan
onProjectTree f plan = plan { root = f (root plan) }

userInterface :: IO ()
userInterface = do -- first detect file, create and prompt username if not found. Then load and interact
  fileExists <- doesFileExist filePathForLearningPlan
  if fileExists
    then do
      plan <- read <$> readFile' filePathForLearningPlan
      displayConcise plan
      plan' <- interAct plan
      writeFile filePathForLearningPlan (show plan')
    else do
      putStrLn "Please input your username:"
      name <- safeAskUserInput :: IO String
      let plan = emptyLearningPlan (UserName name)
      plan' <- interAct plan
      writeFile filePathForLearningPlan (show plan')
  
