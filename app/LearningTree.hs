{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE GADTs, ScopedTypeVariables, FlexibleContexts, FlexibleInstances, DeriveGeneric, DerivingVia, DataKinds, ConstraintKinds, KindSignatures, TypeApplications #-}
module LearningTree 
  ( Finite(..), Priority(..), Subject(..), TaskType(..), Status(..), Target(..), ProgressInt(..), ProgressTrackingType(..), Progress(..), ProgressTracking
  , ProjectData(..), TimeData(..), NodeType(..), Description(..), LearningUnit(..), ProjectTree(..), getUnitN, getUnitE
  , addTask, addProject, upgradeTaskToProject, localChangeTree, removeTaskOrProj, NodePath, ErrMsg
  , newTask, newProject, exampleProjectTree, PrettyPrint(..), PrintMode(..)
  , LU(..), OverallPriority(..), AskUserInput(..), LocalFunction, updateProgressFromTasks
  , setStatus, setProgress, setLU, getSubTree
  ) where

import AutoInput
import Data.Char (toUpper)
import Data.Time
import Data.Proxy
import Data.Coerce
import Data.Functor
import Data.Monoid
import GHC.Generics
import GHC.TypeLits
import Text.Read (readMaybe)
import qualified Data.Map as M
import qualified Data.Set as S
-- Copilot is indeed helpful sometimes, but it generates a lot of garbage code, we should carefully check the generated code and remove the unnecessary parts.
-- Consider turnning off Copilot once every a week to avoid overfitting.

-- this is a study task manager, this is the inner module containing data types.

-- natural numbers with a lower and upper bound, type safe, enum and bounded
newtype Finite (m :: Nat) (n :: Nat) = Finite Int deriving (Show, Eq, Ord, Num, Enum, Read) via Int
instance (KnownNat m, KnownNat n) => AskUserInput (Finite m n) where
  askUserInput = do
    putStrLn $ "input a number between " ++ show (natVal (Proxy @m)) ++ " and " ++ show (natVal (Proxy @n)) ++ ":"
    input <- getLine
    let input' = readMaybe input
    return $ case input' of
      Just x -> if x >= fromIntegral (natVal (Proxy @m)) && x <= fromIntegral (natVal (Proxy @n)) then Just (Finite x) else Nothing
      Nothing -> Nothing

instance (KnownNat m, KnownNat n) => Bounded (Finite m n) where
  minBound = Finite . fromIntegral $ natVal (Proxy @m)
  maxBound = Finite . fromIntegral $ natVal (Proxy @n)

type MaxPri = 9 -- max priority, type level constant
type MinPri = 0 -- min priority, type level constant
newtype Priority = Priority (Finite MinPri MaxPri) deriving (Show, Eq, Ord, Num, Read, AskUserInput) via (Finite MinPri MaxPri) -- user defined priority, from 0 to 9

newtype Subject = Subject { subjectName :: String } deriving (Ord, Eq, Show, Read, AskUserInput) via String
-- user defined subject, like math, physics, chemistry, etc.
newtype TaskType = TaskType { taskTypeName :: String } deriving (Ord, Eq, Show, Read) -- user defined task type, like exam, homework, project, etc.

data Status = NotStarted | InProgress | Completed | Suspended deriving (Show, Read, Eq, Ord)
instance AskUserInput Status where -- let user input status by N, I, C, S, case insensitive
  askUserInput = do
    putStrLn "input status (N for NotStarted, I for InProgress, C for Completed, S for Suspended):"
    input <- getLine -- should be case insensitive
    let input' = map toUpper input
    return $ case input' of
      "N" -> Just NotStarted
      "I" -> Just InProgress
      "C" -> Just Completed
      "S" -> Just Suspended
      _   -> Nothing

newtype Target = Target { targetName :: String } deriving (Show, Eq, Ord, Read) -- target of a task, like specific books
data ProgressInt = ProgressInt Int Int deriving (Show, Eq, Ord, Read) -- progress of a task, a fraction.
newtype ProgressTrackingType = ProgressTrackingType { progressTrackingTypeName :: String } deriving (Show, Eq, Ord, Read) -- user defined

addProgInt :: ProgressInt -> ProgressInt -> ProgressInt
addProgInt (ProgressInt a b) (ProgressInt c d) = ProgressInt (a+c) (b+d)

data Progress = Progress
  { progressType :: ProgressTrackingType -- user defined progress tracking methods
  , targets      :: S.Set Target  -- user defined target, like specific books
  , progress     :: ProgressInt -- progress of a task, a fraction with two integers.
  } deriving (Show, Eq, Ord, Read, Generic)

type ProgressTracking = S.Set Progress 
-- this map contains all the progress summarized for all children, for example, if a project has 3 children, and each child has a progress of page 10/20, exercise 20/30, page 30/40, then the progress of the project is page 60/100, exercise 20/30.

-- example structure:
--Algebra (Project) -- Linear Algebra (Proj)          -- Vectors (Task), by PageNumber, follwowing some book
--                                                    -- Matrices (Task)
--                  -- Group Theory (Proj)            -- Group (Task), by Notes
--                                                    -- Representation (Task), by Exercises
--ComputerScience (Project) -- Haskell (Proj)         -- Syntax (Task), by PageNumber
--                                                    -- Monad (Task), by Exercises
--                          -- Lambda Calculus (Proj) -- Syntax (Task), by PageNumber
--                                                    -- Application (Task), by Exercises
--
--New idea: we call all non-leaf nodes projects, and all leaf nodes tasks.
--the progress of projects are always computed by the progress of its children.
--the progress of tasks are observable in the calendar, and is meant to be updated by the user.
--
--the over all priority of a node is computed by the priority of its parent, and the priority of the node itself, for example, if a project has a priority of 5, and a task has a priority of 3, then the priority of the task is 5.3, we shall use a list [5,3] to represent the priority, comparing is done by comparing the list element by element, and the first element has the highest priority.
--

newtype OverallPriority = OverallPriority [Priority] deriving (Show, Read, Eq) -- the overall priority of a node, computed by the priority of its parent and itself.
instance Ord OverallPriority where
  (OverallPriority (x:xs)) `compare` (OverallPriority (y:ys)) = 
    case x `compare` y of
      EQ -> OverallPriority xs `compare` OverallPriority ys
      other -> other
  (OverallPriority []) `compare` (OverallPriority []) = EQ
  (OverallPriority []) `compare` _ = LT
  _ `compare` (OverallPriority []) = GT

data ProjectData = ProjectData
  { priority :: Priority
  , subject  :: Subject
  , totalChildren :: Data NoInput Int -- we use this to count the relativeId of the children
  } deriving (Show, Read, Eq, Generic)

instance AskUserInput ProjectData

data TimeData = TimeData
  { dateCreated :: Day
  , startDate   :: Day
  , endDate     :: Day
  , currentTime :: Data NoInput Day
  } deriving (Show, Read, Ord, Eq, Generic)

instance AskUserInput TimeData

data NodeType = TaskNode | ProjectNode | NoType  deriving (Show, Read, Eq, Ord)
-- used only at the type level, to distinguish between tasks and projects

data Description = Description 
  { title   :: String
  , content :: String
  } deriving (Show, Read, Eq, Ord, Generic)

instance AskUserInput Description

-- wrap LearningUnit in GADT

data LU (n :: NodeType) where
  TaskLU :: LearningUnit TaskNode    -> LU TaskNode
  ProjLU :: LearningUnit ProjectNode -> LU ProjectNode

data LearningUnit (n :: NodeType) = LearningUnit 
  { description      :: Data UserInput Description
  , timeData         :: Data UserInput TimeData
  , taskType         :: Data UserInput (S.Set TaskType) -- exams, homework, projects, etc.
  , progressTracking :: Data UserInput ProgressTracking -- page number, exercises, etc.
  , status           :: Data UserInput Status
  , relativeId       :: Data NoInput   Int -- given by the parent, counted in ProjectData
  } deriving (Show, Read, Eq, Ord, Generic)
type NInt = Data NoInput Int

instance AskUserInput (LearningUnit n)

data ProjectTree 
  = Task (LearningUnit TaskNode) -- leaf node
  | Project (LearningUnit ProjectNode) ProjectData !(M.Map NInt ProjectTree)
  deriving (Show, Read)

instance AskUserInput (S.Set TaskType) where 
  askUserInput = do
    putStrLn "input task types (separated by space, can be empty):"
    input <- getLine
    return $ Just . S.fromList . map TaskType . words $ input

instance AskUserInput ProgressTracking where
  askUserInput = do
    putStrLn "in which ways do you want to track your progress? (you can have multiple methods, like 'notes' or 'pages'. separated by space, can be empty):"
    input <- getLine
    let methods = words input
    progress <- mapM (\method -> do
      putStrLn $ "input the target for " ++ method ++ ":"
      target <- getLine --safeInput
      putStrLn $ "what is the goal for " ++ method ++ "? (like '20' or '30'):"
      goal <- safeInput
      putStrLn $ "what is the current progress for " ++ method ++ "? (like '10' or '20'):"
      current <- safeInput
      return $ Progress (ProgressTrackingType method) (S.singleton $ Target target) (ProgressInt current goal)
      ) methods
    return $ pure $ S.fromList progress

  --Task    :: LearningUnit TaskNode -> ProjectTree TaskNode
  --Project :: LearningUnit ProjectNode -> ProjectData -> AnyTrees -> ProjectTree ProjectNode
  -- we can't really use GADTs here because we need to include trees of different types in the same map, maybe we can use a type family to do this. 
  -- Or consider a more crazy idea: use a type level list to store the types
  -- Eg. ProjectTree [TaskNode, ProjectNode, TaskNode] means a tree with a task, a project, and a task in order.
  -- I think that is essentially building a type level tree, and we can use type families to do this.
  -- Another way is to use Either (ProjectTree TaskNode) (ProjectTree ProjectNode) to store the type, and use a map of this type.
 
getUnitN :: ProjectTree -> LearningUnit NoType
getUnitN (Task u) = coerce u
getUnitN (Project u _ _) = coerce u

getUnitE :: ProjectTree -> Either (LearningUnit TaskNode) (LearningUnit ProjectNode)
getUnitE (Task u) = Left u
getUnitE (Project u _ _) = Right u

getLU :: ProjectTree -> Either (LU TaskNode) (LU ProjectNode) -- enriched type info, for pattern matching
getLU (Task u) = Left $ TaskLU u
getLU (Project u _ _) = Right $ ProjLU u

type ErrMsg = String

newTask :: LearningUnit TaskNode -> ProjectTree
newTask = Task

newProject :: LearningUnit ProjectNode -> ProjectData -> ProjectTree
newProject learnUnit projData = Project learnUnit projData M.empty

setStatus :: Status -> LocalFunction -- ProjectTree -> Either ErrMsg ProjectTree
setStatus status (Task learnUnit) = Right $ Task learnUnit { status = Data status }
setStatus status (Project learnUnit projData projects) = Right $ Project learnUnit { status = Data status } projData projects

setProgress :: ProgressTracking -> LocalFunction -- ProjectTree -> Either ErrMsg ProjectTree, you should only set progress to a task
setProgress progress (Task learnUnit) = Right $ Task learnUnit { progressTracking = Data progress }
setProgress _ (Project {}) = Left "Cannot set progress to a project! You can only set progress to a task."

setLU :: LearningUnit NoType -> LocalFunction -- ProjectTree -> Either ErrMsg ProjectTree
setLU learnUnit (Task _) = Right $ Task (coerce learnUnit)
setLU learnUnit (Project _ projData projects) = Right $ Project (coerce learnUnit) projData projects

addTask 
  :: LearningUnit TaskNode -- the task to be added, cannot add a project as it requires a ProjectData
  -> ProjectTree -- this is the exact node where we want to add the task 
  -> Either ErrMsg ProjectTree -- the new (sub)tree
addTask _ (Task _) = Left "Cannot add task or project to a task! You can only add tasks or projects to a project."
addTask t (Project learnUnit projData projects) = Right $ Project learnUnit (increaseCount projData) (M.insert (totalChildren projData + 1) (giveRelativeId (Task t)) projects)
  where
    increaseCount (ProjectData p s n) = ProjectData p s (n + 1)
    giveRelativeId (Task t) = Task t { relativeId = totalChildren projData + 1 }
    giveRelativeId (Project {}) = error "we are adding task here, should not be adding a project"

addProject 
  :: LearningUnit ProjectNode -- the project to be added
  -> ProjectData -- the data of the project
  -> LocalFunction -- ProjectTree -> Either ErrMsg ProjectTree
addProject _ _ (Task _) = Left "Cannot add task or project to a task! You can only add tasks or projects to a project."
addProject learnUnit projData (Project learnUnit' projData' projects) = Right $ Project learnUnit' (increaseCount projData') (M.insert (totalChildren projData' + 1) (giveRelativeId (Project learnUnit projData M.empty)) projects)
  where
    increaseCount (ProjectData p s n) = ProjectData p s (n + 1)
    giveRelativeId (Project learnUnit projData projects) = Project learnUnit { relativeId = totalChildren projData' + 1 } projData projects 
    giveRelativeId (Task t) = Task t { relativeId = totalChildren projData' + 1 }

upgradeTaskToProject :: ProjectData -> LocalFunction -- ProjectTree -> Either ErrMsg ProjectTree
upgradeTaskToProject _ Project{} = Left "Cannot upgrade a project to a project! You can only upgrade a task to a project."
upgradeTaskToProject projData (Task learnUnit) = Right $ Project (coerce learnUnit) projData M.empty -- coerce is safe here because we are sure that the type is TaskNode, and the target type is ProjectNode

getSubTree :: NodePath -> ProjectTree -> Either ErrMsg ProjectTree
getSubTree [] tree = Right tree
getSubTree (x:xs) (Project _ _ projects) = 
  case M.lookup (Data x) projects of
    Nothing -> Left "Invalid path"
    Just t -> getSubTree xs t
getSubTree (_:_) (Task _) = Left "Invalid path"

instance AskUserInput [Int] where
  askUserInput = do
    putStrLn "input the path of the node you want to access (separated by space, like 1 2, empty for root):"
    mapM readMaybe . words <$> getLine

type LocalFunction = ProjectTree -> Either ErrMsg ProjectTree
type NodePath = [Int] -- the path to a node, like [1,2,3] means the 3rd child of the 2nd child of the 1st child of the root
localChangeTree
  :: LocalFunction -- the function to change the tree
  -> ProjectTree -- the tree to be changed
  -> NodePath -- the path to the node to be changed
  -> Either ErrMsg ProjectTree -- the new tree
-- first find the path, then pass the function, and return the new tree
-- if the path is invalid, return an error message in the Either monad
-- it can be used to update any node, including the root and leaf nodes
localChangeTree func tree [] = func tree
localChangeTree func (Project learnUnit projData projects) (x:xs) = 
  case M.lookup (Data x) projects of
    Nothing -> Left "Invalid path"
    Just t -> do
      newTree <- localChangeTree func t xs
      return $ Project learnUnit projData (M.insert (Data x) newTree projects)
localChangeTree _ (Task _) (_:_) = Left "invalid path"

removeTaskOrProj :: ProjectTree -> NodePath -> Either ErrMsg ProjectTree -- actually we can use the same function to remove a project, but not the root.
removeTaskOrProj (Task _) _ = Left "Cannot remove a task from a task! You can only remove tasks from a project."
removeTaskOrProj (Project {}) [] = Left "Invalid path"
removeTaskOrProj (Project learnUnit projData projects) [x] = 
  case M.lookup (Data x) projects of
    Nothing -> Left "Invalid path"
    Just _ -> Right $ Project learnUnit projData (M.delete (Data x) projects) -- you don't have to decrease the totalChildren counter, because we want it to be only increasing to avoid conflicts. So relativeId is always unique and stands for an absolute id of creation.
removeTaskOrProj (Project l p projects) (x:y:ys) = 
  case M.lookup (Data x) projects of
    Nothing -> Left "Invalid path"
    Just subproj -> removeTaskOrProj subproj (y:ys) <&> \new -> Project l p $ M.update (const $ Just new) (Data x) projects

mergeProgressOfSameType p1 p2 = Progress (progressType p1) (targets p1 `S.union` targets p2) (progress p1 `addProgInt` progress p2)

sumProgressByType :: [ProgressTracking] -> ProgressTracking -- = S.Set Progress
sumProgressByType list = 
  let l  = map (\prog -> (progressType prog, prog)) $ concatMap S.toList list
      m  = M.fromListWith mergeProgressOfSameType l
      m' = M.unionsWith mergeProgressOfSameType
         $ map (uncurry M.singleton) l
  in S.fromList $ M.elems m'

updateProgressFromTasks :: ProjectTree -> ProjectTree -- this function updates all the progress of projects from the progress of its children, eventually relying only on the leaf nodes
updateProgressFromTasks (Task u) = Task u
updateProgressFromTasks (Project u p projects) = Project u' p projects'
  where u' = u { progressTracking = Data $ sumProgressByType $ map (unData . progressTracking . getUnitN . snd) $ M.toList projects' }
        projects' =  updateProgressFromTasks <$> projects

-- we need to provide an interface to manage study plans
-- I think we should organize the subjects in a tree, allowing us to break down big tasks into smaller ones, and complete them part by part.
-- for example I want to learn Haskell, I can break it down into smaller tasks, like learning the syntax, learning the type system, learning the IO system, etc.
--
-- Let's define some terms:
-- A project is a big (and usually ambiguous) task, it can be broken down into smaller tasks, like learning Haskell
-- A task is a smaller, well defined task, like learning the syntax of Haskell, Monad, etc.
-- Tasks and projects should use a same inner data structure and an external wrapper data structure, so that we can manage them in a unified way.
--
-- A project is a tree in its own
-- The progress of any node should be the sum of the progress of its children. 
-- Only the leaves (which are nodes with no children) should have a user modifiable progress tracking, and is observable in the calendar
--
-- these data are associated with tasks (nodes):
--
-- these data are associated with projects (trees):
-- priority, taskType, subject, status
--
-- We should support the following operations:
-- 1. add, remove, update a task or project
-- 2. change the status of a task, if you change the status of a parent, all the children should be updated as well.
--

data PrintMode = Concise | Verbose | SystemShow
class PrettyPrint a where
  prettyPrint :: PrintMode -> a -> String

--type FilterMode = Maybe (

exampleProjectTree :: ProjectTree
exampleProjectTree = updateProgressFromTasks $
  Project 
    (LearningUnit 
      (Data $ Description "Math" "Learn Math")
      (Data $ TimeData 
        (fromGregorian 2021 1 1) 
        (fromGregorian 2021 1 1) 
        (fromGregorian 2021 3 1)
        (Data $ fromGregorian 2021 2 1)
      ) 
      (Data $ S.singleton (TaskType "exam")) 
      (Data S.empty) (Data NotStarted)
      (Data 1)
    ) 
    (ProjectData (Priority 5) (Subject "Math") 2) -- this number increases as we add children
  (M.fromList [ (1, Task 
      (LearningUnit 
        (Data $ Description "Linear Algebra" "Learn Linear Algebra")
        (Data $ TimeData (fromGregorian 2021 1 1) (fromGregorian 2021 1 1) (fromGregorian 2021 3 1) (Data $ fromGregorian 2021 2 1)) 
        (Data $ S.singleton (TaskType "exam")) 
        (Data S.empty) (Data NotStarted)
        (Data 1)
      ))
  , (2, Task 
      (LearningUnit 
        (Data $ Description "Group Theory" "Learn Group Theory")
        (Data $ TimeData (fromGregorian 2021 1 1) (fromGregorian 2021 1 1) (fromGregorian 2021 3 1) (Data $ fromGregorian 2021 2 1)) 
        (Data $ S.singleton (TaskType "exam")) 
        (Data $ S.fromList [Progress (ProgressTrackingType "notes") (S.singleton $ Target "book") (ProgressInt 10 20), Progress (ProgressTrackingType "exercises") (S.singleton $ Target "book") (ProgressInt 30 30)]) 
        (Data NotStarted)
        (Data 2) -- refered as node 1.2, or a list of integers [1,2]
      ))
  ])
