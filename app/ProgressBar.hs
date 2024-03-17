{-# LANGUAGE GADTs, DataKinds, KindSignatures, FlexibleInstances #-}
module ProgressBar where

import Data.Time
import LearningTree
import AutoInput
import qualified Data.Map as M
import qualified Data.Set as S

-- | Progress bar looks like [====|>....]
--   where = means completed part, > means current, . means unfinished, | means the current time progress
--   '|>' or '==|=>' would mean the current progress is ahead of schedule
--   '==|' would mean the current progress is on schedule
--   '==>..|..' would mean the current progress is behind schedule
--   '[=========]' would mean the task is completed
--   the big bracket is the time frame of the task
--   '[====>....]|' would mean the time is over, but the task is not completed
--   '|[====>....]' would mean the time is not started yet, but we already have some progress
--
-- A project can have many tasks
-- A task may have multiple progress bars of different type (take 'notes', 'pages', 'exercises' as an example)
-- A project will have a summarized progress bar for each type of progress, 
--    with time frame counted on its own, not its tasks
--    whose progress is the sum of all its tasks' progress, summing on the numerators and denominators

type CurrentTime = Day

data ProgressBar (t :: NodeType) where
  TaskPB :: Progress -> TimeData -> ProgressBar TaskNode -- progress contains detailed information, pbtrackingtype, target info, progress fraction.
  ProjPB :: Progress -> TimeData -> ProgressBar ProjectNode -- less detailed, only summary on a specific type of progress tracking.

type PBsByType t = M.Map ProgressTrackingType (ProgressBar t)
getPB :: LU t -> PBsByType t
getPB (ProjLU lu) = let setProgress = unData $ progressTracking lu
                    in M.fromList $ map (\ptt -> (progressType ptt, ProjPB ptt (unData $ timeData lu))) $ S.toList setProgress
getPB (TaskLU lu) = let setProgress = unData $ progressTracking lu
                    in M.fromList $ map (\ptt -> (progressType ptt, TaskPB ptt (unData $ timeData lu))) $ S.toList setProgress
  
instance PrettyPrint (M.Map ProgressTrackingType (ProgressBar TaskNode)) where
  prettyPrint m = M.foldMapWithKey (\k v -> " : " ++ prettyPrint m v ) 

instance PrettyPrint (M.Map ProgressTrackingType (ProgressBar ProjectNode)) where
  prettyPrint m = M.foldMapWithKey (\k v -> " : " ++ prettyPrint m v ) 

instance PrettyPrint (ProgressBar TaskNode) where
  prettyPrint m (TaskPB p t) = progressTypeStr ++ " " ++ targetsStr ++ " : " ++ progressInfo ++ progressBarStr
    where targetsStr = if not (S.null (targets p))
              then "(" ++ foldr1 (\x acc -> acc ++ " " ++ x) (map targetName $ S.toList $ targets p) ++ ")"
              else ""
          progressTypeStr = progressTrackingTypeName $ progressType p
          progressBarStr = progressBar (progress p) (startDate t, unData $ currentTime t, endDate t)
          progressInfo = case m of
            Concise -> let ProgressInt a b = progress p in "(" ++ show a ++ "/" ++ show b ++ ")"
            Verbose -> let ProgressInt a b = progress p in "(" ++ show a ++ "/" ++ show b ++ ")"
            SystemShow -> show p

instance PrettyPrint (ProgressBar ProjectNode) where
  prettyPrint m (ProjPB p t) = progressTrackingTypeName (progressType p) ++ " : " ++ progressInfo ++ progressBar (progress p) (startDate t, unData $ currentTime t, endDate t)
    where progressInfo = case m of
            Concise -> let ProgressInt a b = progress p in "(" ++ show a ++ "/" ++ show b ++ ")"
            Verbose -> let ProgressInt a b = progress p in "(" ++ show a ++ "/" ++ show b ++ ")"
            SystemShow -> show p

progressBar :: ProgressInt -> (Day, CurrentTime, Day) -> String
progressBar (ProgressInt a b) (start, current, end) =
  let completion    = fromIntegral a / max 1 (fromIntegral b) :: Rational
      timeProgress  = fromIntegral (diffDays current start) / max 1 (fromIntegral (diffDays end start)) :: Rational
      discrete dbl = round (dbl * fromIntegral n)
      isCompleted  = completion >= 1
      isAhead      = completion > timeProgress
      isOverDue    = timeProgress > 1
      compd = discrete completion
      ahead = discrete $ completion - timeProgress
      timed = discrete timeProgress
      n = 12 :: Int
  in case (isCompleted, isAhead, isOverDue) of
    (True, _, _) -> "[" ++ replicate ((n - 2) `div` 2) '=' ++ "owo!" ++ replicate ((n - 3) `div` 2) '=' ++ "]"
    (_, _, True) -> "[" ++ replicate compd '=' ++ ">" ++ replicate (n - compd - 1) '.' ++ "]|"
    (_, True, _) -> "[" ++ replicate timed '=' ++ "|" ++ replicate (ahead - 1) '=' ++ ">" ++ replicate (n - compd - 1) '.' ++ "]"
    (_, _, _   ) -> "[" ++ replicate compd '=' ++ ">" ++ replicate (timed - compd) '.' ++ "|" ++ replicate (n - timed - 1) '.' ++ "]"

safeFoldr1 f acc def [] = def
safeFoldr1 f acc def (x:xs) = foldr1 f (x:xs)

instance PrettyPrint ProjectTree where
  prettyPrint mode = withPadding 0
    where
      withPadding :: Int -> ProjectTree -> String
      withPadding n (Task u)               = "\n" ++ padding n ++ task (prettyPrint mode u) ++ prettyPrint mode (getPB (TaskLU u))
      withPadding n (Project u _ projects) 
        = "\n" ++ padding n 
        ++ project ( prettyPrint mode u 
                     ++ prettyPrint mode (getPB (ProjLU u)) 
                     ++ safeFoldr1 (++) "" "" (map (withPadding (n+1)) $ M.elems projects)
                     -- ++ M.foldMapWithKey (\_ v -> withPadding (n+1) v) projects
                   )
      padding n = replicate (n*2) ' '
      task s = case mode of
        Concise -> s
        Verbose -> "Task:" ++ s
        SystemShow -> "(Task " ++ s ++ ")"
      project s = case mode of
        Concise -> s
        Verbose -> "Proj:" ++ s
        SystemShow -> "(Project " ++ s ++ ")"

instance PrettyPrint (LearningUnit a) where
  prettyPrint Concise u = show (relativeId u) ++ "->" ++ title (unData $ description u) -- ++ "  " ++ show (startDate (timeData u)) ++ "~" ++ show (endDate (timeData u))
  prettyPrint Verbose u = show (relativeId u) ++ "->" ++ title (unData $ description u) ++ " " ++ show (timeData u) ++ " " ++ show (taskType u) ++ " " ++ show (progressTracking u) ++ " " ++ show (status u)
  prettyPrint SystemShow u = show u
