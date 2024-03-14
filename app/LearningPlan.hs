{-# LANGUAGE DataKinds #-}
module LearningPlan where

import LearningTree

newtype UserName = UserName String
  deriving (Show, Read)

data LearningPlan = LearningPlan 
  { userName :: UserName
  , root     :: ProjectTree -- root is a project, it contains all the projects and tasks
  } deriving (Show, Read)

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

data UserAction = AddProject NodePath (LearningUnit ProjectNode) ProjectData
                | AddTask NodePath (LearningUnit TaskNode)
                | SetStatus NodePath Status
                | SetProgress NodePath Progress -- you should only set progress to a task
                | SetLU NodePath (LearningUnit NoType)
                | DisplayDetail NodePath
                -- | RemoveNode NodePath
                -- | MoveNode NodePath NodePath


-- consider using Generics to derive the 'askUserInput' function for generic types



