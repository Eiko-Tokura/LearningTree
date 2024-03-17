# LearningTree

LearningTree is a simple program used to organize your complex learning plan with a tree structure.

It supports:
- Adding projects and tasks
- tracking progress by multiple self-defined methods
- auto summary of progress
- more features to come

Core concepts:
- Project: a big goal you want to achieve, like "learn algebraic geometry". Projects can be attached to a parent project, like "learn algebraic geometry" can be a subproject of "learn math".
- Task: a small step to achieve a project, like "read chapter 1 of a specific book".
- Projects form the nodes of the tree, and tasks form the leaves of the tree.
- Only add progress to tasks, not projects. Projects will automatically update their progress based on the progress of their tasks.

Example:

```
1->Gauss Manin : cards : (0/50)[>.|..........] : pages : (18/50)[=|==>.......]
  1->Gauss-Manin Connection : cards : (0/50)[>|...........] : pages : (18/50)[|===>.......]
    1->Calculus on Schemes : cards () : (0/50)[>|...........] : pages (Tiago's notes) : (18/50)[|===>.......]
2->Categorification
3->Quiver Variety
4->Programming : lines : (5000/20000)[|==>........] : pages : (60/216)[|==>........]
  1->Haskell : lines : (5000/20000)[|==>........] : pages : (60/216)[|==>........]
    1->Thinking with Type : pages (Thinking With Types) : (60/216)[|==>........]
    2->Coding : lines : (5000/20000)[|==>........]
      1->General Coding : lines () : (5000/20000)[|==>........]
5->Lambda and Types
6->Algebraic Geometry
```

currently we have just built the basic structure of our program, it is usable but not very user-friendly.
