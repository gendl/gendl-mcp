
# GendL Training 

This is a collaborative effort between Genworks and Mike Twelves, with
the intention of releasing the materials under a shared copyright


## Compiling/Loading


First, (re)generate the `training.asd` file if necessary (only needed if files were added/removed/renamed):

```
(cl-lite ".../path/to/training/" :create-asd-file? t)
```

Next, set up Quicklisp (this can be in your `gdlinit.cl`):

```
(load-quicklisp)
(pushnew ".../path/to/training/" ql:*local-project-directories*)
(ql:quickload :training)

```

To visit the toplevel you can go to

  `http://localhost:9000/gendl-self-start-backdoor`

or to any of the tutorial sets with e.g.

  `http://localhost:9000/t2-backdoor`


  





