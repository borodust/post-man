# POST-MAN

Avoid annoying skinbags and tidy up storage rooms: bring stray boxes into a pile
they belong. Does there anything possibly more exciting exist for a storage
robot?

## Controls
| Action  | Keyboard | Gamepad |
|---------|---------|---------|
| Movement/Select  | WASD, Arrows | D-pad |
| Pick/Drop/Action | Enter | X |


## Installation and running

Binaries available at [releases](https://github.com/borodust/post-man/releases)
page.


To run from REPL, clone project into `~/quicklisp/local-projects/` and
```lisp
(ql:register-local-projects)

(ql:quickload :post-man)

(post-man:run)
```

## Requirements

* OpenGL 2.1+
* 64-bit (x86_64) Windows, GNU/Linux or macOS
* x86_64 SBCL or CCL
