#auca --- the auto-caller

`auca` automatically calls an external command based on one or more file modifications.

#Dependencies

###System

- `sed` utility
- Linux system (because of the use of inotify)

###Haskell packages from Hackage

- `hinotify`
- `cmdargs`

#Install

Get GHC and just run `make` in the `src/` folder to generate the `auca` binary.
