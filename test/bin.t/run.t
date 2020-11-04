# CLI tests

## A term coming from a file
  $ mtt parse unit.mtt
  ()

  $ mtt infer unit.mtt
  ()

  $ mtt check '()' unit.mtt

  $ mtt eval unit.mtt
  ()

## A term coming from stdin (heredoc syntax)
  $ mtt parse <<EOF
  > ()
  > EOF
  ()

  $ mtt check '()' <<EOF
  > ()
  > EOF

  $ mtt infer <<EOF
  > ()
  > EOF
  ()

  $ mtt eval <<EOF
  > ()
  > EOF
  ()

## Missing file
  $ mtt parse MISSING_FILE.mtt
  mtt: FILE argument: no `MISSING_FILE.mtt' file
  Usage: mtt parse [OPTION]... [FILE]
  Try `mtt parse --help' or `mtt --help' for more information.
  [124]

  $ mtt check '()' MISSING_FILE.mtt
  mtt: FILE argument: no `MISSING_FILE.mtt' file
  Usage: mtt check [OPTION]... TYPE [FILE]
  Try `mtt check --help' or `mtt --help' for more information.
  [124]

  $ mtt infer MISSING_FILE.mtt
  mtt: FILE argument: no `MISSING_FILE.mtt' file
  Usage: mtt infer [OPTION]... [FILE]
  Try `mtt infer --help' or `mtt --help' for more information.
  [124]

  $ mtt eval MISSING_FILE.mtt
  mtt: FILE argument: no `MISSING_FILE.mtt' file
  Usage: mtt eval [OPTION]... [FILE]
  Try `mtt eval --help' or `mtt --help' for more information.
  [124]

