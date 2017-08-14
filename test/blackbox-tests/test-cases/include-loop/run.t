  $ $JBUILDER build -j1 x --root .
  No rule found for _build/default/a/x
  the error may be caused by dependency cycle with include:
  _build/default/a/x
  -> _build/default/b.jbuild
  -> rules of _build/default/a
  [1]
