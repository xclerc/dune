  $ $JBUILDER build -j1 x --root .
      ocamldep t/gen.depends.ocamldep-output
        ocamlc t/gen.{cmi,cmo,cmt}
      ocamlopt t/gen.{cmx,o}
      ocamlopt t/gen.exe
           gen t/b.jbuild
