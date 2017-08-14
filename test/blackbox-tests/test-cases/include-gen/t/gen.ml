
let () =
  let x = 1000000000 in
  let i =
    let rec loop a x =
      if a = 0 then x
      else loop (a - 1) (x + 1)
    in
    loop x 0
  in
  assert (i = x);
  Printf.printf "(rule
  ((targets (x))
   (deps    ())
   (action  (copy asd x))))"

