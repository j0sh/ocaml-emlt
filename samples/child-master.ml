let () = Child.print ~layout:Master.print ()
let layout_with = (Master.print, ())
let () = Child.print ~layout_with ()
