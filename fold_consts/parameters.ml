open Core_kernel

type options = {
  mutable fix_sp : string option option;
  mutable resolve_loads : [`no | `ro | `rw];
}

let options = {
  fix_sp = None;
  resolve_loads = `ro;
}
