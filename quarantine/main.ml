class main_context k p = object
  inherit tainted_context p
  val k = k
  method step = if k > 0 then Some {< k = k - 1 >} else None
end

class ['a] main k p = object
  constraint 'a = #main_context
  inherit ['a] taint_propagator
  inherit ['a] sanitizer
end


let compute_result (ctxt : #context)  =
  let checked = ctxt#sanitized in
  let all_taints = ctxt#all_taints in
  let maybe = Set.diff all_taints checked in
  let live = ctxt#live_taints in
  let dead = Set.diff maybe live in
  `Cured checked, `Uncured (Set.inter maybe live), `Dead dead
