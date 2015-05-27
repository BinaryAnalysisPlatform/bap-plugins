open Core_kernel.Std
open Bap.Std
open Project

class custom ?image ?sym mem blk = object(self)
  inherit ARM.ABI.stub
  method! id = ["gnueabi"; "linux"; "unknown"; "custom"]
  method! specific = true
  method! choose other =
    if List.mem other#id "gnueabi" then
      Int.compare (List.length self#id) (List.length other#id)
    else 0

  method! return_value = Some (Bil.var ARM.CPU.r0)
  method! args =
    let vars = match sym with
      | Some "__gets_chk"
      | Some "__printf_chk" -> [ARM.CPU.r0; ARM.CPU.r1]
      | Some "__strcat_chk"
      | Some "__stpcpy_chk" -> [ARM.CPU.r0; ARM.CPU.r1; ARM.CPU.r2]
      | Some "__memmove_chk"
      | Some "__fgets_chk"
      | Some "__strncat_chk"
      | Some "__strncpy_chk"
      | Some "__memcpy_chk"
      | Some "__mempcpy_chk"
      | Some "__memset_chk" -> [ARM.CPU.r0; ARM.CPU.r1; ARM.CPU.r2; ARM.CPU.r3]
      | Some "__sprintf_chk"
      (* note: contains, var args *)
      | Some "__vsprintf_chk" -> [ARM.CPU.r0; ARM.CPU.r1; ARM.CPU.r2;
                                  ARM.CPU.r3; ARM.CPU.r4]
      (* note: contains, var args *)
      | Some "__vsnprintf_chk" -> [ARM.CPU.r0; ARM.CPU.r1; ARM.CPU.r2;
                                   ARM.CPU.r3; ARM.CPU.r4; ARM.CPU.r5]
      | Some _
      | None -> [] in
    List.map vars ~f:(fun r -> sym, Bil.var r)
  method! records = [[]]
end
