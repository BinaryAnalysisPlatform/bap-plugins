open Bap.Std
module Stmt = Bil_piqi

module rec Lifted_image_piqi:
  sig
    type uint64 = int64
    type instruction = Stmt.stmt list
    type lifted_instruction = Lifted_instruction.t
    type lifted_region = Lifted_image_piqi.lifted_instruction list
    type lifted_regions = Lifted_image_piqi.lifted_region list
  end = Lifted_image_piqi
and Lifted_instruction:
  sig
    type t = {
      mutable instruction: Lifted_image_piqi.instruction;
      mutable length: int;
      mutable offset: Lifted_image_piqi.uint64;
    }
  end = Lifted_instruction


let rec parse_int x = Piqirun.int_of_zigzag_varint x
and packed_parse_int x = Piqirun.int_of_packed_zigzag_varint x

and parse_uint64 x = Piqirun.int64_of_varint x
and packed_parse_uint64 x = Piqirun.int64_of_packed_varint x

and parse_instruction x =
  Piqirun.parse_list (Stmt.parse_stmt) x


and parse_lifted_instruction x =
  let x = Piqirun.parse_record x in
  let _instruction, x = Piqirun.parse_required_field 1 parse_instruction x in
  let _length, x = Piqirun.parse_required_field 2 parse_int x in
  let _offset, x = Piqirun.parse_required_field 3 parse_uint64 x in
  Piqirun.check_unparsed_fields x;
  {
    Lifted_instruction.instruction = _instruction;
    Lifted_instruction.length = _length;
    Lifted_instruction.offset = _offset;
  }

and parse_lifted_region x =
  Piqirun.parse_list (parse_lifted_instruction) x


and parse_lifted_regions x =
  Piqirun.parse_list (parse_lifted_region) x



let rec gen__int code x = Piqirun.int_to_zigzag_varint code x
and packed_gen__int x = Piqirun.int_to_packed_zigzag_varint x

and gen__uint64 code x = Piqirun.int64_to_varint code x
and packed_gen__uint64 x = Piqirun.int64_to_packed_varint x

and gen__instruction code x = (Piqirun.gen_list (Stmt.gen__stmt)) code x

and gen__lifted_instruction code x =
  let _instruction = Piqirun.gen_required_field 1 gen__instruction x.Lifted_instruction.instruction in
  let _length = Piqirun.gen_required_field 2 gen__int x.Lifted_instruction.length in
  let _offset = Piqirun.gen_required_field 3 gen__uint64 x.Lifted_instruction.offset in
  Piqirun.gen_record code (_instruction :: _length :: _offset :: [])

and gen__lifted_region code x = (Piqirun.gen_list (gen__lifted_instruction)) code x

and gen__lifted_regions code x = (Piqirun.gen_list (gen__lifted_region)) code x


let gen_int x = gen__int (-1) x
let gen_uint64 x = gen__uint64 (-1) x
let gen_instruction x = gen__instruction (-1) x
let gen_lifted_instruction x = gen__lifted_instruction (-1) x
let gen_lifted_region x = gen__lifted_region (-1) x
let gen_lifted_regions x = gen__lifted_regions (-1) x


let rec default_int () = 0
and default_uint64 () = 0L
and default_instruction () = []
and default_lifted_instruction () =
  {
    Lifted_instruction.instruction = default_instruction ();
    Lifted_instruction.length = default_int ();
    Lifted_instruction.offset = default_uint64 ();
  }
and default_lifted_region () = []
and default_lifted_regions () = []


include Lifted_image_piqi
