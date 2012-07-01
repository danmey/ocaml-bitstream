(*----------------------------------------------------------------------------
  Imperative bitstreams
  Copyright (C) 2012 Wojciech Meyer

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU Lesser General Public License as published by
  the Free Software Foundation, either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
  --------------------------------------------------------------------------*)

(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |                      Create imperative bitstream                      | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

module Imperative (I : Sig.OTARGET_IMP
                   with type block = int) : Sig.O_IMP
  with type target = I.target
  = struct

  type word
  and dword
  and qword
  and target = I.target

  let endianess = `Little

  type t = { buffer : I.t;
             mutable pending : I.block;
             mutable bit : int; }

(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |                            Implementation                             | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

  let create size =
    { buffer = I.create size;
      pending = 0;
      bit = 0; }

  let contents s = I.contents s.buffer

  let update s =
    I.put s.buffer s.pending;
    s.bit <- 0;
    s.pending <- 0

  let bit s b =
    s.pending <- (s.pending lsl 1) lor (if b then 1 else 0);
    s.bit <- s.bit+1;
    if s.bit >= I.block_size then begin
      I.put s.buffer s.pending;
      update s;
    end

  (* FIXME: Should be functorised *)
  let mask v = v land (1 lsl I.block_size - 1)

(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |                            Generate masks                             | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

  let masks =
    let a = Array.make Nativeint.size 0 in
    Array.iteri (fun i _ -> a.(i) <- (1 lsl i) - 1) a;
    a

(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |                              Slice bits                               | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

  let bit_slice l n v = (v lsr l) land masks.(n)

(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |                              Push n bits                              | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

  let bits s n v =
    let rec loop n =
      let left_bits = I.block_size - s.bit in
      let insert_bits = min n left_bits in
      let pending_bits = n - insert_bits in
      s.pending <- s.pending lsl left_bits;
      if pending_bits = 0
      then s.pending <- s.pending lor (bit_slice 0 insert_bits v)
      else begin
        s.pending <- s.pending lor (bit_slice pending_bits s.bit v);
        update s;
        loop pending_bits
      end
    in
    loop n

  let many_bits _ _ _ = ()
  let nibble _ _ = ()
  let byte _ _ = ()
  let word _ _ = ()
  let dword _ _ = ()
  let qword _ _ = ()
  let flush _ = ()
  let bool _ _ = ()
  let int32 _ _ = ()
  let int64 _ _ = ()
  let int _ _ = ()

end
  (* module Persistent (T : Sig.OTARGET_FUN) : Sig.O_FUN *)

(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |                      Streams without exceptions                       | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

(* module Exceptionless = struct *)
(*   module Imperative (T : Sig.OTARGET_IMP) : Sig.O_IMP = struct *)
(*     type word *)
(*     and dword *)
(*     and qword *)
(*     type t *)
(*     let endianess = `Little *)
(*     let bit _ _ = () *)
(*     let bits _ _ _ = () *)
(*     let many_bits _ _ _ = () *)
(*     let nibble _ _ = () *)
(*     let byte _ _ = () *)
(*     let word _ _ = () *)
(*     let dword _ _ = () *)
(*     let qword _ _ = () *)
(*     let flush _ = () *)
(*   end *)
(* end *)
(* module Persistent (T : Sig.OTARGET_FUN) : Sig.O_FUN *)

(* end *)
