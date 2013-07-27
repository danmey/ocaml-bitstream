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

  let buffer_size = 1

  (* After long hesistation came to conlusion maybe a simple
     amortised implementation with Int64.t would be better *)
  type t = { buffer : I.t;
             mutable size : int;
             mutable bitfields : bitfield list;
             mutable pending : bitfield }
  and bitfield = bitsize * I.block
  and bitsize = int

(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |                            Implementation                             | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

  let update s =
    let bitfields = List.rev s.bitfields in
    s.bitfields <- [];
    s.size <- 0;
    s.pending <-
      List.fold_left
      (fun (size, bits) (bitsize, bitfield) ->
        if size + bitsize >= I.block_size then
      g    let size' = I.block_size - size in
          let bitfield' = I.upper size' bitfield in
          let bits = I.concat size' bits bitfield' in
          I.put s.buffer bits;
          size + bitsize - I.block_size, bits
        else size+bitsize, I.concat bitsize bits bitfield)
      s.pending bitfields

  let create size =
    { buffer = I.create size;
      size = 0;
      pending = 0, I.off;
      bitfields = [] }

  let contents s = I.contents s.buffer

  let append s n v =
    s.bitfields <- (n, v) :: s.bitfields;
    s.size <- s.size+1;
    if s.size >= buffer_size then update s

  let bit s b =
    append s 1 (if b then I.on else I.off)

  let flush s =
    let size, block = s.pending in
    s.pending <- (0, I.off);
    let left = I.block_size - size in
    if left <> 0 then
      I.flush s.buffer size block
    else update s


(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |                              Push n bits                              | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

  let bits s n v = append s n v
  let many_bits s n v = ()
  let nibble _ _ = ()
  let byte _ _ = ()
  let word _ _ = ()
  let dword _ _ = ()
  let qword _ _ = ()
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
