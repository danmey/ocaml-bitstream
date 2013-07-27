(*----------------------------------------------------------------------------
  String based implementations
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
(* |                 Minimal signature for implementation                  | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

module type IMPL = sig
  type block
  type buffer
  val block_size : int
  val store : buffer -> int -> block -> int
  val store_n : buffer -> int -> block -> int -> int
  val create : int -> buffer
  val blit : buffer -> buffer -> int -> unit
  val concat : int -> block -> block -> block
  val upper : int -> block -> block
  val on : block
  val off : block
end

module Make (I : IMPL) : Sig.OTARGET_IMP
  with type block = I.block
  and type target = I.buffer
= struct

  type t = { mutable buffer : target;
             mutable size : int;
             mutable pos : int }
  and target = I.buffer
  and block = I.block

  let block_size = I.block_size
  let concat = I.concat
  let upper = I.upper
  let on = I.on
  let off = I.off

(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |                       Create stream from string                       | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

  let create size =
    { buffer = I.create size;
      size = size;
      pos = 0 }

  let contents s = s.buffer

  let resize state size =
    let buffer' = I.create size in
    let blit_size = min state.size size in
    I.blit state.buffer buffer' blit_size;
    state.buffer <- buffer';
    state.size <- size

  let resize_if_needed state =
    if state.size - state.pos < block_size lsr 3 then
      begin
        let size' =
          (if state.size = 0
           then block_size lsr 3
           else state.size) * 2
        in
        resize state size'
      end

  let put state block =
    resize_if_needed state;
    state.pos <- I.store state.buffer state.pos block

  let flush state count block =
    resize_if_needed state;
    state.pos <- I.store_n state.buffer state.pos block count;
    resize state state.pos

end

(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |                        Mask a byte from block                         | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

module type BUFFER = sig
  type buffer and block
  val set : buffer -> int -> block -> unit
  val blit : buffer -> buffer -> int -> unit
  val create : int -> buffer
end

(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |                     64 bit integer implementation                     | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

module ByteImpl(B : BUFFER with type block = int) : IMPL
  with type block = int
  and type buffer = B.buffer
= struct
  include B

  let block_size = 8

  let concat size v1 v2 = (v1 lsl size) lor v2
  let upper size v = v lsl (block_size - size)

  let off = 0
  let on = 1

  let store str pos byte =
    B.set str pos byte;
    pos+1

  let store_n str pos byte c =
    B.set str pos byte;
    pos+1
end

(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |                       ... and our string buffer                       | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

module StringB : BUFFER
  with type buffer = string
  and type block = int = struct
  type buffer = string
  and block = int
  let set str pos block =
    String.set str pos
      (Char.chr
            (block land 0xFF))
  let blit str str' size = String.blit str 0 str' 0 size
  let create = String.create
end

(* module String32 = Make(Impl32(StringB)) *)
module String64 = Make(ByteImpl(StringB))
