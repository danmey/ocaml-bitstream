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
  val store : buffer -> int -> int -> block -> int
  val create : int -> buffer
  val blit : buffer -> buffer -> int -> unit
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

(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |                       Create stream from string                       | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

  let create size =
    { buffer = I.create size;
      size = size;
      pos = 0 }

  let contents s = s.buffer

  let put state block =
    if state.size - state.pos < block_size lsr 3 then
      begin
        let size' = (if state.size = 0 then 1 else state.size) * 2 in
        let buffer' = I.create size' in
        I.blit state.buffer buffer' state.size;
        state.buffer <- buffer';
        state.size <- size'
      end;
    state.pos <- I.store state.buffer state.pos state.size block

  let flush state = ()

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
(* |                       For 32 bit architectures                        | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

module Impl32(B : BUFFER with type block = int) : IMPL
  with type block = int
  and type buffer = B.buffer
  = struct
    include B
    let block_size = 16
    let store str pos size block =
      B.set str pos (block lsr 8);
      B.set str (pos+1) block;
      pos+2
  end

(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |                        For 64 bit architectues                        | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

module Impl64(B : BUFFER with type block = int) : IMPL
  with type block = int
  and type buffer = B.buffer
= struct
  include B
  let block_size = 32
  let store str pos size block =
    B.set str (pos+0) (block lsr 24);
    B.set str (pos+1) (block lsr 16);
    B.set str (pos+2) (block lsr 8);
    B.set str (pos+3) block;
    pos+4
end

(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |                       ... and our string buffer                       | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

module StringB : BUFFER
  with type buffer = string
  and type block = int = struct
  type buffer = string
  and block = int
  let set str pos block = String.set str pos (Char.chr (block land 0xFF))
  let blit str str' size = String.blit str 0 str' 0 size
  let create = String.create
end

module String32 = Make(Impl32(StringB))
module String64 = Make(Impl64(StringB))
