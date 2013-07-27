(*----------------------------------------------------------------------------
  Singatures
  Copyright (C) 2012 Wojciech Meyer

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU Lesser General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
  --------------------------------------------------------------------------*)

(** Module [Sig]: Signatures used in the whole library. *)

(** {1 Endianess} *)

type endianess =  [ `Big | `Little | `System ]
(** [`System] indicates endianess detected at compile time *)

(** {2 Common types } *)

module type TYPES = sig
  type word
  (** Word could be 16, 32 or 64 bits depending on architecture.
      [word] datatype wraps up the platform dependency  *)
  and dword
  (** [dword] same purpose as [word] but for double words *)
  and qword
  (** [qword] defines qwords *)
end

(** [Basis] defines some basic data types to be included in the
    the target implementation *)
module type BASIS = sig
  type t
  (** [t] defines the current state of the target *)
  and block
  (** [block] is a maximal block of bits passes to the target *)
  and target
  (** [target] is type of value passed to [create] *)
  val block_size : int
  (** [block_size] is the block size *)
end

(** {3 Target signatures} *)

(** Minimal signature needed to implement output imperative target *)
module type OTARGET_IMP = sig
  include BASIS
  val create : int -> t
  (** [create ()] creates target with initial state [t] *)
  val put : t -> block -> unit
  (** [put t b] puts a block in the stream *)
  val contents : t -> target
  (** [contents t] gets the raw buffer *)
  val concat : int -> block -> block -> block
  val upper : int -> block -> block
  val flush : t -> int -> block -> unit
  val on : block
  val off : block
  val block_size : int
end

module type ITARGET_IMP = sig
  include BASIS
  val create : unit -> t
  val fetch : t -> unit -> block
end

module type OTARGET_FUN = sig
  include BASIS
  val create : target -> t
  val put : block -> t -> t
end

module type ITARGET_FUN = sig
  include BASIS
  val create : unit -> t
  val fetch : t -> block * t
end

(** {4 Output imperative signature} *)
module type O_IMP =
sig
  include TYPES
  type t
  (** [t] stream of type t *)

  and target
  (** buffered with type [target] *)

  val endianess : endianess
  (** [endianess] controls target endianess *)

  val create : int -> t
  (** [create n] creates stream of initial size [n] *)

  val contents : t -> target
  (** [contents s] contents of the stream [s]  *)

  val flush : t -> unit

  val bit : t -> bool -> unit
  (** [bit s v] pushes single bit [v] to stream [s] *)

  val bits : t -> int -> int -> unit
  val many_bits : t -> int -> int64 -> unit
  val nibble : t -> int -> unit
  val byte : t -> int -> unit
  val word : t -> word -> unit
  val dword : t -> dword -> unit
  val qword : t -> qword -> unit

  val bool : t -> bool -> unit
  val int32 : t -> int32 -> unit
  val int64 : t -> int32 -> unit
  val int : t -> int -> unit
end

module type O_IMP_EXC =
sig
  include O_IMP
  exception Empty_stream
end
