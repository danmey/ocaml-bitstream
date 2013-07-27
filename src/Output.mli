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

module Imperative (I : Sig.OTARGET_IMP
                   with type block = int)
  : Sig.O_IMP with type target = I.target


(* module Exceptionless : sig *)
(*   module Imperative (T : Sig.OTARGET_IMP) : Sig.O_IMP *)
(* end *)
