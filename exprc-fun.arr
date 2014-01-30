#lang pyret/check

# exprc.arr - simple interpreter for arithmetic and functions expressions
# Copyright (C) 2014 - Humberto Ortiz-Zuazaga <humberto.ortiz@upr.edu>

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

data FunDefC:
  | fdC (name :: String, arg :: String, body :: ExprC)
end

data ExprC:
      | numC (n :: Number)
      | plusC (l :: ExprC, r :: ExprC)
      | multC (l :: ExprC, r :: ExprC)
      | idC (s :: String)
      | appC (f :: String, a :: ExprC)
end

predefined_functions = [fdC("double", "x", plusC(idC("x"), idC("x"))),
       fdC("quadruple", "x", appC("double", appC("double", idC("x")))),
       fdC("const5", "_", numC(5)),
       fdC("foo", "x", multC(idC("x"), idC("x")))]

fun interp(e :: ExprC, fds :: List<FunDefC>) -> Number:
  cases (ExprC) e:
      | numC(n) => n
      | plusC(l, r) => interp(l, fds) + interp(r, fds)
      | multC(l, r) => interp(l, fds) * interp(r, fds)
      | idC(s) => raise("boom!")
      | appC(f, a) => 
         func = get-fundef(f,fds)
         interp(subst(a, func.arg, func.body), fds)
  end
end

fun get-fundef(name :: String, fds :: List<FunDefC>)
        -> FunDefC:
  cases (List<FunDefC>) fds:
    | empty => raise("no such function")
    | link(first, rest) => if first.name == name: 
                             first
                           else:
                             get-fundef(name, rest)
                           end
  end
where:
  get-fundef("foo", []) raises "no such function"
  get-fundef("double", predefined_functions).name is "double"
end

fun subst(with :: ExprC, at :: String, in :: ExprC)
        -> ExprC:
  cases (ExprC) in:
      | numC(n) => in
      | plusC(l, r) => plusC(subst(with, at, l), subst(with, at, r))
      | multC(l, r) => multC(subst(with, at, l), subst(with, at, r))
      | appC(f, a) => appC(f, subst(with, at, a))
      | idC(s) =>
        if s == at:
          with
        else:
          in
        end
  end
end

check:
  interp(plusC(numC(3), numC(5)), predefined_functions) is 8
  interp(appC("double", numC(5)), predefined_functions) is 10
  interp(appC("foo", numC(5)), predefined_functions) is 25
end
