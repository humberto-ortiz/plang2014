#lang pyret/check

# cfwae.arr - Template of a simple interpreter for CFWAE
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

# Bindings map identifiers to values
data Binding:
  | bind (name :: String, value :: CFWAE-Value)
end

# An Environment is a List<Binding>
mt-env = []
xtnd-env = link

# Data type for expressions
                          
data CFWAE:
  | numC (n :: Number)
  | binopC (op :: String, l :: CFWAE, r :: CFWAE)
  | withC (bindings :: List<Binding>, expr :: CFWAE)
  | idC (name :: String)
  | if0C (test-expr :: CFWAE, then-expr :: CFWAE, else-expr :: CFWAE)
  | fdC (args :: List<String>, body :: CFWAE)
  | appC (f :: CFWAE, args :: List<CFWAE>)
end

# and values
data CFWAE-Value:
  | numV (n :: Number)
  | closV (f :: CFWAE, e :: List<Binding>) # ExprC must be an fdC
end
              
fun parse(s) -> CFWAE:
  doc: "Parse reads an s-expression S and returns the abstract syntax tree."
  numC(0)
where:
  parse("3") is numC(3)
end

fun interp(e :: CFWAE) -> CFWAE-Value:
  doc: "Execute the expression E, return the result of evaluation."
  numV(0)
where:
  interp(if0C(numC(0), numC(1), numC(2))) is numV(2)
  interp(if0C(numC(1), numC(3), numC(2))) is numV(3)
end

check:
  fun run(s): interp(parse(read-sexpr(s))) end
  run("(if0 0 1 2)") is numV(2)
  run("(if0 1 2 3)") is numV(2)
end
