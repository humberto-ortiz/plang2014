#lang pyret/check

# exprc.arr - simple interpreter for arithmetic and numeric if expressions
# Copyright (C) 2014 - Humberto Ortiz-Zuazaga <humberto.ortiz@upr.edu>

#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.

# Data type for expressions
                          
data ExprC:
  | numC (n :: Number)
  | plusC (l :: ExprC, r :: ExprC)
  | multC (l :: ExprC, r :: ExprC)
  | if0C (test-expr :: ExprC, then-expr :: ExprC, else-expr :: ExprC)
end

# parse takes s-expressions and converts them to expressions 
              
fun parse(s) -> ExprC:
  if Number(s):
    numC(s)
  else if List(s):
    cases (List) s:
      | empty => raise("parse: unexpected empty list")
      | link(op, args) =>
        if args.length() == 2:
          argL = list.index(args, 0)
          argR = list.index(args, 1)
          if op == "+":
            plusC(parse(argL), parse(argR))
          else if op == "*":
            multC(parse(argL), parse(argR))
          end
        else if args.length() == 3:
          argIf =  list.index(args, 0)
          argThen =  list.index(args, 1)
          argElse =  list.index(args, 2)
          if op == "if":
            if0C(parse(argIf), parse(argThen), parse(argElse))
          else:
            raise("parse: unknown ternary operator")
          end
        end
    end
  else:
    raise("parse: not number or list")
  end
end

fun interp(e :: ExprC) -> Number:
  cases (ExprC) e:
    | numC(n) => n
    | plusC(l, r) => interp(l) + interp(r)
    | multC(l, r) => interp(l) * interp(r)
    | if0C(i, th, els) => 999999999 # not really, please fix me!
  end
where:
  interp(if0C(numC(0), numC(1), numC(2))) is 2
  interp(if0C(numC(1), numC(3), numC(2))) is 3
end

check:
  fun run(s): interp(parse(read-sexpr(s))) end
  run("(if 0 1 2)") is 2
  run("(if 1 2 3)") is 2
end
