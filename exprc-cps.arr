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

data ExprC:
  | numC (n :: Number)
  | plusC (l :: ExprC, r :: ExprC)
  | multC (l :: ExprC, r :: ExprC)
  | idC (s :: String)
  | appC (f :: ExprC, a :: ExprC)
  | app2C(f :: ExprC, a :: ExprC, k :: ExprC)
  | fdC (arg :: String, body :: ExprC)
  | fd2C (arg :: String, cont :: String, body :: ExprC)
end

data Binding:
  | bind (name :: String, value :: Value)
end
 
# An Environment is a List<Binding>
mt-env = []
xtnd-env = link

data Value:
  | numV (n :: Number)
  | closV (f :: ExprC, e :: List<Binding>) # ExprC must be an fdC
end

#predefined_functions = [fdC("double", "x", plusC(idC("x"), idC("x"))),
#       fdC("quadruple", "x", appC("double", appC("double", idC("x")))),
#       fdC("const5", "_", numC(5)),
#       fdC("foo", "x", multC(idC("x"), idC("x")))]

fun interp(e :: ExprC, nv :: List<Binding>) -> Value:
  fun plus-v(v1, v2): numV(v1.n + v2.n) end
  fun mult-v(v1, v2): numV(v1.n * v2.n) end
  cases (ExprC) e:
    | numC(n) => numV(n)
    | plusC(l, r) => plus-v(interp(l, nv), interp(r, nv))
    | multC(l, r) => mult-v(interp(l, nv), interp(r, nv))
    | if0C(i, th, els) =>
      ival = interp(i, nv)
      if ival.n <> 0:
        interp(th, nv)
      else:
        interp(els, nv)
      end
    | idC(s) => lookup(s, nv)
    | fdC(_, _) => closV(e, nv)  
    | appC(f, a) => 
      clos = interp(f, nv)
      arg-val = interp(a, nv)
      interp(clos.f.body,
        xtnd-env(bind(clos.f.arg, arg-val), clos.e))
    | app2C(f, a, k) =>
      clos = interp(f, nv)
      arg-val = interp(a, nv)
      cont = interp(k, nv)
      k-clos = interp(k, nv)
      k-arg =interp(clos.f.body,
        xtnd-env(bind(clos.f.arg, arg-val), clos.e))
      interp(k-clos,
        xtnd-env(bind(k-clos.f.arg, k-arg), k-clos.e))
    | whereC(a, v, b) =>
      arg-val = interp(v, nv)
      interp(b,
        xtnd-env(bind(a, arg-val), nv))
  end
where:
  interp(plusC(numC(3), numC(5)),mt-env) is numV(8)
  interp(appC(fdC("x", plusC(idC("x"), idC("x"))), numC(5)), mt-env) is numV(10)
end

fun lookup(name :: String, nv :: List<Binding>) -> Value:
  cases (List<Binding>) nv:
    | empty => raise("unbound identifier")
    | link(first, rest) =>
      if first.name == name: 
        first.value
      else:
        lookup(name, rest)
      end
  end
where:
  lookup("foo", []) raises "unbound"
  lookup("foo", [bind("foo", numV(2))]) is numV(2)
end

fun cps(e :: ExprC) -> ExprC:
  cases (ExprC) e:
    | numC(_) => fdC("k", appC(idC("k"), e))
    | idC(_) => fdC("k", appC(idC("k"), e))
    | plusC(l, r) =>
      fdC("k",
        appC(cps(l),
          fdC("l-v",
            appC(cps(r),
              fdC("r-v",
                appC(idC("k"), plusC(idC("l-v"), idC("r-v"))))))))
    | appC(f, a) =>
      fdC("k",
        appC(cps(f),
          fdC("f-v",
            appC(cps(a),
              fdC("a-v",
                app2C(idC("f-v"), idC("a-v"), idC("k")))))))
    | fdC(v, b) =>
      fdC("k",
        appC(idC("k"),
          fd2C(v, "dyn-k",
            appC(cps(b), idC("dyn-k")))))
  end
end

# parse takes s-expressions and converts them to expressions 
              
fun parse(s) -> ExprC:
  if Number(s):
    numC(s)
  else if String(s):
    idC(s)
  else if List(s):
    cases (List) s:
      | empty => raise("parse: unexpected empty list")
      | link(op, args) =>
        if args.length() == 1:
          appC(parse(op), parse(list.index(args, 0)))
        else if args.length() == 2:
          argL = list.index(args, 0)
          argR = list.index(args, 1)
          if op == "+":
            plusC(parse(argL), parse(argR))
          else if op == "*":
            multC(parse(argL), parse(argR))
          else if op == "fun":
            fdC(argL, parse(argR))
          else:
            raise("parse error")
          end
        else:
          raise("parse: can't parse " + s)
        end
        
    end
  else:
    raise("parse: not number, string or list")
  end
where:
  fun p(s): parse(read-sexpr(s)) end
  p("3") is numC(3)
  p("(+ 1 2)") is plusC(numC(1), numC(2))
  p("(fun x (+ x x))") is fdC("x", plusC(idC("x"), idC("x")))
  p("((fun x (+ x x)) 2)") is appC(fdC("x", plusC(idC("x"), idC("x"))), numC(2))
end


fun icps(e):
  id-cps = fdC("v", idC("v"))
  interp(appC(cps(e), id-cps), mt-env)
end

check:
  icps(plusC(numC(3), numC(5))) is numV(8)
  cps(plusC(numC(3), numC(5))) is fdC("k",
    appC(fdC("k", appC(idC("k"), numC(3))), fdC("l-v",
        appC(fdC("k", appC(idC("k"), numC(5))), fdC("r-v",
            appC(idC("k"),
              plusC(idC("l-v"), idC("r-v"))))))))
end
