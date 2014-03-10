#lang pyret/check

# cfwae.arr - Template of a simple interpreter for CFWAE
# Copyright (C) 2014 - Humberto Ortiz-Zuazaga <humberto.ortiz@upr.edu>

#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published byhttp://fasterdata.es.net/science-dmz/
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

# Bindings map identifiers to expressions
data Binding:
  | bind (name :: String, expr :: CFWAE)
end

# An Environment is a List<Env>
data Env:
  | env (name :: String, value :: CFWAE-Value)
end

mt-env = []
xtnd-env = link

# Data type for expressions
                          
data CFWAE:
  | numC (n :: Number)
  | binopC (op :: String, l :: CFWAE, r :: CFWAE)
  | withC (bindings :: List<Binding>, expr :: CFWAE)
  | idC (name :: String)
  | if0C (test-expr :: CFWAE, then-expr :: CFWAE, else-expr :: CFWAE)
  | fdC (formal-args :: List<String>, body :: CFWAE)
  | appC (f :: CFWAE, actual-args :: List<CFWAE>)
end

# and values
data CFWAE-Value:
  | numV (n :: Number)
  | closV (f :: CFWAE, e :: List<Env>) # CFWAE must be an fd
end

keywords = ['+', '-', '*', '/', 'with', 'if0', 'fun']

fun parse(s) -> CFWAE:
  doc: "Parse reads an s-expression S and returns the abstract syntax tree."
  if Number(s):
    numC(s)
  else if String(s):
    idC(s)
  else if List(s):
    cases (List) s:
      | empty => raise("parse: unexpected empty list")
      | link(op, args) =>
        if (op == "+") or  (op == "*") or  (op == "-") or  (op == "/"):
          if args.length() == 2:
            argL = list.index(args, 0)
            argR = list.index(args, 1)
            
            binopC(op, parse(argL), parse(argR))
          else:
            raise("parse: arity mismatch")
          end
        else if op == "with":
          if args.length() == 2:
            bindings = list.index(args, 0)
            if bindings.length() > 0:
              withC(parse-bindings(bindings, keywords), parse(list.index(args, 1)))
            else:
              raise("parse: list of bindings can't be empty")
            end
          else:
            raise("parse: malformed with expression")
          end
        else if op == "fun":
          fdC(parse-formals(list.index(args, 0), keywords), parse(list.index(args, 1)))
        else if op == "if0":
          if args.length() == 3:
            argIf =  list.index(args, 0)
            argThen =  list.index(args, 1)
            argElse =  list.index(args, 2)

            if0C(parse(argIf), parse(argThen), parse(argElse))
          else:
            raise("parse: expected 3 arguments to if")
          end
        else:
          appC(parse(op), map(parse, args))
        end

    end
  else:
    raise("parse: not number, string or list")
  end
where:
  parse(read-sexpr("3")) is numC(3)
end

fun parse-formals(s, illegals) -> List<String>:
  doc: "Read a list of identifiers S and construct a list of Strings"
  if List(s):
    cases (List) s:
      | empty => empty
      | link(first, rest) =>
        if illegals.member(first):
          raise("parse-formals: formal arguments must be named uniquely")
        else:
          link(first, parse-formals(rest, link(first, illegals)))
        end
    end
  else:
    raise("parse-formals: illegal formal arguments")
  end
where:
  parse-formals(["x", "y"], keywords) is ["x", "y"]
end

fun parse-bindings(s, illegals):
  doc: "Read a list of pairs of sexprs in S and construct a list of Binding"
  if List(s):
    cases (List) s:
      | empty => empty
      | link(pair, rest) =>
        name = list.index(pair, 0)
        expr = list.index(pair, 1)
        # print([name, illegals])
        if true == illegals.member(name):
          raise("parse: illegal identifier " + name)
        else:
          link(bind(name, parse(expr)), parse-bindings(rest, link(name, illegals)))
        end
    end
  else:
    raise("parse-bindings: illegal bindings")
  end
where:
  parse-bindings([["x", 1]], keywords) is [bind("x", numC(1))]
  parse-bindings([["+", 1]], keywords) raises "illegal"
  parse-bindings([["x", 1], ["x", 2]], keywords) raises "illegal"
end

fun lookup(name :: String, nv :: List<Env>) -> CFWAE-Value:
  cases (List<Env>) nv:
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
  lookup("foo", [env("foo", numV(2))]) is numV(2)
end

fun interp-helper(e :: CFWAE, nv :: List<Env>) -> CFWAE-Value:
  fun plus-v(v1, v2): numV(v1.n + v2.n) end
  fun mult-v(v1, v2): numV(v1.n * v2.n) end
  fun sub-v(v1, v2): numV(v1.n - v2.n) end
  fun div-v(v1, v2):
    if v2.n == 0:
      raise("divide by zero")
    else:
      numV(v1.n / v2.n)
    end
  end
  binop-table = [['+', plus-v], ['-', sub-v], ['*', mult-v], ['/', div-v]]
  fun dispatch(op, table):
    cases (List) table:
      | empty => raise("dispatch: unknown operator" + op)
      | link(f, r) =>
        if list.index(f, 0) == op:
           list.index(f, 1)
        else:
           dispatch(op, r)
        end
    end
  end
  cases (CFWAE) e:
    | numC(n) => numV(n)
    | binopC(op, l, r) =>
      f = dispatch(op, binop-table)
      f(interp-helper(l,nv), interp-helper(r,nv))
    | idC(s) => lookup(s, nv)
    | withC(bs, wb) =>
      new-env = for map(binding from bs):
        name = binding.name
        val = interp-helper(binding.expr, nv)
        env(name , val)
      end
      interp-helper(wb, new-env.append(nv))
    | if0C(i, th, els) =>
      val = interp-helper(i, nv)
      if val.n == 0:
        interp-helper(th, nv)
      else:
        interp-helper(els, nv)
      end
    | fdC(_, _) => closV(e, nv)
    | appC(f, actuals) =>
      clos = interp-helper(f, nv)
      formals = clos.f.formal-args
      if formals.length() <> actuals.length():
        raise("arity mismatch")
      else:
        vals = for map(actual from actuals):
          interp-helper(actual, nv)
        end
        args = map2(env, formals, vals)
        interp-helper(clos.f.body, args.append(clos.e))
      end
  end
where:
  interp-helper(withC([bind("x", numC(5))], binopC("+", idC("x"), idC("x"))), mt-env) is numV(10)
end

fun interp(e :: CFWAE) -> CFWAE-Value:
  interp-helper(e, mt-env)
where:
  interp(if0C(numC(0), numC(1), numC(2))) is numV(1)
  interp(if0C(numC(1), numC(3), numC(2))) is numV(2)
end

check:
  fun run(s): interp(parse(read-sexpr(s))) end
  run("(if0 0 1 2)") is numV(1)
  run("(if0 1 2 3)") is numV(3)

  run("((fun (x) (+ x x)) 2)") is numV(4)
  # example from book
  run("(+ 2 ((fun (x) (* x 3)) 4))") is numV(14)
  
  run("(with ((foo 5)) (* foo foo))") is numV(25)
  run("(with ((a 1)) (with ((b a)) (with ((c (+ a b))) (+ b c))))") is numV(3)
  run("(with ((a 1)) (with ((a 2)) (with ((c (+ a a))) (+ a c))))") is numV(6)
  run("(with ((b 1)) (with ((a 2)) (with ((c 3)) (+ a b))))") is numV(3)
  
  # check dynamic scope
  run("(with ((f1 (fun (x) (f2 4)))) (with ((f2 (fun (y) (+ x y)))) (f1 3)))") raises "unbound"

  # nested funcitons
  interp(appC(appC(fdC(["x"], fdC(["y"], binopC("+", idC("x"), idC("y")))), [numC(4)]), [numC(1)])) is numV(5)

  # from assignment http://cs.brown.edu/courses/csci1730/2012/Assignments/Interpreter/
  run("(with ((x 2)
       (y 3))
  (with ((z (+ x y)))
    (+ x z)))") is numV(7)

  run("(with ((x 10) (x 20)) 50)") raises "parse"
  run("(fun (x x) 10)") raises "formals"

  # Crazy test from last year
  run("((fun (self n) (self self n)) (fun (self n) (if0 n 0 (+ n (self self (- n 1))))) 10)") is numV(55)
  run("((fun (self n) (self self n)) (fun (self n) (if0 n 1 (* n (self self (- n 1))))) 5)") is numV(120)
  run("((fun () 5))") is numV(5)
  run("(with () 5)") raises "bindings"
  run("(with ((x 1)) )") raises "malformed"
end
