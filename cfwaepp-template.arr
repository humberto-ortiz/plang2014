#lang pyret/check

# cfwaepp-template.arr - Template of a interpreter for CFWAE++
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

# Data type for expressions
data FieldP:
  | fieldP (name :: String, value :: ExprP)
end

data ExprP:
  | ObjectP (fields :: List<FieldP>)

  | FuncP (args :: List<String>, body :: ExprP)
  | AppP (func :: ExprP, args :: List<ExprP>)
  | DefvarP (id :: String, bind :: ExprP, body :: ExprP)
  | DeffunP (name :: String, ids :: List<String>, funbody :: ExprP, body :: ExprP)
  | IdP (name :: String)

  | GetfieldP (o :: ExprP, field :: ExprP)
  | SetfieldP (o :: ExprP, field :: ExprP, newval :: ExprP)
  | SetvarP (id :: String, val :: ExprP)

  | WhileP (test :: ExprP, body :: ExprP)
  | ForP (init :: ExprP, test :: ExprP, update :: ExprP, body :: ExprP)

  | SeqP (es :: List<ExprP>)
  | IfP (cond :: ExprP, thn :: ExprP, els :: ExprP)

  | NumP (n :: Number)
  | StrP (s :: String)
  | TrueP
  | FalseP

# An op is one of '+ '- '== 'print '< '>
  | PrimP (op :: String, args :: List<ExprP>)
end                          

data FieldC:
  | fieldC (name :: String, value :: ExprC)
end

data ExprC:

  | ObjectC (fields :: List<FieldC>)
  | GetFieldC (obj :: ExprC, field :: ExprC)
  | SetFieldC (obj :: ExprC, field :: ExprC, value :: ExprC)

  | FuncC (args :: List<String>, body :: ExprC)
  | AppC (func :: ExprC, args :: List<ExprC>)
  | LetC (id :: String, bind :: ExprC, body :: ExprC)
  | IdC (id :: String)
  | SetC (id :: String, value :: ExprC)

  | IfC (cond :: ExprC, thn :: ExprC, els :: ExprC)
  | SeqC (e1 :: ExprC, e2 :: ExprC)

  | NumC (n :: Number)
  | StrC (s :: String)
  | TrueC
  | FalseC

  | ErrorC (expr :: ExprC)

# The core operations are 'String+ 'num+ 'num- '== '< '> 'print 'tagof
  | Prim1C (op :: String, arg :: ExprC)
  | Prim2C (op :: String, arg1 :: ExprC, arg2 :: ExprC)
end

# Environments and Values
data Binding:
  | bind (name :: String, value :: Number)
end

# Environments are lists of bindings
mt-env = []
xtnd-env = link

# Helper function to generate stateful counters for store
fun mk-counter():
  var ctr = 0
  fun():
    ctr := ctr + 1
    ctr
  end
end

new-loc = mk-counter()

data FieldV:
  | fieldV (name :: String, value :: ValueC)
end

data ValueC:
  | TrueV
  | FalseV
  | NumV (n :: Number)
  | StrV (s :: String)
  | ClosureV (args :: List<String>, body :: ExprC, env :: List<Binding>)
  | ObjectV (fields :: List<FieldV>)
end

fun pretty-value(v :: ValueC) -> String:
  cases (ValueC) v:
    | ObjectV(_) => "object"
    | ClosureV(_, _, _) => "function"
    | NumV(n) => torepr(n)
    | StrV(s) => s
    | TrueV => "true"
    | FalseV => "false"
  end
end

# helper function for errors
interp-error = raise

# The store maps locations to values
data Cell:
  | cell (location :: Number, value :: ValueC)
end

# The store is a list of cells
mt-sto = []
xtnd-sto = link

data Result:
  | v-x-s (value :: ValueC, store :: List<Cell>)
end

binops = ["+", "-", "==", "<", ">"]
keywords = ['if', 'fun', 'true', 'false', 'defvar', 'deffun', 'obj', 'getfield', 'setfield', 'setvar', 'begin', 'while', 'print', 'for']


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

fun parse-fields(lof) -> List<FieldP>:
  for map(field from lof):
    name = field.first
    val = parse(list.index(field, 1))
    fieldP(name, val)
  end
end

fun parse(s) -> ExprP:
  doc: "Parse reads an s-expression S and returns the abstract syntax tree."
  if Number(s):
    # num
    NumP(s)
  else if String(s):
    # true
    if s == "true":
      TrueP
    # false
    else if s == "false":
      FalseP
    # id
    else:
      IdP(s)
    end
  else if List(s):
    cases (List) s:
      | empty => raise("parse: empty sexpr")
      | link(op, r) =>
        len = r.length()
        # while
        if "while" == op:
          if len == 2:
            t = parse(list.index(r, 0))
            b = parse(list.index(r, 1))
            WhileP(t, b)
          else:
            raise("parse: malformed while" + s)
          end
        # string
        else if "string" == op:
          if len == 1:
            StrP(r.first)
          else:
            raise("parse: malformed string" + r)
          end
        # + - == < >
        else if binops.member(op):
          if len == 2:
            PrimP(op, [parse(list.index(r, 0)), parse(list.index(r, 1))])
          else:
            raise("parse: binary operations require two arguments")
          end
        # print
        else if "print" == op:
          if len == 1:
            PrimP(op, [parse(r.first)])
          else:
            raise("parse: print requires a single argument")
          end
        # if
        else if "if" == op:
          if len == 3:
            cond = parse(list.index(r, 0))
            then = parse(list.index(r, 1))
            esle = parse(list.index(r, 2))
            IfP(cond, then, esle)
          else:
            raise("parse: malformed if expression")
          end
        # begin
        else if "begin" == op:
          es = for map(e from r):
            parse(e)
          end
          SeqP(es)
        # defvar
        else if "defvar" == op:
          if len == 3:
            id = list.index(r, 0)
            val = parse(list.index(r, 1))
            bod = parse(list.index(r, 2))
            DefvarP(id, val, bod)
          else:
            raise("parse: malformed defvar")
          end
        # setvar
        else if "setvar" == op:
          if len == 2:
            id = list.index(r, 0)
            val = parse(list.index(r, 1))
            SetvarP(id, val)
          else:
            raise("parse: malformed setvar")
          end
        # for
        else if "for" == op:
          if len == 4:
            args = for map(arg from r):
              parse(arg)
            end
            init = list.index(args, 0)
            test = list.index(args, 1)
            update = list.index(args,2)
            body = list.index(args, 3)
            ForP(init, test, update, body)
          else:
            raise("parse: malformed for " + torepr(s))
          end
          # fun
        else if "fun" == op:
          if len == 2:
            formals = parse-formals(list.index(r, 0), keywords)
            body = parse(list.index(r, 1))
            FuncP(formals, body)
          else:
            raise("parse: malformed function definition")
          end
          # obj
        else if "obj" == op:
          if len == 1:
            ObjectP(parse-fields(r.first))
          else:
            raise("parse: malformed object" + torepr(s))
          end
          # getfield
        else if "getfield" == op:
          if len == 2:
            obj = parse(list.index(r, 0))
            field = parse(list.index(r, 1))
            GetfieldP(obj, field)
          else:
            raise("parse: malformed getfield" + torepr(s))
          end
          # setfield
        else if "setfield" == op:
          if len == 3:
            obj = parse(list.index(r, 0))
            field = parse(list.index(r, 1))
            val = parse(list.index(r, 2))
            SetfieldP(obj, field, val)
          else:
            raise("parse: malformed setfield" + torepr(s))
          end
          # deffun
        else if "deffun" == op:
          if len == 3:
            ids = list.index(r, 0)
            funbody = parse(list.index(r, 1))
            body = parse(list.index(r, 2))
            DeffunP(ids.first, ids.rest, funbody, body)
          else:
            raise("parse: malformed fun " + torepr(s))
          end
        else:
          # app
          AppP(parse(s.first), map(parse, s.rest))
        end
    end
  else:
    raise("parse: unknown expression " + torepr(s))
  end
where:
  fun p(s): parse(read-sexpr(s)) end 
  p("3") is NumP(3)
  p("(while true 5)") is WhileP(TrueP, NumP(5))
  p("(for (setvar x 0) (< x 10) (setvar x (+ x 1)) 
    (print x))") is ForP(SetvarP("x", NumP(0)), PrimP("<", [IdP("x"), NumP(10)]), SetvarP("x", PrimP("+", [IdP("x"), NumP(1)])), PrimP("print", [IdP("x")]))
  # NOTE: strings must be enclosed in double quotes
  # You can put them inside single quotes, or escape them
  p("\"hello\"") is StrP("hello")
  p('"hello"') is StrP("hello")
  p("(print (+ 2 3))") is PrimP("print", [PrimP("+", [NumP(2), NumP(3)])])
  p("(if true 1 2)") is IfP(TrueP, NumP(1), NumP(2))
  p("(begin 1 2 3)") is SeqP([NumP(1), NumP(2), NumP(3)])
  p("(defvar x 1 x)") is DefvarP('x', NumP(1), IdP('x'))
  p("(setvar x 2)") is SetvarP('x', NumP(2))
  p("(for 0 true 1 2)") is ForP(NumP(0), TrueP, NumP(1), NumP(2))
  p("(fun (x) x)") is FuncP(['x'], IdP("x"))
  p("(obj ((x 1) (f (fun (x) x))))") is ObjectP([fieldP("x", NumP(1)), fieldP("f", FuncP(['x'], IdP("x")))])
  p('(getfield o "x")') is GetfieldP(IdP('o'), StrP('x'))
  p('(setfield o "x" 2)') is SetfieldP(IdP('o'), StrP('x'), NumP(2))
  p("(deffun (id x) x 3)") is DeffunP("id", ["x"], IdP('x'), NumP(3)) 
  p("(deffun (id x) x (id 3))") is DeffunP("id", ["x"], IdP('x'), AppP(IdP("id"), [NumP(3)])) 
end

# Humberto's desugar
fun desugar-fields(fields :: List<FieldP>) -> List<FieldC>:
  for map(field from fields):
    name = field.name
    val = desugar(field.value)
    fieldC(name, val)
  end
end

fun desugar-begin(es :: List<ExprP>) -> ExprC:
  len = es.length()
  if len == 0:
    ErrorC(StrC("empty sequence of expressions"))
  else if len == 1:
    desugar(es.first)
  else if len == 2:
    SeqC(desugar(list.index(es, 0)), desugar(list.index(es, 1)))
  else:
    SeqC(desugar(es.first), desugar-begin(es.rest))
  end
end

fun desugar(e :: ExprP) -> ExprC:
  doc: "Desugar the expression E, return the equivalent in the core language."
  cases (ExprP) e:
    | TrueP => TrueC
    | FalseP => FalseC
    | NumP(n) => NumC(n)
    | StrP(s) => StrC(s)
    | IdP(name) => IdC(name)
    | IfP(tst, thn, els) => IfC(desugar(tst), desugar(thn), desugar(els))
    | FuncP(formals, body) => FuncC(formals, desugar(body))
    | AppP(func, actuals) => AppC(desugar(func), map(desugar, actuals))
    | ObjectP(fields) => ObjectC(desugar-fields(fields))
    | GetfieldP(o, f) => GetFieldC(desugar(o), desugar(f))
    | SetfieldP(o, f, v) => SetFieldC(desugar(o), desugar(f), desugar(v))
    | DefvarP(id, val, body) => LetC(id, desugar(val), desugar(body))
    | SetvarP(id, val) => SetC(id, desugar(val))
    | DeffunP(n, ids, fb, b) =>
      dummy-fun = FuncC([], ErrorC(StrC("Dummy function")))
      LetC(n, dummy-fun,
        SeqC(SetC(n, FuncC(ids, desugar(fb))),
          desugar(b)))
    | SeqP(es) => desugar-begin(es)
    | WhileP(test, body) =>
      dummy-fun = FuncC([], ErrorC(StrC("Dummy function")))
      IfC( desugar(test),
       # while-var will hold the actual function once we tie
       # everything together
       LetC( "while-var", dummy-fun,
         LetC( "while-func", 
           # this function does the real work - it runs the body of
           # the while loop, then re-runs it if the test is true, and
           # stops if its false
            FuncC([],
              LetC("temp-var",
                desugar( body),
               IfC(desugar( test),
                  AppC(IdC( "while-var"), []),
                  IdC( "temp-var")))),
           # The SetC here makes sure that 'while-var will resolve
           # to the right value later, and the AppC kicks things off
           SeqC(SetC( "while-var", IdC( "while-func")),
              AppC(IdC("while-var"), [])))),
       FalseC)
    | ForP(init, test, update, body) =>
      dummy-fun = FuncC([], ErrorC(StrC("Dummy function")))
      LetC("for-init", desugar(init),
        IfC(desugar(test),
          # set up recursion
          LetC("for-var", dummy-fun,
            LetC("for-fun",
              FuncC([], 
                LetC("for-body", desugar(body),
                  LetC("for-update", desugar(update),
                    LetC("for-test", desugar(test),
                      IfC(IdC("for-test"),
                        AppC(IdC("for-var"), []),
                        IdC("for-body")))))),
              SeqC(
                SetC("for-var", IdC("for-fun")),
                AppC(IdC("for-var"), [])))),
          IdC("for-init")))
    | PrimP(op, args) =>
      len = args.length()
      if 1 == len:
        Prim1C('print', desugar(args.first))
      else if 2 == len:
        argL = desugar(list.index(args, 0))
        argR = desugar(list.index(args, 1))
        if op == '+':
        LetC("tag-l", Prim1C('tagof', argL),
          LetC("tag-r", Prim1C('tagof', argR),
            IfC(Prim2C("==", IdC("tag-l"), IdC("tag-r")),
              IfC(Prim2C("==", IdC("tag-l"), StrC("string")),
                  Prim2C("string+", argL, argR),
                  Prim2C("num+", argL, argR)),
                ErrorC(StrC("Bad arguments to +")))))
        else if op == '-':
          Prim2C("num-", argL, argR)
        else:
          Prim2C(op, argL, argR)
        end
      else:
        ErrorC(StrC("Bad primop"))
      end
  end
where:
  fun run(s): desugar(parse(read-sexpr(s))) end
  run("true") is TrueC
  run("false") is FalseC
  run("0") is NumC(0)
  run('"Hello, World!"') is StrC("Hello, World!")
  run("x") is IdC("x")
  run("(fun (x) x)") is FuncC(["x"], IdC("x"))
  run("((fun (x) x) 5)") is AppC(FuncC(["x"], IdC("x")), [NumC(5)])
  run("(if true 1 0)") is IfC(TrueC, NumC(1), NumC(0))
  run("(obj ((x 5)))") is ObjectC([fieldC("x", NumC(5))])
  run('(getfield (obj ((x 5))) "x")') is GetFieldC(ObjectC([fieldC("x", NumC(5))]), StrC("x"))
  run("(setvar x 2)") is SetC("x", NumC(2))
  run("(deffun (id x) x (id 3))") is
  LetC("id",  FuncC([], ErrorC(StrC("Dummy function"))),
    SeqC(SetC("id", FuncC(["x"], IdC("x"))),
      AppC(IdC("id"), [NumC(3)]) ))
  run("(begin 1 2)") is SeqC(NumC(1), NumC(2))
  run("(begin 1 2 3)") is SeqC(NumC(1), SeqC(NumC(2), NumC(3)))
  run("(while true 1)") is IfC(TrueC, LetC("while-var", FuncC([], ErrorC(StrC("Dummy function"))), LetC("while-func", FuncC([], LetC("temp-var", NumC(1), IfC(TrueC, AppC(IdC("while-var"), []), IdC("temp-var")))), SeqC(SetC("while-var", IdC("while-func")), AppC(IdC("while-var"), [])))), FalseC)
  run("(defvar x 0 (for (setvar x 0) (< x 10) (setvar x (+ x 1)) 
  (print x)))") is LetC("x", NumC(0), LetC("for-init", SetC("x", NumC(0)), IfC(Prim2C("<", IdC("x"), NumC(10)), LetC("for-var", FuncC([], ErrorC(StrC("Dummy function"))), LetC("for-fun", FuncC([], LetC("for-body", Prim1C("print", IdC("x")), LetC("for-update", SetC("x", LetC("tag-l", Prim1C("tagof", IdC("x")), LetC("tag-r", Prim1C("tagof", NumC(1)), IfC(Prim2C("==", IdC("tag-l"), IdC("tag-r")), IfC(Prim2C("==", IdC("tag-l"), StrC("string")), Prim2C("string+", IdC("x"), NumC(1)), Prim2C("num+", IdC("x"), NumC(1))), ErrorC(StrC("Bad arguments to +")))))), LetC("for-test", Prim2C("<", IdC("x"), NumC(10)), IfC(IdC("for-test"), AppC(IdC("for-var"), []), IdC("for-body")))))), SeqC(SetC("for-var", IdC("for-fun")), AppC(IdC("for-var"), [])))), IdC("for-init"))))
  run("(for a b c d)") is LetC("for-init", IdC("a"), IfC(IdC("b"), LetC("for-var", FuncC([], ErrorC(StrC("Dummy function"))), LetC("for-fun", FuncC([], LetC("for-body", IdC("d"), LetC("for-update", IdC("c"), LetC("for-test", IdC("b"), IfC(IdC("for-test"), AppC(IdC("for-var"), []), IdC("for-body")))))), SeqC(SetC("for-var", IdC("for-fun")), AppC(IdC("for-var"), [])))), IdC("for-init")))
  # The primops
  desugar(PrimP("<", [IdP("x"), NumP(10)])) is Prim2C("<", IdC("x"), NumC(10))
end


# Templates for interpreter: fix interp-full

fun interp-full (expr :: ExprC, env :: List<Binding>, store :: List<Cell>) -> Result:
  cases (ExprC) expr:
    | NumC (n) => v-x-s(NumV(n), store)
    | else => interp-error("Haven't covered a case yet:".append(torepr(expr)))
  end
end

fun interp(expr :: ExprC) -> ValueC:
  cases (Result) interp-full(expr, mt-env, mt-sto):
    | v-x-s (value, store) => value
  end
end

check:
  interp(NumC(5)) is NumV(5)
end
