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


fun parse(s) -> ExprP:
  doc: "Parse reads an s-expression S and returns the abstract syntax tree."
  NumP(0)
where:
  parse(read-sexpr("3")) is NumP(3)
end

fun desugar(e :: ExprP) -> ExprC:
  doc: "Desugar the expression E, return the equivalent in the core language."
  cases (ExprP) e:
    | NumP(n) => NumC(n)
    | TrueP => TrueC
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
  end
where:
  desugar(NumP(0)) is NumC(0)
  desugar(WhileP(TrueP, NumP(1))) is IfC(TrueC, LetC("while-var", FuncC([], ErrorC(StrC("Dummy function"))), LetC("while-func", FuncC([], LetC("temp-var", NumC(1), IfC(TrueC, AppC(IdC("while-var"), []), IdC("temp-var")))), SeqC(SetC("while-var", IdC("while-func")), AppC(IdC("while-var"), [])))), FalseC)
end


