# example dispatch table
# won't work, as it's missing all the datatypes

fun plus-v(v1, v2): numV(v1.n + v2.n) end
fun mult-v(v1, v2): numV(v1.n * v2.n) end

binop-table = [['+', plus-v], ['*', mult-v]]

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

fun interp-helper(e :: CFWAE, nv :: List<Env>) -> CFWAE-Value:
  cases (CFWAE) e:
    | numC(n) => numV(n)
    | binopC(op, l, r) =>
      f = dispatch(op, binop-table)
      f(interp-helper(l,nv), interp-helper(r,nv))
  end
end