#
# This example shows how to write a basic calculator with variables.
#
# Comparison with Lark
#

using Lerchen


calc_grammar = """
    ?start: sum
          | NAME "=" sum    -> assign_var

    ?sum: product
        | sum "+" product   -> add
        | sum "-" product   -> sub

    ?product: atom
        | product "*" atom  -> mul
        | product "/" atom  -> div

    ?atom: NUMBER           -> number
         | "-" atom         -> neg
         | NAME             -> var
         | "(" sum ")"

    %import common.CNAME -> NAME
    %import common.NUMBER
    %import common.WS_INLINE

    %ignore WS_INLINE
"""


struct CalculateTree <: Transformer
    vars::Dict
end

CalculateTree() = CalculateTree(Dict())

@inline_rule assign_var(s::CalculateTree, name, value) = begin
    s.vars[name] = value
    return value
end

@inline_rule var(s::CalculateTree, name) =  s.vars[name]
@rule add(s::CalculateTree, nums) = nums[1]+nums[2]
@rule sub(s::CalculateTree, nums) = nums[1]-nums[2]
@rule mul(s::CalculateTree, nums) = nums[1]*nums[2]
@rule div(s::CalculateTree, nums) = nums[1]/nums[2]
@rule number(s::CalculateTree, nums) = Base.parse(Float64,nums[1])
@rule neg(s::CalculateTree, nums) = -1*nums[1]

calc_parser = Lark(calc_grammar, parser="lalr", transformer=CalculateTree())
calc(a::String) = Lerchen.parse(calc_parser,a)


test() = begin
    println(calc("a = 1+2"))
    println(calc("1+a*-3"))
end

main() = begin
    while true
        print("> ")
        s = ""
        s = readline(stdin)
        if s == "" break end
        println(calc(s))
    end
end

main()
#test()
