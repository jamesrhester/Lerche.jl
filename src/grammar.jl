# This is a literal-ish translation. However
# we may be more idiomatic by e.g. having
# Symbol(true,false)

abstract type LarkSymbol end

LarkSymbol(name) = LarkSymbol(name,missing)

Base.:(==)(s1::LarkSymbol,s2::LarkSymbol) = begin
    typeof(s1) == typeof(s2) && s1.is_term == s2.is_term && s1.name == s2.name
end

Base.hash(s1::LarkSymbol) = begin
    hash(s1.is_term) + hash(s1.name)
end

Base.show(io::IO,ls::LarkSymbol) = print(io,"$(typeof(ls))($(ls.name))")

struct Terminal <: LarkSymbol
    name
    is_term
    filter_out
end

Terminal(name;filter_out=false) = Terminal(name,true,filter_out)

Base.show(io::IO,ls::Terminal) = print(io,"Terminal($(ls.name))")

struct NonTerminal <: LarkSymbol
    name
    is_term
end

Base.show(io::IO,ls::NonTerminal) = print(io,"NonTerminal($(ls.name))")

NonTerminal(name) = NonTerminal(name,false)

mutable struct Rule
    origin
    expansion
    alias
    options
end

Rule(origin,expansion;alias=nothing,options=nothing) = Rule(origin,expansion,alias,options)

Base.show(io::IO,r::Rule) = print(io,"<$(r.origin.name):  $(join(r.expansion," "))>")

struct RuleOptions
    keep_all_tokens
    expand1
    priority
end

RuleOptions(;keep_all_tokens=false,expand1=false,priority=nothing) = RuleOptions(keep_all_tokens,expand1,priority)

