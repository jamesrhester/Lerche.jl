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
    name::String
    is_term::Bool
    filter_out::Bool
end

Terminal(name;filter_out=false) = Terminal(name,true,filter_out)

Base.show(io::IO,ls::Terminal) = print(io,"Terminal($(ls.name))")

struct NonTerminal <: LarkSymbol
    name::String
    is_term::Bool
end

Base.show(io::IO,ls::NonTerminal) = print(io,"NonTerminal($(ls.name))")

NonTerminal(name) = NonTerminal(name,false)

struct RuleOptions
    keep_all_tokens::Bool
    expand1::Bool
    priority::Union{Int,Nothing}
end

RuleOptions(;keep_all_tokens=false,expand1=false,priority=nothing) = RuleOptions(keep_all_tokens,expand1,priority)

mutable struct Rule
    origin::NonTerminal
    expansion::Array{LarkSymbol}
    alias::Union{Nothing,String}
    options::Union{Nothing,RuleOptions}
end

Rule(origin,expansion;alias=nothing,options=nothing) = Rule(origin,expansion,alias,options)

Base.show(io::IO,r::Rule) = print(io,"<$(r.origin.name):  $(join(r.expansion," "))>")

# Equality, needed for indexing into rules

Base.:(==)(r1::Rule,r2::Rule) = begin
    return r1.origin == r2.origin && r1.expansion == r2.expansion &&
        r1.alias == r2.alias && r1.options == r2.options
end

Base.hash(r1::Rule) = hash(r1.origin) + hash(r1.expansion) + hash(r1.alias) + hash(r1.options)
