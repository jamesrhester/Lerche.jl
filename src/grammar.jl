# This is a literal-ish translation. However
# we may be more idiomatic by e.g. having
# Symbol(true,false)

abstract type LarkSymbol end

LarkSymbol(name) = LarkSymbol(name,missing)

Base.equals(s1::LarkSymbol,s2::LarkSymbol) = begin
    typeof(s1) == typeof(s2) && s1.is_term == s2.is_term && s1.name == s2.name
end

Base.show(io::IOStream,ls::LarkSymbol) = print(io,"$(typeof(ls))$(ls.name)")

struct Terminal <: LarkSymbol
    name
    is_term
    filter_out
end

Terminal(name) = Terminal(name,true,false)
Terminal(name,filter) = Terminal(name,true,filter)

struct NonTerminal <: LarkSymbol
    name
    is_term
end

NonTerminal(name) = NonTerminal(name,false)

struct Rule
    origin
    expansion
    alias
    options
end

Rule(origin,expansion) = Rule(origin,expansion,nothing,nothing)

Base.show(io::IOStream,r::Rule) = print(io,"Rule($(r.origin), $(r.expansion), $(r.alias), $(r.options))")

struct RuleOptions
    keep_all_tokens
    expand1
    priority
end

RuleOptions() = RuleOptions(false,false,nothing)

