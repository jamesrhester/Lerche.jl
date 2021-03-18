# Replace terminal/non-terminal with
# Traits.  Not sure if this is a win.

abstract type LarkSymbol end

abstract type TerminalTrait end
struct IsTerminal <: TerminalTrait end
struct IsNotTerminal <: TerminalTrait end

# == Symbols == #
Base.:(==)(s1::LarkSymbol,s2::LarkSymbol) = begin
    typeof(s1) == typeof(s2) && s1.name == s2.name
end

Base.hash(s1::LarkSymbol,h::UInt64) = begin
    hash(s1.name,h)
end

Base.show(io::IO,ls::LarkSymbol) = print(io,"$(typeof(ls))($(ls.name))")
Base.isless(s1::LarkSymbol,s2::LarkSymbol) = s1.name < s2.name
# == Terminals == #
struct Terminal <: LarkSymbol
    name::String
    filter_out::Bool
end

Terminal(name;filter_out=false) = Terminal(name,filter_out)
TerminalTrait(::Type{Terminal}) = IsTerminal()
Base.show(io::IO,ls::Terminal) = print(io,"$(ls.name)")

# == Non terminals == #
struct NonTerminal <: LarkSymbol
    name::String
    #is_term::Bool
end

TerminalTrait(::Type{NonTerminal}) = IsNotTerminal()

Base.show(io::IO,ls::NonTerminal) = print(io,"$(ls.name)")

is_terminal(l::T) where {T} = is_terminal(TerminalTrait(T),l)
is_terminal(::IsTerminal,l) = true
is_terminal(::IsNotTerminal,l) = false

# == Rule options == #

struct RuleOptions
    keep_all_tokens::Bool
    expand1::Bool
    priority::Union{Int,Nothing}
    template_source
    empty_indices
end

RuleOptions(;keep_all_tokens=false,expand1=false,priority=nothing,template_source=nothing,empty_indices=()) = RuleOptions(keep_all_tokens,expand1,priority,template_source,empty_indices)

Base.show(io::IO,ro::RuleOptions) = begin
    print(io,"RuleOptions($(ro.keep_all_tokens),$(ro.expand1), $(ro.priority), $(ro.template_source))")
end

mutable struct Rule
    origin::NonTerminal
    expansion::Array{LarkSymbol}
    order::Int32
    alias::Union{Nothing,String}
    options::Union{Nothing,RuleOptions}
end

Rule(origin,expansion;order=0,alias=nothing,options=nothing) = Rule(origin,expansion,order,alias,options)

Base.show(io::IO,r::Rule) = print(io,"<$(r.origin.name):  $(join(r.expansion," "))>")

# Equality, needed for indexing into rules

Base.:(==)(r1::Rule,r2::Rule) = begin
    return r1.origin == r2.origin && r1.expansion == r2.expansion
end

# The rule name should be unique enough
Base.hash(r1::Rule,h::UInt64) = hash(r1.origin,h)
