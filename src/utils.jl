export have_method, invoke_callback, _rule_dict, @rule

const _rule_dict = Dict{Tuple,Function}()

## Is this a Julia builtin?
classify_bool(seq, pred) = begin
    true_elems = []
    false_elems = []

    for elem in seq
        if pred(elem)
            push!(true_elems,elem)
        else
            push!(false_elems,elem)
        end
    end
    return true_elems, false_elems
end


"""
classify(sequence, key=nothing,value=nothing)

Return a dictionary where the items in sequence
are classified into groups according to the key()
function, with corresponding values calculated
using the value() function. If neither key nor
value are provided, items become their own
values.

This is used to classify patterns into types,
either literal strings or regular expressions.

"""
classify(seq;key=nothing,value=nothing) = begin
    d = Dict()
    for item in seq
        if !(key==nothing) k = key(item) else k = item end
        if !(value==nothing) v = value(item) else v = item end
        if k in collect(keys(d))
            push!(d[k],v)
        else
            d[k] = [v]
        end
    end
    return d
end

bfs(initial,expand) = Channel() do q_chan
    # Python: open_q = deque(list(initial))
    open_q = collect(initial)
    visited = Set(open_q)
    # println("Queue start: $visited")
    while length(open_q) > 0
        node = popfirst!(open_q)
        #println("\nReturning $node")
        put!(q_chan,node)
        for next_node in expand(node)
            if !(next_node in visited)
                push!(visited,next_node)
                push!(open_q,next_node)
            end
        end
    end
    #println("BFS has finished!!")
end

# The Python version
# returns (min, max) possible width of matching strings. We only
# have access to the minimum width through PCRE, so we use that.
# Note that this relies on implementation details of the pcre2
# access in Julia base (not part of the published API) so may
# fail in future.  We provide a dummy maximum length.
get_regexp_width(regexp) = begin
    try
        r = Regex(regexp)
        return Base.PCRE.info(r.regex,Base.PCRE.INFO_MINLENGTH,Int32),missing
    catch
        error("Cannot parse $regexp")
    end
end

## ============Only in the Julia version ===========
##
## Regular expression escape; copied from Python version

escape_re_string(s::String) = begin
    special_chars = "()[]{}?*+-|^\$\\.&~# \t\n\r\v\f"
    replace(s, Set(special_chars) => x -> "\\" * x)
end


## Partial function

partial(f,a...) = (b...) -> return f(a...,b...)


## Helpers for constructing subtypes with methods named after rules

"""
Add a field to the concrete type containing a dictionary of functions.

Use as @has_rules struct .... end. A constructor for the type
without the extra member is also defined. Not currently used
"""
macro add_rules(s)
    if s.head != :struct
        error("Macro @has_rules can only be used on structure definitions")
    end
    push!(s.args[3].args,:(_rule_methods::Dict{String,Function}))
    # Create a default constructor
    constructor = :($(s.args[2])(a,b,c) = ($(s.args[2]))(a,b,c,Dict()))
    return :($s; $(esc(constructor)))
end

"""
Flag that the following method is a rule. Usage: @rule rule_name(t::Type,args) = begin .... end.
There can only be one argument after the type argument, which will be the tree children.
"""

macro rule(s)
    if s.head != :(=) || s.args[1].head != :call
        error("A rule must be a function definition")
    end    
    rule_name = String(s.args[1].args[1])
    rule_type = s.args[1].args[2].args[2] # the type name
    #println("Rule name: $rule_name, Rule type $rule_type")
    quote
        _rule_dict[($rule_name,$(esc(rule_type)))] = $(esc(s))
    end
end

"""
Flag that the following is an inline rule, that is, that
its arguments should be splatted when called.

TODO: splice in the results of rule(s) instead of rewriting it.
"""
macro inline_rule(s)
    if s.head != :(=) || s.args[1].head != :call
        error("A rule must be a function definition")
    end    
    rule_name = String(s.args[1].args[1])
    rule_type = s.args[1].args[2].args[2] # the type name
    #println("Inline rule name: $rule_name, Rule type $rule_type")
    quote
        _rule_dict[($rule_name,$(esc(rule_type)))] = (x,y) -> $(esc(s))(x,y...)
    end
end

have_method(t,meth_name) = haskey(_rule_dict,(meth_name,typeof(t)))
get_method(t,meth_name) = _rule_dict[(meth_name,typeof(t))]
