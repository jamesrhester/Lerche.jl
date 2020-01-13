export have_method,invoke_callback, @rule, @inline_rule, @rule_holder, @contains_rules

# These definitions just for internal module use
const _rule_dict = IdDict{Tuple,Function}()
get_rule_dict() = _rule_dict

#const _rule_dict = IdDict{Tuple,Function}()

#@noinline get_rule_dict() = _rule_dict

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
# fail in future.  We do a naive guess for the maximum length.
get_regexp_width(regexp) = begin
    minlength = 0
    try
        r = Regex(regexp)
        minlength = Base.PCRE.info(r.regex,Base.PCRE.INFO_MINLENGTH,Int32)
    catch
        error("Cannot parse $regexp")
    end
    # A naive guess. Character ranges are not caught, neither are repeats
    if (occursin("+",regexp) && !occursin("\\+",regexp))|| (
        occursin("*",regexp) && !occursin("\\*",regexp))
        maxlength = 1000
        #println("$regexp has a big maxlength")
    else
        maxlength = minlength
    end
    return (minlength,maxlength)

end

## ============Only in the Julia version ===========
##
## Regular expression escape; copied from Python version

escape_re_string(s::String) = begin
    special_chars = "()[]{}?*+-|^\$\\.&~# \t\n\r\v\f"
    replace(s, Set(special_chars) => x -> "\\" * x)
end

## Find the largest index of matching captures

lastmatch(m::RegexMatch) = begin
    found_match = 0
    for i in 1:length(m.captures)
        if m.captures[i] != nothing
            found_match = i
        end
    end
    return found_match
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
    println("Rule name: $rule_name, Rule type $rule_type")
    esc(quote
        get_rule_dict()[($rule_name,$rule_type)] = $s
    end )
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
    println("Inline rule name: $rule_name, Rule type $rule_type")
    esc(quote
        println("Setting rule "*$rule_name*", length $(length(get_rule_dict()))")
        get_rule_dict()[($rule_name,$rule_type)] = (x,y) -> $s(x,y...)
    end)
end

# Adds a dictionary for storing rules
macro rule_holder()
    s1 = esc(:(const _rule_dict = IdDict{Tuple,Function}();get_rule_dict()=_rule_dict))
    return s1
end

macro contains_rules(s)
    if s.head != :struct
        error("Macro contains_rules must be called on type declaration")
    end
    members = s.args[3].args
    push!(members,esc(:(_rule_dict::IdDict{Tuple,Function})))
    # build a new full constructor
    if s.args[2] isa Symbol
        typename = s.args[2]
    else   #There is a type constructor, assume no parameters...
        typename = s.args[2].args[1]
    end
    
    all_members = length(filter(x-> typeof(x) != LineNumberNode,members))
    dummy_args = []
    rule_holder = :_rule_dict
    for i in 1:(all_members-1) push!(dummy_args,gensym()) end
    constructor = :($(esc(typename))($(dummy_args...))=$(esc(typename))($(dummy_args...),$(esc(rule_holder))))
    quote
        $s
        $constructor
    end
end

#have_method(t,meth_name) = haskey(get_rule_dict(),(meth_name,typeof(t)))
#get_method(t,meth_name) = get_rule_dict()[(meth_name,typeof(t))]

have_method(t,meth_name) = !isnothing(t) && haskey(t._rule_dict,(meth_name,typeof(t)))
get_method(t,meth_name) = t._rule_dict[(meth_name,typeof(t))]
#==
have_method(t,meth_name) = begin
    f = nothing
    try
        f=eval(Symbol(meth_name))
    catch e
        if e isa UndefVarError
            return false
        else
            rethrow(e)
        end
    end
    return length(methods(f,Tuple{typeof(t),Any})) != 0
end

get_method(t,meth_name) = eval(Symbol(meth_name))
==#
