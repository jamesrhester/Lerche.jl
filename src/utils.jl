export have_method,invoke_callback, @rule, @inline_rule

classify_bool(seq, pred) = begin
    true_elems = eltype(seq)[]
    false_elems = eltype(seq)[]

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

This is e.g. used to classify patterns into types,
either literal strings or regular expressions.

"""
classify(seq;key=nothing,value=nothing) = begin
    d = Dict()
    for item in seq
        if !(key==nothing) k = key(item) else k = item end
        if !(value==nothing) v = value(item) else v = item end
        if haskey(d,k)
            push!(d[k],v)
        else
            d[k] = [v]
        end
    end
    return d
end

"""
    bfs(initial,expand)

Call function `expand` on elements of `initial` and 
any newly generated elements until
no new elements are generated. A sequence of elements
are returned via a channel.
"""
#TODO: use proper Queue from DataStructures
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

# I know there's probably a fancy way to do this in Julia but this
# is basically a copy of the Python version for now
raw"""
    Accepts a list of alternatives, and enumerates all their possible concatinations.

    Examples:
        >>> combine_alternatives([range(2), [4,5]])
        [[0, 4], [0, 5], [1, 4], [1, 5]]

        >>> combine_alternatives(["abc", "xy", '$'])
        [['a', 'x', '$'], ['a', 'y', '$'], ['b', 'x', '$'], ['b', 'y', '$'], ['c', 'x', '$'], ['c', 'y', '$']]

        >>> combine_alternatives([])
        [[]]
"""
combine_alternatives(lists) = begin
    if isempty(lists)
        return [[]]
    end
    @assert all(l->!isempty(l),lists)
    init = [[x] for x in lists[1]]
    return reduce((x,y) -> [vcat(i,j) for i in x for j in y],ll[2:end],init=init)
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
        throw(error("Cannot parse $regexp as regexp"))
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

## Defaultdict: initialise with a Type that produces the
## default value when the key is missing. Mimic use of defaultdict
## in Lark

struct DefaultDict{K,V} <: AbstractDict{K,V}
    d::Dict{K,V}
end

DefaultDict{K,V}() where {K,V} = begin
    DefaultDict(Dict{K,V}())
end

Base.getindex(d::DefaultDict{K,V},k) where {K,V} = begin
    if !haskey(d,k)
        d.d[k] = V()
    end
    return d.d[k]
end

Base.setindex!(d::DefaultDict,v,k) = d.d[k] = v
Base.haskey(d::DefaultDict,k) = haskey(d.d,k)
Base.keys(d::DefaultDict) = keys(d.d)
Base.values(d::DefaultDict) = values(d.d)
Base.iterate(d::DefaultDict) = Base.iterate(d.d)
Base.iterate(d::DefaultDict,s) = Base.iterate(d.d,s)
Base.length(d::DefaultDict) = Base.length(d.d)

"""
    @rule s

`s` is a function `rule_name(t,args)` where `t` is an instance of a subtype of
`Transformer` or `Visitor`. `args` is an array holding values for each node of
the grammar production.
"""
macro rule(s)
    if s.head != :(=) || !(s.args[1].head in(:call,:where))
        error("A rule must be a function definition, got $(s.head)")
    end
    c_expr = s.args
    while c_expr[1].head == :where
        c_expr = c_expr[1].args
    end
    rule_name = QuoteNode(c_expr[1].args[1])
    if c_expr[1].args[2].head != :(::)
        error("Type must be included in the first argument to define a rule: $s")
    end
    rule_type = c_expr[1].args[2].args[2] # the type name
    @debug "Rule name: $rule_name, Rule type $rule_type"
    c_expr[1] = :(Lerche.transformer_func($(c_expr[1].args[2]),::Val{$rule_name},meta::Lerche.Meta,$(c_expr[1].args[3])))
    @debug "$s"
    esc(quote $s end)
end
   
"""
    @inline_rule s

`s` is a function of the form `rule_name(t,args...)` where `rule_name` is the
name of a grammar rule and `args` are values for the individual elements of that
rule's grammar production. This rule is called by `transform(t,tree)`
and `visit(v,tree)`. `t` is a subtype of `Transformer` and v is a
subtype of `Visitor`.
"""
macro inline_rule(s)
    if s.head != :(=) || !(s.args[1].head in (:call,:where))
        error("A rule must be a function definition")
    end
    c_expr = s.args
    while c_expr[1].head == :where
        c_expr = c_expr[1].args
    end
    rule_name = QuoteNode(c_expr[1].args[1])
    rule_type = c_expr[1].args[2].args[2] # the type name
    @debug "Inline rule name: $rule_name, Rule type $rule_type"
    @debug "Args $(c_expr.args)"
    #== esc(quote
    Lerche.transformer_func(x::$rule_type,y::Val{$rule_name},meta::Lerche.Meta,z::Array) = Lerche.transformer_func(x,y,z...) 
    Lerche.transformer_func($(s.args[1].args[2]),::Val{$rule_name},$(s.args[1].args[3:end]...)) = $(s.args[2])
    end) ==#
    
    c_expr[1] = :(Lerche.transformer_func($(c_expr[1].args[2]),::Val{$rule_name},$(c_expr[1].args[3:end]...)))
    #== esc(quote
        Lerche.transformer_func(x::$rule_type,y::Val{$rule_name},meta::Lerche.Meta,z::Array) = Lerche.transformer_func(x,y,z...)
    ==#
    @debug "$s"
    esc(quote
        Lerche.transformer_func(x::$rule_type,y::Val{$rule_name},meta::Lerche.Meta,z::Array) = Lerche.transformer_func(x,y,z...);
        $s
        end)
end

"""
    grab!(s::Stack,n)

Pop! the top `n` elements of `s`, returning them as a list in reverse order of removal
"""
grab!(s::Stack{T},n) where T = begin
    output = Array{T,1}(undef,n)
    for i in 1:n
        output[n-i+1] = pop!(s)
    end
    return output
end
