export have_method,invoke_callback, @rule, @inline_rule

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

"""
Flag that the following method is a rule. Usage: @rule rule_name(t::Type,args) = begin .... end.
There can only be one argument after the type argument, which will be the tree children.
"""

macro rule(s)
    if s.head != :(=) || s.args[1].head != :call
        error("A rule must be a function definition")
    end    
    rule_name = QuoteNode(s.args[1].args[1])
    if s.args[1].args[2].head != :(::)
        error("Type must be included in the first argument to define a rule: $s")
    end
    rule_type = s.args[1].args[2].args[2] # the type name
    println("Rule name: $rule_name, Rule type $rule_type")
    esc(quote
        Lerche.transformer_func($(s.args[1].args[2]),::Val{$rule_name},meta::Lerche.Meta,$(s.args[1].args[3])) = $(s.args[2])
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
    rule_name = QuoteNode(s.args[1].args[1])
    rule_type = s.args[1].args[2].args[2] # the type name
    println("Inline rule name: $rule_name, Rule type $rule_type")
    println("Args $(s.args[1].args)")
    esc(quote
        Lerche.transformer_func(x::$rule_type,y::Val{$rule_name},meta::Lerche.Meta,z::Array) = Lerche.transformer_func(x,y,z...) 
        Lerche.transformer_func($(s.args[1].args[2]),::Val{$rule_name},$(s.args[1].args[3:end]...)) = $(s.args[2])
    end)
end

"""
    grab!(s::Stack,n)

Pop! the top `n` elements of `s`, returning them as a list in order of removal
"""
grab!(s::Stack,n) = begin
    output = []
    for i in 1:n
        push!(output,pop!(s))
    end
    return reverse!(output)
end
