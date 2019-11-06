export @have_method, @invoke_callback

## Is this a Julia builtin?
classify_bool(seq, pred) = begin
    true_elems = []
    false_elems = []

    for elem in seq
        if pred(elem)
            append!(true_elems,elem)
        else
            append!(false_elems,elem)
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
        if k in keys(d)
            append!(d[k],v)
        else
            d[k] = [v]
        end
    end
    return d
end

bfs(initial,expand) = begin
    Channel() do q_chan
        # Python: open_q = deque(list(initial))
        open_q = collect(initial)
        visited = Set(open_q)
        while length(open_q) > 0
            node = popfirst!(open_q)
            put!(q_chan,node)
            for next_node in expand(node)
                if !(next_node in visited)
                    push!(visited,next_node)
                    push!(open_q,next_node)
                end
            end
        end
    end
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
## Partial function

partial(f,a...) = (b...) -> f(a...,b...)

## Is my method defined? Is there a more elegant way?

macro have_method(s)
    quote
        try
            methods(@eval $__module__ Symbol($s))
        catch e
            if e isa UndefVarError
                println("$(Symbol($s)) not found")
                return false
            else
                rethrow(e)
            end
        end
        return true
    end
end

macro invoke_callback(c,args...)
    quote
        f = @eval $__module__ $(Symbol(c))
        println("Evaluating in $(@__MODULE__), callback is $f, $(typeof(f))")
        f($args...)
    end
end
