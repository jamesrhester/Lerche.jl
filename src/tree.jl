# What is Meta? A place to store extra information. All of
# the extra information that I could find is here
mutable struct Meta
    rule
    line::Int64
    column::Int64
    end_line::Int64
    end_column::Int64
    start_pos::Int64
    end_pos::Int64
    empty::Bool
    Meta() = new()
end

# Note that a Python slotted tree is just a tree with
# restricted possible fields, like a Julia struct.

mutable struct Tree
    data
    children
    _meta::Meta
end

Tree(data,children) = Tree(data,children,Meta())

meta(t::Tree) = t._meta

_pretty_label(t::Tree) = t.data

pretty(t::Tree,level,indent_str::String) = begin
    if length(t.children) == 1 && !(typeof(t.children[1]) == Tree)
        return [repeat(indent_str,level), _pretty_label(t), "\t", "$(t.children[1])", "\n"]
    end

    l = [ repeat(indent_str,level), _pretty_label(t), "\n" ]
    for n in t.children
        if typeof(n) == Tree
            append!(l, pretty(n,level+1, indent_str))
        else
            append!(l, [ repeat(indent_str,level+1), "$n", "\n" ])
        end
    end
    return l
end

pretty(t::Tree; indent_str = "  ") = join(pretty(t,0, indent_str),"")

#Base.show(io::IO,t::Tree) = println(io,pretty(t))
Base.show(io::IO,t::Tree) = println(io,"Tree($(t.data), $(t.children))")
"""
Expand children at the given indices. Rewritten for Julia as we
can't expand inline.
"""
expand_kids_by_index!(t::Tree, indices) = begin
    #println("Expanding $t and $indices")
    new_children = []
    for (i,c) in enumerate(t.children)
        if i in indices
            append!(new_children,c.children)
        else
            push!(new_children,c)
        end
    end
    t.children = new_children
    #println("Returning: $t")
end

Base.:(==)(t1::Tree,t2::Tree) = begin
    return t1.data == t2.data && t1.children == t2.children
end

Base.hash(t1::Tree) = begin
    return hash(t1.data) + hash(t1.children)
end

find_pred(t::Tree,pred) = begin
    filter(pred, collect(iter_subtrees(t)))
end

find_data(t::Tree,data) = begin
    find_pred(t, x -> x.data == data)
end

scan_values(t::Tree,pred) = Channel() do val_chan
    begin
        for c in t.children
            if typeof(c) == Tree
                for t in scan_values(c,pred)
                    put!(val_chan,t)
                end
            else
                if pred(c)
                    put!(val_chan,c)
                end
            end
        end
    end
end


iter_subtrees(t::Tree) = Channel() do tree_chan
    visited = Set()
    q = [t]
    l = []
    while !isempty(q)
        subtree = pop!(q)
        push!(l,subtree)
        if objectid(subtree) in visited
            continue
        end
        push!(visited,objectid(subtree))
        append!(q,[c for c in subtree.children if typeof(c) == Tree])
    end
    seen = Set()
    for x in reverse(l)
        if !(objectid(x) in seen)
            put!(tree_chan,x)
            push!(seen,objectid(x))
        end
    end
end

tree_set(t::Tree,data,children) = begin
    t.data = data
    t.children = children
end

