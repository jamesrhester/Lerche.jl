# What is Meta? A place to store extra information. All of
# the extra information that I could find is here
mutable struct Meta
    rule
    line
    column
    end_line
    end_column
    Meta() = new()
end

# We do not use slotted trees as there is no efficiency win
# Is there any other use for them?

mutable struct Tree
    data
    children
    _meta::Meta
end

Tree(data,children) = Tree(data,children,Meta())

meta(t::Tree) = t._meta

_pretty_label(t::Tree) = t.data

pretty(t::Tree,level,indent_str) = begin
    if length(t.children) == 1 && !(typeof(t.children[1] == Tree))
        return [indent_str*level, _pretty_label(t), "\t", "$(t.children[1])", "\n"]
    end

    l = [ indent_str*level, _pretty_label(t), "\n" ]
    for n in t.children
        if typeof(n) == Tree
            l += pretty(n,level+1, indent_str)
        else
            l += [ indent_str*(level+1), "$n", "\n" ]
        end
    end
    return l
end

pretty(t::Tree; indent_str = "  ") = join(" ", pretty(t,0, indent_str))

"""
Expand (inline) children at the given indices. This may not work as we
can't change array sizes??
"""
expand_kids_by_index(t::Tree, indices...) = begin
    for i in sort(indices, rev=true)
        kid = t.children[i]
        t.children[i:i] = kid.children
    end
end

Base.:(==)(t1::Tree,t2::Tree) = t1.data == t2.data && t1.children == t2.children

find_pred(t::Tree,pred) = begin
    filter(pred, iter_subtrees(t))
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
    while q
        subtree = pop!(q)
        append!(l,subtree)
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

