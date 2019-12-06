#== lexer.py

from .utils import Str, classify  #Str not needed

from .exceptions import UnexpectedCharacters, LexError
==#
# Patterns.  We assume all have a value and flags field

abstract type Pattern end

get_flags(p1::Pattern) = p1.flags
get_value(p1::Pattern) = p1.value

Base.:(==)(p1::Pattern, p2::Pattern) = begin
    if typeof(p1) == typeof(p2) && p1.value == p2.value && p1.flags == p2.flags
        return true
    end
    return false
end

Base.hash(p1::Pattern) = begin
    return hash((typeof(p1),p1.value,p1.flags))
end

# TODO: how to look after flags in Julia?
_get_flags(p1::Pattern,value) = begin
    for f in get_flags(p1)
        value = ("(?$f)"*value)
    end
    return value
end

struct PatternStr <: Pattern
    value::String
    flags::Set{Char}
end

struct PatternRE <: Pattern
    value::String
    flags::Set{Char}
end

PatternRE(value;flags=Set()) = PatternRE(value,flags)
PatternRE(value,flags::String) = PatternRE(value,Set(flags))
PatternStr(value,flags::String) = PatternStr(value,Set(flags))
PatternStr(value,flags::Char) = PatternStr(value,[flags])
PatternRE(value,flags::Char) = PatternRE(value,[flags])

to_regexp(pre::PatternRE) = _get_flags(pre,pre.value)
to_regexp(pstr::PatternStr) = _get_flags(pstr,escape_re_string(pstr.value))
min_width(pre::PatternRE) = get_regexp_width(to_regexp(pre))[1]
max_width(pre::PatternRE) = get_regexp_width(to_regexp(pre))[2]

min_width(pstr::PatternStr) = length(pstr.value)
max_width(pstr::PatternStr) = min_width(pstr)

struct TerminalDef
    name::String
    pattern::Pattern
    priority::Int64
end

TerminalDef(name,pattern; priority=1) = TerminalDef(name,pattern,priority)

mutable struct Token #<: AbstractString
    type_::String
    pos_in_stream::Union{Int,Nothing}
    value::String
    line::Union{Int,Nothing}
    column::Union{Int,Nothing}
    end_line::Union{Int,Nothing}
    end_column::Union{Int,Nothing}
end

Token(type_,value;pos_in_stream=nothing,line=nothing,column=nothing) =
    Token(type_,pos_in_stream,value,line,column,nothing,nothing)

# Reimplement some string methods for later use...
Base.startswith(t::Token,c) = startswith(t.value,c)
Base.lstrip(t::Token,c) = lstrip(t.value,c)
Base.parse(::Type{T},t::Token) where T = Base.parse(T,t.value)

# Lexer.py implements a generic equality, where any comparison other
# than with another token defaults to string comparison
Base.:(==)(t1::Token,t2::Token) = begin
    if t1.type_ != t2.type_
        return false
    end
    return t1.value == t2.value
end

Base.:(==)(t1::Token,t2) = begin
    return isequal(t1.value,t2)
end

Base.:(==)(t1,t2::Token) = begin
    return isequal(t1,t2.value)
end

Base.hash(t1::Token) = hash(t1.value)

Base.show(io::IO,t::Token) = print(io,"Token($(t.type_), $(t.value))")

new_borrow_pos(type_,value, borrow_t) = Token(type_,value,borrow_t.pos_in_stream,line=borrow_t.line, column=borrow_t.column)

## __reduce__ not reproduced; need to check if we need it.

mutable struct LineCounter
    newline_char::Char
    char_pos::Int #1 is first pos unlike Python
    line::Int
    column::Int
    line_start_pos::Int
end

LineCounter() = LineCounter('\n',1,1,1,1)

"""
feed

Consume a token and calculate the new line & column.
As an optional optimization, set test_newline=false if token doesn't contain a newline.
TODO: check indexing is correct
"""
feed!(lc::LineCounter,token;test_newline=true) = begin
    if test_newline
        newlines = count(x -> x == lc.newline_char, token)
        if newlines > 0
            lc.line = lc.line + 1
            lc.line_start_pos = lc.char_pos + first(findlast("$(lc.newline_char)",token))
        end
    end
    lc.char_pos += length(token)
    lc.column = lc.char_pos - lc.line_start_pos + 1
end

#==
mutable struct _Lex
    lexer
    state
end

_Lex(lexer;state=nothing) = _Lex(lexer,state)
==#

abstract type Lexer end

# The lex function creates a channel which is iterated over to get the
# tokens. If state_src is not nothing, it is a channel from which to obtain
# the next parser state to determine the correct lexer to use.
lex(l::Lexer,stream::String,newline_types,ignore_types) = Channel() do token_chan
        newline_types = Set(newline_types)
        ignore_types = Set(ignore_types)

    line_ctr = LineCounter()
    sub_lexer = get_lexer(l)  #For contextual lexers
    while line_ctr.char_pos <= length(stream)
        #println("Now at char pos $(line_ctr.char_pos)")
        mres,names_by_idx = sub_lexer.mres   #
        m = Base.match(mres,stream,line_ctr.char_pos)
        if m == nothing || m.match == ""
            throw(UnexpectedCharacters(stream,line_ctr.char_pos, line_ctr.line,
                                       line_ctr.column))
        end
        t = nothing
        value = m.match
        match_num = min([i for (i,v) in enumerate(m.captures) if !isnothing(v)]...)
        type_ = names_by_idx[match_num]
        if !(type_ in ignore_types)
            t = Token(type_,value,pos_in_stream=line_ctr.char_pos,line=line_ctr.line,column=line_ctr.column)
            if t.type_ in collect(keys(sub_lexer.callback))
                t = sub_lexer.callback[t.type_](t)
            end
            put!(token_chan,t)
            sub_lexer = get_lexer(l)  #wait for update
        else
            if type_ in collect(keys(sub_lexer.callback))
                t = Token(type_, value, pos_in_stream=line_ctr.char_pos, line=line_ctr.line, column=line_ctr.column)
                sub_lexer.callback[type_](t)
            end
        end
        feed!(line_ctr,value,test_newline = type_ in newline_types)
        if t != nothing
            t.end_line = line_ctr.line
            t.end_column = line_ctr.column
        end
    end
end

# UnlessCallback appears to be a closure?
#struct UnlessCallback
#    mres
#end

# This checks for patterns that belong to different REs
unless_callback(mres) = function (t)
    # Note that mres is a single tuple as we are not splitting
    # REs into 100-pattern chunks unlike Python
    mre, type_from_index = mres
    m = Base.match(mre,t.value)
    if m != nothing
        t.type_ = type_from_index[m.offset]
    end
    return t
end

_create_unless(terminals) = begin
    tokens_by_type = classify(terminals,key = t->typeof(t.pattern))
    embedded_strs = Set()
    callback = Dict()
    for retok in get(tokens_by_type,PatternRE,[])
        unless = []
        for strtok in get(tokens_by_type,PatternStr,[])
            if strtok.priority > retok.priority
                continue
            end
            s = strtok.pattern.value
            m = Base.match(Regex(to_regexp(retok.pattern)),s)
            if !isnothing(m) && m.match == s
                push!(unless,strtok)
                if get_flags(strtok.pattern) <= get_flags(retok.pattern)
                    push!(embedded_strs,strtok)
                end
            end
        end
        if !isempty(unless)
            callback[retok.name] = unless_callback(build_mres(unless, match_whole = true))
        end
    end
    terminals = [t for t in terminals if !(t in embedded_strs)]
    return terminals, callback
end

# The python version is convoluted due to the maximum number of groups of 100
# Don't know yet if this applies to Julia. Match whole means that the regex
# must match the whole expression e.g. when checking tokens for matches with
# other things.
build_mres(terminals;match_whole=false) = begin
    postfix = ""
    if match_whole postfix = "\$" end
    prep_string = join(["(?P<$(t.name)>$(to_regexp(t.pattern)*postfix))" for t in terminals],"|")
    #println("String for regex: $prep_string")
    mres = Regex(prep_string)
    names_by_idx = Base.PCRE.capture_names(mres.regex)
    #println("All regexes now $mres")
    return mres,names_by_idx
end

_regexp_has_newline(r) = begin
    return occursin("\n", r) || occursin("\\n", r) || occursin("[^", r) ||
        (occursin("(?s", r) && occursin('.', r))
end


struct TraditionalLexer <: Lexer
    newline_types
    ignore_types
    callback
    terminals
    mres
end

TraditionalLexer(terminals;ignore=(),user_callbacks=Dict()) = begin
    @assert all(t -> typeof(t)==TerminalDef, terminals)
    for t in terminals
        try
            Regex(to_regexp(t.pattern))
        catch
            throw(LexError("Cannot compile token $(t.name): $(t.pattern)"))
        end
        if min_width(t.pattern) == 0
            throw(LexError("Lexer does not allow zero-width terminals. ($(t.name): $(t.pattern))"))
        end
    end
    @assert Set(ignore) <= Set([t.name for t in terminals])
    
    newline_types = [t.name for t in terminals if _regexp_has_newline(to_regexp(t.pattern))]
    sort!(terminals, by= x -> (-x.priority, -max_width(x.pattern),-length(x.pattern.value),x.name))
    terminals, callback = _create_unless(terminals)
    for (type_,f) in user_callbacks
        @assert !(type_ in collect(keys(callback)))
        callback[type_] = f
    end
    TraditionalLexer(newline_types,ignore,callback,terminals,build_mres(terminals))
end

lex(tl::TraditionalLexer,stream::String) = begin
    lex(tl,stream,tl.newline_types,tl.ignore_types)
end

mutable struct ContextualLexer <: Lexer
    lexers::Dict{Any,TraditionalLexer}
    root_lexer::TraditionalLexer
    parser_state
    state_source::Channel
end

ContextualLexer(terminals,states,state_channel;ignore=(),always_accept=(),user_callbacks=Dict()) = begin
    tokens_by_name = Dict()
    for t in terminals
        @assert !(t.name in collect(keys(tokens_by_name))) 
        tokens_by_name[t.name] = t
    end

    lexer_by_tokens = Dict()
    lexers = Dict()
    lexer = missing  #define in scope
    for (state,accepts) in states
        key = Set(accepts)
        try
            lexer = lexer_by_tokens[key]
        catch KeyError
            accepts = union( Set(accepts), ignore, always_accept)
            state_tokens = [tokens_by_name[n] for n in accepts if (n != nothing && n in keys(tokens_by_name))]
            lexer = TraditionalLexer(state_tokens, ignore=ignore, user_callbacks=user_callbacks)
            lexer_by_tokens[key] = lexer
        end
        lexers[state] = lexer
    end
    root_lexer = TraditionalLexer(terminals,ignore=ignore,user_callbacks=user_callbacks)
    ContextualLexer(lexers,root_lexer,nothing,state_channel)
end

# This is a method in Python designed to capture the lexer it belongs to
# in a closure, so the lexer doesn't have to be followed explicitly within
# the parser
#==
set_parser_state!(cl::ContextualLexer,state_src) = begin
    tt = time()
    new_state = take!(state_src)
    println("$tt: Set lexer parser state to $state")
end
==#
get_lexer(l::Lexer) = l

get_lexer(cl::ContextualLexer) = begin
    tt = time()
    #println("$tt: waiting for state change")
    flush(stdout)
    new_state = take!(cl.state_source)
    tt = time()
    #println("$tt: Set lexer parser state to $new_state")
    return cl.lexers[new_state]
end

set_lexer_state(l::Lexer,state) = nothing

set_lexer_state(l::ContextualLexer,state) = begin
    tt = time()
    #println("$tt: Send off state change to $state")
    put!(l.state_source,state)
end

get_channel(l::Lexer) = nothing
get_channel(l::ContextualLexer) = l.state_source

#== 

TODO. The contextual lexer switches lexers depending on the
parse state. To do this it creates a separate Lexer object
based on the current state and then returns a token. After
returning this token, it switches state ready to lex the
next token. This is why the _Lex object exists in Python:
it has mutable lexer and state fields. The Channel has only
one entry so that the lexer has to wait until the parser
has processed the token before deciding on which lexer to
use next.


lex(cl::ContextualLexer,stream) = Channel() do lexchan
    begin
        # Set the starting state
        new_state = take!(cl.state_source)
        l = cl.lexers[new_state]
        token_src = lex(l[1],stream,cl.root_lexer.newline_types,cl.root_lexer.ignore_types)
        while true
            x = nothing
            try
                x = take!(token_src)
            catch e
                println("Finishing lexing, final status $e")
                break
            end
            tt = time()
            println("$tt Token: $x")
            put!(lexchan,x)  #will block until the value is taken by the parser
            new_state = take!(cl.state_source)
            tt = time()
            println("$tt: Now setting the lexer state to $(new_state)")
            l[1] = cl.lexers[new_state]  #
            l[2] = new_state
        end
    end
end

==#

lex(cl::ContextualLexer,stream) = lex(cl,stream,cl.root_lexer.newline_types,cl.root_lexer.ignore_types)
