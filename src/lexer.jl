#== lexer.py

from .utils import Str, classify  #Str not needed

from .exceptions import UnexpectedCharacters, LexError
==#
# Patterns.  We assume all have a value and flags field

abstract type Pattern end

get_flags(p1::Pattern) = p1.flags
get_value(p1::Pattern) = p1.value

Base.equals(p1::Pattern, p2::Pattern) = begin
    if p1.value == p2.value && p1.flags == p2.flags
        return true
    end
    return false
end

# TODO: how to look after flags in Julia?
_get_flags(p1::Pattern,value) = begin
    for f in get_flags(p1)
        value = ("(?$f)"*value)
    end
    return value
end

struct PatternStr <: Pattern
    value
    flags
end

struct PatternRe <: Pattern
    value
    flags
end

to_regexp(pre::PatternRe) = _get_flags(pre.value)
to_regexp(pstr::PatternStr) = _get_flags(escape_string(pstr.value))
min_width(pre::PatternRe) = get_regexp_width(to_regexp(pre))[1]
max_width(pre::PatternRe) = get_regexp_width(to_regexp(pre))[2]

min_width(pstr::PatternStr) = length(pstr.value)
max_width(pstr::PatternStr) = min_width(pstr)

struct TerminalDef
    name
    pattern
    priority
end

TerminalDef(name,pattern; priority=1) = TerminalDef(name,pattern,priority)

mutable struct Token <: String
    type_
    pos_in_stream
    value
    line
    column
    end_line
    end_column
end

Token(type_,value;pos_in_stream=nothing,line=nothing,column=nothing) =
    Token(type_,pos_in_stream,value,line,column,nothing,nothing)

# Lexer.py ... is this the right meaning?
Base.equals(t1::Token,t2::Token) = begin
    if t1.type_ != t2.type_
        return false
    end
    return t1 == t2
end

new_borrow_pos(type_,value, borrow_t) = Token(type_,value,borrow_t.pos_in_stream,line=borrow_t.line, column=borrow_t.column)

## __reduce__ not reproduced; need to check if we need it.

mutable struct LineCounter
    newline_char
    char_pos::Int
    line::Int
    column::Int
    line_start_pos::Int
end

LineCounter() = LineCounter("\n",0,1,1,0)

"""
feed

Consume a token and calculate the new line & column.
As an optional optimization, set test_newline=false if token doesn't contain a newline.
TODO: check indexing is correct
"""
feed(lc::LineCounter,token;test_newline=true) = begin
    if test_newline
        newlines = count(x -> x == lc.newline_char, token)
        if newlines > 0
            lc.line = lc.line + 1
            lc.line_start_pos = lc.char_pos + findlast(lc.newline_char)+1
        end
    end
    lc.char_pos += length(token)
    lc.column = lc.char_pos - lc.line_start_pos + 1
end

mutable struct _Lex
    lexer
    state
end

_Lex(lexer;state=nothing) = _Lex(lexer,state)

# We create a channel
lex(l::_Lex,stream,newline_types,ignore_types) = Channel() do token_chan
    begin
        newline_types = Set(newline_types)
        ignore_types = Set(ignore_types)

        line_ctr = LineCounter()
        while line_ctr.char_pos < length(stream)
            not_found = true
            for (mre, type_from_index) in l.lexer.mres
                m = match(mre,streamline_ctr.char_pos)
                if m == nothing
                    continue
                end

                t = nothing
                value = m.match
                type_ = type_from_index[m.offsets[1]] #is offset correct here?
                if type_ not in ignore_types
                    t = Token(type_,value,line_ctr.char_pos,line_ctr.line,line_ctr.column)
                    if t.type_ in keys(l.lexer.callback)
                        t = l.lexer.callback[t.type_](t)
                    end
                    put!(token_chan,t)
                else
                    if type_ in keys(l.lexer.callback)
                        t = Token(type_, value, line_ctr.char_pos, line_ctr.line, line_ctr.column)
                        lexer.callback[type_](t)
                    end
                end
                feed(line_ctr,value,test_newline = type_ in newline_types)
                if t != nothing
                    t.end_line = line_ctr.line
                    t.end_column = line_ctr.column
                end
                not_found = false
                break
            end
            if not_found
                throw(UnexpectedCharacters(stream,line_ctr.char_pos, line_ctr.line,
                                           line_ctr.column,state=l.state))
            # Python has an else clause here that is executed if the for
            # statement finishes normally (not via a break).
                # It raises an error if the stream is not exhausted
            end
        end
    end
end

# UnlessCallback appears to be a closure?
#struct UnlessCallback
#    mres
#end

# Looks like this will find tokens in the list of regexps
unless_callback(mres) = begin
    function (t)
        for (mre, type_from_index) in mres
            m = match(mre,t.value)
            if m != nothing
                t.type_ = type_from_index[m.offset]
                break
            end
        end
        return t
    end
end

_create_unless(terminals) = begin
    tokens_by_type = classify(terminals,keys = t->typeof(t.pattern))
    embedded_strs = Set()
    callback = Dict()
    for retok in get(tokens_by_type,PatternRE,[])
        unless = []
        for strtok in get(tokens_by_type,PatternStr,[])
            if strtok.priority > retok.priority
                continue
            end
            s = strtok.pattern.value
            m = match(to_regexp(retok.pattern),s)
            if m && m.match == s
                append!(unless,strtok)
                if get_flags(strtok.pattern) <= get_flags(retok.pattern)
                    push!(embedded_strs,strtok)
                end
            end
        end
        if unless
            callback[retok.name] = unless_callback(build_mres(unless, match_whole = true))
        end
    end
    terminals = [t for t in terminals if !(t in embedded_strs)]
    return terminals, callback
end

# The python version is convoluted due to the maximum number of groups of 100
# Don't know yet if this applies to Julia
build_mres(terminals, max_size; match_whole=true) = begin
    postfix = ""
    if match_whole postfix = "$" end
    mres = Regex(join("|" , ["(?P<$(t.name)>%$(to_regexp(t.pattern)*postfix)" for t in terminals]))
end

_regexp_has_newline(r) = begin
    return "\n" in r || "\\n" in r || "[^" in r or ("(?s" in r && '.' in r)
end

abstract type Lexer end

struct TraditionalLexer <: Lexer
    newline_types
    ignore_types
    callback
    terminals
    mres
end

TraditionalLexer(terminals,ignore=(),user_callbacks=Dict()) = begin
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
    sort!(terminals, by= x -> (-x.priority, -max_width(x),-length(x.pattern.value),x.name))
    terminals, callback = _create_unless(tokens)
    for (type_,f) in user_callbacks
        @assert !(type_ in keys(callback))
        callback[type_] = f
    end
    TraditionalLexer(newline_types,ignore_types,callback,terminals,build_mres(tokens))
end

lex(tl::TraditionalLexer,stream) = begin
    lex(tl,stream,tl.newline_types,tl.ignore_types)
end

mutable struct ContextualLexer <: Lexer
    lexers
    root_lexer
    parser_state
end

ContextualLexer(terminals,states;ignore=(),always_accept=(),user_callbacks=Dict()) = begin
    tokens_by_name = Dict()
    for t in terminals
        @assert !(t.name in keys(tokens_by_name)) 
        tokens_by_name[t.name] = t
    end

    lexer_by_tokens = Dict()
    lexers = Dict()
    for (state,accepts) in states
        key = Set(accepts)
        try
            lexer = lexer_by_tokens[key]
        catch KeyError
            accepts = union( Set(accepts), ignore, always_accept)
            state_tokens = [tokens_by_name[n] for n in accepts if (n != nothing && n in tokens_by_name)]
            lexer = TraditionalLexer(state_tokens, ignore=ignore, user_callbacks=user_callbacks)
            lexer_by_tokens[key] = lexer
        end
        lexers[state] = lexer
    end
    root_lexer = TraditionalLexer(tokens,ignore=ignore,user_callbacks=user_callbacks)
    ContextualLexer(lexers,root_lexer,nothing)
end

set_parser_state!(cl,state) = cl.parser_state = state

lex(cl::ContextualLexer,stream) = Channel() do lexchan
    begin
        l = _Lex(cl.lexers[cl.parser_state],cl.parser_state)
        for x in lex(l,stream,cl.root_lexer.newline_types,cl.root_lexer.ignore_types)
            put!(lexchan,x)
            # TODO: are we really allowed to mess with our lexer settings within the
            # iteration??
            l.lexer = cl.lexer[c.parser_state]
            l.state = cl.parser_state
        end
    end
end
