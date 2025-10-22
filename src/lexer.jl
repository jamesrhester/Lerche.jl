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

_get_flags(p1::Pattern,value) = begin
    add_flags(get_flags(p1),value)
end

"""
Add the characters in `flags` as regexp flags for
regular expression `value`
"""
add_flags(flags,value) = begin
    for f in flags
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
    _width
end

PatternRE(value;flags=Set()) = PatternRE(value,flags)
PatternRE(value,flags::String) = PatternRE(value,Set(flags))
PatternStr(value,flags::String) = PatternStr(value,Set(flags))
PatternStr(value,flags::Char) = PatternStr(value,[flags])
PatternRE(value,flags::Char) = PatternRE(value,[flags])
PatternRE(value,flags::Set) = PatternRE(value,flags,get_regexp_width(add_flags(flags,value)))

to_regexp(pre::PatternRE) = add_flags(pre.flags,pre.value)
to_regexp(pstr::PatternStr) = _get_flags(pstr,escape_re_string(pstr.value))
min_width(pre::PatternRE) = pre._width[1]
max_width(pre::PatternRE) = pre._width[2]

min_width(pstr::PatternStr) = length(pstr.value)
max_width(pstr::PatternStr) = min_width(pstr)

struct TerminalDef
    name::String
    pattern::Pattern
    priority::Int64
end

TerminalDef(name,pattern; priority=1) = TerminalDef(name,pattern,priority)

"""
    A string with meta-information, that is produced by the lexer.

    When parsing text, the resulting chunks of the input that haven't been discarded,
    will end up in the tree as Token instances. Token is a subclass of AbstractString
    so normal string comparisons and operations will work as expected.

    Attributes:
        type: Name of the token (as specified in grammar)
        value: Value of the token (redundant, as ``token.value == token`` will always be true)
        pos_in_stream: The index of the token in the text
        line: The line of the token in the text (starting with 1)
        column: The column of the token in the text (starting with 1)
        end_line: The line where the token ends
        end_column: The next column after the end of the token. For example,
            if the token is a single character with a column value of 4,
            end_column will be 5.
        end_pos: the index where the token ends (basically ``pos_in_stream + length(token)``)
"""
mutable struct Token <: AbstractString
    type_::String  #type is a Julia keyword
    pos_in_stream::Union{Int,Nothing}
    value::String
    line::Union{Int,Nothing}
    column::Union{Int,Nothing}
    end_line::Union{Int,Nothing}
    end_column::Union{Int,Nothing}
    end_pos::Union{Int,Nothing}
end

Token(type_,value;pos_in_stream=nothing,line=nothing,column=nothing) =
    Token(type_,pos_in_stream,String(value),line,column,nothing,nothing,nothing)

# Reimplement some string methods for later use...
Base.ncodeunits(t::Token) = ncodeunits(t.value)
Base.codeunit(t::Token) = codeunit(t.value)
Base.codeunit(t::Token,i::Integer) = codeunit(t.value,i)
Base.isvalid(t::Token,i::Int64) = isvalid(t.value,i)
Base.iterate(t::Token) = Base.iterate(t.value)
Base.iterate(t::Token,i::Int64) = Base.iterate(t.value,i)

# Efficent conversion to String, otherwise goes through printing IO.

Base.String(t::Token) = t.value

# Lexer.py implements a generic equality, where any comparison other
# than with another token defaults to string comparison
Base.:(==)(t1::Token,t2::Token) = begin
    if t1.type_ != t2.type_
        return false
    end
    return t1.value == t2.value
end

Base.:(==)(t1::Token,t2::String) = begin
    return isequal(t1.value,t2)
end

Base.:(==)(t1::String,t2::Token) = begin
    return isequal(t1,t2.value)
end

Base.hash(t1::Token) = hash(t1.value)

Base.show(io::IO,t::Token) = print(io,"Token($(t.type_), $(t.value))")

update(t::Token;type_=nothing,value=nothing) = begin
    return new_borrow_pos(type_!=nothing ? type_ : t.type_,
                          value!=nothing ? value : t.value,
                          t)
end

new_borrow_pos(type_,value, borrow_t) = Token(type_,borrow_t.pos_in_stream,value,
                                              borrow_t.line, borrow_t.column,borrow_t.end_line,
                                              borrow_t.end_column,borrow_t.end_pos)

## __reduce__ not reproduced; need to check if we need it.

mutable struct LineCounter
    newline_char::Char
    char_pos::Int #1 is first pos unlike Python
    line::Int
    column::Int
    line_start_pos::Int
end

LineCounter() = LineCounter('\n',1,1,1,1)

LineCounter(newline_char) = LineCounter(newline_char,1,1,1,1)
"""
feed

Consume a token and calculate the new line & column.
As an optional optimization, set test_newline=false if token doesn't contain a newline.
"""
feed!(lc::LineCounter,token;test_newline=true) = begin
    #println("Handling $token")
    if test_newline
        newlines = count(x -> x == lc.newline_char, token)
        if newlines > 0
            lc.line = lc.line + newlines
            newline_pos = first(findlast("$(lc.newline_char)",token))
            lc.line_start_pos = lc.char_pos + newline_pos
            lc.column = textwidth(token[newline_pos:end]) + 1
            #println("Line $(lc.line) starts at $(lc.line_start_pos)")
        else
            lc.column += textwidth(token)
        end
    else
        lc.column += textwidth(token)
    end
    lc.char_pos += ncodeunits(token)  #Unicode
    #lc.column = lc.char_pos - lc.line_start_pos + 1
    #println("At end column is $(lc.column) for char pos $(lc.char_pos) token length $(length(token))")
end

#==
mutable struct _Lex
    lexer
    state
end

_Lex(lexer;state=nothing) = _Lex(lexer,state)
==#

abstract type Lexer end

make_lexer_state(text) = begin
    line_ctr = LineCounter('\n')
    return LexerState(text,line_ctr)
end

# UnlessCallback appears to be a closure?
#struct UnlessCallback
#    mres
#end

# This and `_create_unless` check for strings that will also match
# REs. If a string pattern is matched by a re pattern, and the
# string has a greater priority, then even if the re pattern
# matches, the actual match will be the string pattern.
#
unless_callback(mres) = function (t)
    # Note that mres is a single tuple as we are not splitting
    # REs into 100-pattern chunks unlike Python
    mre, type_from_index = mres
    #println("Unless...$mre for $(t.value)")
    m = Base.match(mre,t.value,1)
    if m != nothing
        t.type_ = type_from_index[lastmatch(m)]
    end
    return t
end

_create_unless(terminals, g_regex_flags) = begin
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
            m = Base.match(Regex(to_regexp(retok.pattern),g_regex_flags),s)
            if m!=nothing && m.match == s
                push!(unless,strtok)
                if get_flags(strtok.pattern) <= get_flags(retok.pattern)
                    push!(embedded_strs,strtok)
                end
            end
        end
        if !isempty(unless)
            callback[retok.name] = unless_callback(build_mres(unless, g_regex_flags, match_whole = true))
        end
    end
    terminals = [t for t in terminals if !(t in embedded_strs)]
    return terminals, callback
end

# Match whole means that the regex
# must match the whole expression e.g. when checking tokens for matches with
# other things. "\A" forces the match to match at the beginning.
build_mres(terminals,g_regex_flags;match_whole=false) = begin
    postfix = ")"
    prefix = raw"\A("
    if match_whole
        postfix = "\$"
        prefix = "^"
    end
    prep_string = join(["(?P<$(t.name)>$(prefix*to_regexp(t.pattern)*postfix))" for t in terminals],"|")
    #println("String for regex: $prep_string")
    mres = Regex(prep_string,g_regex_flags)
    names_by_idx = Base.PCRE.capture_names(mres.regex)
    @debug "All regexes now $mres"
    return mres,names_by_idx
end

_regexp_has_newline(r) = begin
    return occursin("\n", r) || occursin("\\n", r) || occursin("[^", r) ||
         occursin("\\s",r) || (occursin("(?s", r) && occursin('.', r))
end


struct TraditionalLexer <: Lexer
    newline_types::Array{String,1}
    ignore_types::Set{String}
    callback::Dict{String,Function}
    terminals::Array{TerminalDef}
    mres::Tuple{Regex,Dict{Int,String}}
end

TraditionalLexer(conf) = begin
    terminals = collect(conf.tokens)
    @assert all(x-> x isa TerminalDef,terminals)
    if !conf.skip_validation 
        for t in terminals
            try
                Regex(to_regexp(t.pattern),conf.g_regex_flags)
            catch
                throw(LexError("Cannot compile token $(t.name): $(t.pattern)"))
            end
            if min_width(t.pattern) == 0
                throw(LexError("Lexer does not allow zero-width terminals. ($(t.name): $(t.pattern))"))
            end
        end
        @assert Set(conf.ignore) <= Set([t.name for t in terminals])
    end
    newline_types = [t.name for t in terminals if _regexp_has_newline(to_regexp(t.pattern))]
    ignore_types = Set(conf.ignore)
    sort!(terminals, by= x -> (-x.priority, -max_width(x.pattern),-length(x.pattern.value),x.name))
    @debug "Terminals: " terminals
    # _build called instead in Lark 0.11.1
    temp_terminals, callback = _create_unless(terminals,conf.g_regex_flags)
    for (type_,f) in conf.callbacks
        @assert !(type_ in keys(callback)) "TODO: add call chain" #Lark has CallChain here
        callback[type_] = f
    end
    TraditionalLexer(newline_types,ignore_types,callback,terminals,build_mres(temp_terminals,conf.g_regex_flags))
end

# Note that an empty string will match an empty regex, instead of returning nothing
match(tl::TraditionalLexer,text,pos) = begin
    if tl.mres[1] == r"" return nothing end
    m = Base.match(tl.mres[1],SubString(text,pos))
    if !(m===nothing)
        match_num = findfirst(v -> v!=nothing,m.captures)
        return m.match, tl.mres[2][match_num]
    end
end

next_token(tl::TraditionalLexer,lex_state) = begin
    line_ctr = lex_state.line_ctr
    @debug "Pos: $(line_ctr.char_pos) out of $(lex_state.text_length), remaining text\n"*lex_state.text[line_ctr.char_pos:end]*"EOF"
    while line_ctr.char_pos <= lex_state.text_length
        res = match(tl,lex_state.text,line_ctr.char_pos)
        if res === nothing
            allowed = setdiff(Set(values(tl.mres[2])), tl.ignore_types)
            if length(allowed) == 0
                allowed = Set(["<END-OF-FILE>"])
            end
            throw(UnexpectedCharacters(lex_state.text,line_ctr.char_pos,line_ctr.line,line_ctr.column,allowed=allowed,token_history=lex_state.last_token !== nothing ? [lex_state.last_token] : nothing))
        end
        value,type_ = res
        if !(type_ in tl.ignore_types)
            t = Token(type_,value,pos_in_stream=line_ctr.char_pos,line=line_ctr.line,column=line_ctr.column)
            feed!(line_ctr,value, test_newline=type_ in tl.newline_types)
            t.end_line = line_ctr.line
            t.end_column = line_ctr.column
            t.end_pos = line_ctr.char_pos
            if type_ in keys(tl.callback)
                t = tl.callback[type_](t)
                if !(t isa Token) throw(error("Callbacks must return a token (returned $t)")) end
            end
            lex_state.last_token = t
            return t
        else
            if type_ in keys(tl.callback)
                t2 = Token(type_, value, pos_in_stream=line_ctr.char_pos, line=line_ctr.line, column=line_ctr.column)
                tl.callback[type_](t2)
            end
            feed!(line_ctr,value,test_newline=type_ in tl.newline_types)
        end
    end
    return nothing
end

next_token(tl::TraditionalLexer,ls,dummy) = next_token(tl,ls)

include("common.jl")   #definitions for LexerConf and ParserConf

mutable struct LexerState
    text::String
    line_ctr::LineCounter
    last_token::Union{Token,Nothing}
    text_length::Int64   #optimisation for speed
end

LexerState(text,line_ctr;last_token=nothing) = begin
    LexerState(text,line_ctr,last_token,ncodeunits(text))
end

struct ContextualLexer <: Lexer
    lexers::Dict{Any,TraditionalLexer}
    root_lexer::TraditionalLexer
end

ContextualLexer(conf::LexerConf,states;always_accept=()) = begin
    terminals = collect(conf.tokens)
    tokens_by_name = Dict()
    for t in terminals
        @assert !(t.name in keys(tokens_by_name)) 
        tokens_by_name[t.name] = t
    end

    trad_conf = reconfigure(conf,terminals)
    lexer_by_tokens = Dict()
    lexers = Dict()
    lexer = missing  #define in outer scope
    for (state,accepts) in states
        @debug "State $state accepts $accepts"
        key = Set(accepts)
        try
            lexer = lexer_by_tokens[key]
        catch KeyError
            accepts = union( Set(accepts), conf.ignore, always_accept)
            state_tokens = [tokens_by_name[n] for n in accepts if (n != nothing && n in keys(tokens_by_name))]
            lexer_conf = reconfigure(trad_conf,state_tokens)
            lexer = TraditionalLexer(lexer_conf)
            lexer_by_tokens[key] = lexer
            @debug "Added lexer that accepts $key"
        end
        lexers[state] = lexer
    end
    @assert trad_conf.tokens == terminals
    root_lexer = TraditionalLexer(trad_conf)
    ContextualLexer(lexers,root_lexer)
end

# Was lex
next_token(cl::ContextualLexer,lexer_state,parser_state) = begin
    try
        lexer = cl.lexers[position(parser_state)]
        return next_token(lexer,lexer_state)
    catch e
        if e isa EOFError
            return nothing
        elseif e isa UnexpectedCharacters
            token = next_token(cl.root_lexer,lexer_state)
            throw(UnexpectedToken(token,e.allowed,state=position(parser_state)))
        else
            rethrow(e)
        end
    end
end

struct LexerThread
    lexer::Lexer
    state::LexerState
end

LexerThread(lexer,text::String) = LexerThread(lexer,make_lexer_state(text))

lex(l::LexerThread,parser_state) = l.lexer.lex(l.state,parser_state)

### Not in Lark: an iterator-based approach
#==
The parser state is passed in to the constructor. It would have been
created by the parser, and the current state is altered by the
parser externally.
==#
struct LexerParser
    lexer::Lexer
    lexer_state::LexerState
    parser_state
end

LexerParser(lexer::Lexer,text::AbstractString,ps) = begin
    LexerParser(lexer,make_lexer_state(text),ps)
end

LexerParser(lt::LexerThread,ps) = begin
    LexerParser(lt.lexer,lt.state,ps)
end

Base.iterate(l::LexerParser) = begin
    n = next_token(l.lexer,l.lexer_state,l.parser_state)
    if n === nothing return n end
    return (n,0)
end

Base.iterate(l::LexerParser,dummy) = begin
    n = next_token(l.lexer,l.lexer_state,l.parser_state)
    if n === nothing return n end
    return (n,0)
end

