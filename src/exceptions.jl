struct GrammarError <: Exception
    message::String
end

# These are concrete to allow messages to be attached
# to them.
struct ParseError <: Exception end
struct LexError <: Exception
    message::String
end

#==
The UnexpectedInput type holds the common behaviour of
the UnexpectedCharacters and UnexpectedToken types
==#

abstract type UnexpectedInput <: Exception end

# TODO: are we accessing strings in a Unicode-aware way?
get_context(ui::UnexpectedInput,text;span=40) = begin
    pos = ui.pos_in_stream
    start = max(pos - span,1)
    _end = min(pos + span,length(text))
    before = rsplit(text[start:pos],"\n",limit=2)[end]
    after = split(text[pos+1:_end],"\n",limit=2)[1]
    return before*after*"\n"*repeat(" ",length(before)) * "^\n"
end

""" 
    Given a parser instance and a dictionary mapping some label with
    some malformed syntax examples, it'll return the label for the
    example that bests matches the current error.

For Julia we need to make sure the correct parse_fn is dispatched for
this to make sense.
"""
#== match_examples(ui::UnexpectedInput,parse_type,examples;parse_fn = parse) = begin
    candidate = nothing
    for (label,example) in examples
        for malformed in example
            try
                parse_fn(parse_type,malformed)
            catch ut
                if ut isa UnexpectedInput
                    if ut.state == ui.state
                        try
                            if ut.token == ui.token
                                return label
                            end
                        catch a
                            if !(a isa AttributeError)
                                rethrow(a)
                            end
                        end
                        if candidate == nothing
                            candidate = label
                        end
                    end
                else
                    rethrow(ut)
                end
            end
        end
    end
    return candidate
end
==#

struct UnexpectedCharacters <: UnexpectedInput
    seq
    pos_in_stream
    line
    column
    allowed
    considered_tokens
    state
end

UnexpectedCharacters(seq,lex_pos,line,column;state=nothing) = UnexpectedCharacters(seq,lex_pos,line,column,nothing,nothing,nothing)

Base.showerror(io::IO,uc::UnexpectedCharacters) = begin
    message = "Unexpected characters: \n\n $(get_context(uc,uc.seq))"
    if uc.allowed != nothing
        message *= "\nExpecting: $allowed"
    end
    println(io,message)
end

struct UnexpectedToken <: UnexpectedInput
    token
    expected
    line
    column
    considered_rules
    state
    pos_in_stream
end

UnexpectedToken(token,expected;considered_rules=nothing,state=nothing) = begin
    line = token.line
    column = token.column
    pos_in_stream = token.pos_in_stream
    UnexpectedToken(token,expected,line,column,considered_rules,state,pos_in_stream)
end

Base.showerror(io::IO,ut::UnexpectedToken) = begin
    message = "Unexpected token $(ut.token) at line $(ut.line), column $(ut.column).\n"
    message *= "Expected: $(join(ut.expected,", "))"
    println(message)
end

