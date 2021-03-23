struct GrammarError <: Exception
    message::String
end

# These are concrete to allow messages to be attached
# to them.
struct ParseError <: Exception end
struct LexError <: Exception
    message::String
end

struct UnexpectedEOF <: Exception
    expected::Array
end

Base.showerror(io::IO, ue::UnexpectedEOF) = begin
    println("Unexpected end-of-input. Expected one of:"* join(expected,"\t\t*")*"\n")
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
    return before*after*"\n"*repeat(" ",length(before)-1) * "^\n"
end

""" 
    Given a parser instance and a dictionary mapping some label with
    some malformed syntax examples, it'll return the label for the
    example that bests matches the current error.

For Julia we need to make sure the correct parse_fn is dispatched for
this to make sense.
"""
match_examples(ui::UnexpectedInput,parse_call,examples::Dict,token_type_match_fallback=false,use_accepts=false) = begin
    match_examples(ui,parse_call,((a,b) for (a,b) in examples),
                   token_type_match_fallback=token_type_match_fallback,
                   use_accepts=use_accepts)
end


match_examples(ui::UnexpectedInput,parse_call,examples,token_type_match_fallback=false,use_accepts=false) = begin
    candidate = (nothing,false)
    for (i,(label,example)) in enumerate(examples)
        @assert !(example <: AbstractString)
        for (j,malformed) in enumerate(example)
            try
                parse_call(malformed)
            catch ut
                if ut isa UnexpectedInput
                    if ut.state == ui.state
                        if use_accepts && ut.accepts != ui.accepts
                            @debug "Different accepts with the same state[$(ui.state)]: $(ui.accepts) != $(ut.accepts) at example [$i][$j]"
                            continue
                        end
                        try
                            if ut.token == ui.token
                                @debug "Exact match at example [$i][$j]"
                                return label
                            end
                            if token_type_match_fallback
                                if (ut.token.type_ == ui.token.type_) && candidate[end]==nothing
                                    @debug "Token type fallback at example [$i][$j]"
                                    candidate = label,true
                                end
                            end
                        catch a
                            if !(a isa AttributeError)
                                rethrow(a)
                            end
                        end
                        if candidate[1] == nothing
                            @debug "Same state match at example [$i][$j]"
                            candidate = label,false
                        end
                    end
                else
                    rethrow(ut)
                end
            end
        end
    end
    return candidate[1]
end


struct UnexpectedCharacters <: UnexpectedInput
    seq
    pos_in_stream
    line
    column
    allowed
    considered_tokens
    state
    token_history
end

UnexpectedCharacters(seq,lex_pos,line,column;allowed=nothing,consider_tokens=nothing,state=nothing,token_history=nothing) = UnexpectedCharacters(seq,lex_pos,line,column,allowed,consider_tokens,state,token_history)

Base.showerror(io::IO,uc::UnexpectedCharacters) = begin
    s = uc.seq[uc.pos_in_stream]
    message = "No terminal defined for $s at line $(uc.line) col $(uc.column)"
    message = message * "\n\n" * get_context(uc,uc.seq)
    if uc.allowed != nothing
        message *= "\nExpecting: $(uc.allowed)"
    end
    if uc.token_history != nothing
        message *= "\nPrevious tokens: " * join(uc.token_history,", ")*"\n"
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
    puppet
    accepts
end

UnexpectedToken(token,expected;considered_rules=nothing,state=nothing,puppet=nothing) = begin
    line = token.line
    column = token.column
    pos_in_stream = token.pos_in_stream
    accepts = puppet != nothing && accepts(puppet)
    UnexpectedToken(token,expected,line,column,considered_rules,state,pos_in_stream,puppet,accepts)
end

Base.showerror(io::IO,ut::UnexpectedToken) = begin
    message = "Unexpected token $(ut.token) at line $(ut.line), column $(ut.column).\n"
    message *= "Expected one of: $(join(ut.expected,", "))"
    println(message)
end

struct VisitError <: Exception
    tree
    orig_exc
end

Base.showerror(io::IO,ve::VisitError) = begin
    println("Error trying to process rule \"$(ve.tree.data):\n\n$(ve.orig_exc)\" ")
end
