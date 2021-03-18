abstract type ParserPuppet end

struct NormalParserPuppet
    parser
    parser_state
    lexer_state
end

# In lark, the immutable puppet keeps its
# result in a class variable so it can be
# hashed. I don't think we have this issue
# in Julia.
struct ImmutableParserPuppet
    parser
    parser_state
    lexer_state
end

feed_token(p::ParserPuppet,token) = begin
    feed_token(p.parser_state,token, token.type_ == "\$END")
end

# TODO: proper copies

isequal(p1::ParserPuppet,p2) = false
isequal(p1::ParserPuppet,p2::ParserPuppet) = begin
    p1.parser_state == p2.parser_state && p1.lexer_state == p2.lexer_state
end

copy(p::ParserPuppet) = typeof(p)(p.parser,copy(p.parser_state),copy(p.lexer_state))

as_immutable(p::ParserPuppet) = ImmutableParserPuppet(p.parser,p.parser_state,p.lexer_state)

pretty(p::ParserPuppet) = begin
    out = ["Puppet choices:"]
    for (k,v) in p.choices()
        push!(out,"\t- $k -> $v")
    end
    push!(out,"stack size: $(length(p.parser_state.state_stack))")
    return join(out,"\n")
end

choices(p::ParserPuppet) = p.parser_state.parse_conf.parse_table.states[p.parser_state.position]

accepts(p::ParserPuppet) = begin
    accepts = Set()
    for t in keys(p.choices())
        if isupper(t)
            new_puppet = copy(p)
            try
                feed_token(new_puppet,Token(t,''))
            catch e
                if e isa UnexpectedToken
                    continue
                else
                    push!(accepts,t)
                end
            end
        end
    end
    return accepts
end

resume_parse(p::ParserPuppet) = begin
    p.parser.parse_from_state(p.parser_state)
end

# TODO: make the immutableparserpuppet work
feed_token(i::ImmutableParserPuppet) = begin
    c = copy(i)
end

