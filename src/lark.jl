
"""
        parser - Decides which parser engine to use, "earley" or "lalr". (Default: "earley")
                 Note: "lalr" requires a lexer

        lexer - Decides whether or not to use a lexer stage
            "standard": Use a standard lexer
            "contextual": Stronger lexer (only works with parser="lalr")
            "dynamic": Flexible and powerful (only with parser="earley")
            "dynamic_complete": Same as dynamic, but tries *every* variation
                                of tokenizing possible. (only with parser="earley")
            "auto" (default): Choose for me based on grammar and parser

        ambiguity - Decides how to handle ambiguity in the parse. Only relevant if parser="earley"
            "resolve": The parser will automatically choose the simplest derivation
                       (it chooses consistently: greedy for tokens, non-greedy for rules)
            "explicit": The parser will return all derivations wrapped in "_ambig" tree nodes (i.e. a forest).

        transformer - Applies the transformer to every parse tree
        debug - Affects verbosity (default: False)
        keep_all_tokens - Don't automagically remove "punctuation" tokens (default: False)
        cache_grammar - Cache the Lark grammar (Default: False)
        postlex - Lexer post-processing (Requires standard lexer. Default: None)
        start - The start symbol (Default: start)
        profile - Measure run-time usage in Lark. Read results from the profiler proprety (Default: False)
        propagate_positions - Propagates [line, column, end_line, end_column] attributes into all tree branches.
        lexer_callbacks - Dictionary of callbacks for the lexer. May alter tokens during lexing. Use with caution.
"""

check_options(od) = begin
    get!(od,"debug",false)
    get!(od,"keep_all_tokens",false)
    get!(od,"cache_grammar", false)
    get!(od,"postlex", nothing)
    get!(od,"parser", "earley")
    get!(od,"lexer", "auto")
    get!(od,"transformer", nothing)
    get!(od,"start", "start")
    get!(od,"profile", false)
    get!(od,"ambiguity", "auto")
    get!(od,"propagate_positions", false)
    get!(od,"earley__predict_all", false)
    get!(od,"lexer_callbacks", Dict())

    if !(od["parser"] in ("earley", "lalr", "cyk", nothing))
        error("Option error: Parser must be earley, lalr, cyk or nothing")
    end
    if od["parser"] == "earley" && !(od["transformer"]==nothing)
        error("Cannot specify an embedded transformer when using the Earley algorithm." *
              "Please use your transformer on the resulting parse tree, or use a different algorithm (i.e. lalr)")
    end
end

struct Lark
    options::Dict
    source
    grammar
    terminals
    rules
    ignore_tokens
    lexer_conf
    parser
end

Lark(grammar::IOStream;options...) = begin
    source = grammar.name
    cache_file = "larkcache_$(basename(source))"
    textgrammar = read(grammar)
    Lark(textgrammar,Dict{String,Any}((String(k),v) for (k,v) in options),source,cache_file)
end

Lark(grammar::String;options...) = begin
    source = "<string>"
    cache_file = "larkcache__$(hash(grammar)%(2^32))"
    Lark(grammar,Dict{String,Any}((String(k),v) for (k,v) in options),source,cache_file)
end

Lark(grammar::String,options,source,cache_file) = begin
    if isempty(options)
        options = Dict{String,Any}()
    end
    check_options(options)
    if options["lexer"] == "auto"
        if options["parser"] == "lalr"
            options["lexer"] = "contextual"
        elseif options["parser"] == "earley"
            options["lexer"] = "dynamic"
        elseif options["parser"] == "cyk"
            options["lexer"] = "standard"
        else
            @assert !options["parser"]
        end
    end
    
    lexer = options["lexer"]
    @assert lexer in ("standard", "contextual", "dynamic", "dynamic_complete") || (lexer isa Lexer)
    
    if options["ambiguity"] == "auto"
        if options["parser"] == "earley"
            options["ambiguity"] = "resolve"
        end
    else
        disambig_parsers = ["earley", "cyk"]
        @assert (options["parser"] in disambig_parsers) 
        "Only $(join(", ",disambig_parser)) supports disambiguation right now"
    end
    @assert options["ambiguity"] in ("resolve", "explicit", "auto", "resolve__antiscore_sum")
    
    grammar = load_grammar(grammar, grammar_name=source)
    
    # Compile the EBNF grammar into BNF
    try
        terminals, rules, ignore_tokens = compile(grammar)
        lexer_conf = LexerConf(terminals,ignore_tokens,options["postlex"],options["lexer_callbacks"])
        if options["parser"] != nothing
            parser = _build_parser(options,rules,lexer_conf)
        elseif lexer != nothing
            lexer = TraditionalLexer(lexer_conf.tokens,lexer_conf.ignore,lexer_conf.callbacks)
        end
        Lark(options,source,grammar,terminals,rules,ignore_tokens,lexer_conf,parser)
    catch ex
        if ex isa TaskFailedException
            rethrow(ex.task.exception)
        else
            rethrow(ex)
        end
    end
end

_build_parser(options,rules,lexer_conf) = begin
    parser_class = get_frontend(options["parser"],options["lexer"])
    _parse_tree_builder = ParseTreeBuilder(rules,
                                           propagate_positions = options["propagate_positions"],
                                           keep_all_tokens=options["keep_all_tokens"],
                                           ambiguous = options["parser"] != "lalr")
    callback = create_callback(_parse_tree_builder,transformer=options["transformer"])
    parser_conf = ParserConf(rules,callback,options["start"])
    return parser_class(lexer_conf,parser_conf,options=options)
end

open(grammar_filename;rel_to=nothing,options...) = begin
    if rel_to != nothing
        basepath = dirname(rel_to)
        grammar_filename = join(basepath,grammar_filename)
    end
    Lark(grammar_filename,options...)
end

show(io::IOStream,l::Lark) = begin
    print(io,"Lark(open($(l.source)), parser = $(l.options["parser"]), lexer = $(l.options["lexer"]), ...)")
end

lex(l::Lark, text) = begin
    stream = lex(l.lexer,text)
    if l.options["postlex"]
        return process(l.options["postlex"],stream)
    end
    return stream
end

parse(l::Lark,text) = begin
    try
        parse(l.parser,text)
    catch ex
        if ex isa TaskFailedException
            rethrow(ex.task.exception)
        else
            rethrow(ex)
        end
    end
end

