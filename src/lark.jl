
const _lark_defaults = Dict([
        "debug"=> false,
        "keep_all_tokens"=> false,
        "postlex"=> nothing,
        "parser"=> "lalr",
        "lexer"=> "auto",
        "transformer"=> nothing,
        "start"=> "start",
        "priority"=> "auto",
        "ambiguity"=> "auto",
        "propagate_positions"=> false,
        "lexer_callbacks"=> Dict(),
        "maybe_placeholders"=> false,
        "edit_terminals"=> nothing,
        "g_regex_flags"=> 0,
        "import_paths"=> [],
        "source_path"=> nothing,
    ])

"""
    LarkOptions(options_dict)

A LarkOptions instance contains options for running Lark. Options can be accessed both
as a traditional dictionary and as properties using dot notation. The possible options are
described under the Lark constructor.

"""
struct LarkOptions
    options_dict::Dict
    LarkOptions(o::Dict) = begin
        options = Dict()
        for (name,default) in _lark_defaults
            if name in keys(o)
                value = pop!(o,name)
                if default isa Bool
                    value = Bool(value)
                end
            else value = default
            end
            options[name] = value
        end
        if typeof(options["start"]) <: AbstractString
            options["start"] = [options["start"]]
        end
        if !(options["parser"] in ("earley", "lalr", "cyk", nothing))
            error("Option error: Parser must be earley, lalr, cyk or nothing, given $(options["parser"])")
        end
        if options["parser"] == "earley" && !(options["transformer"]==nothing)
            error("Cannot specify an embedded transformer when using the Earley algorithm." *
                  "Please use your transformer on the resulting parse tree, or use a different algorithm (i.e. lalr)")
        end
        new(options)
    end
end

Base.getproperty(lo::LarkOptions,name::Symbol) = begin
    pdict = getfield(lo,:options_dict)
    if String(name) in keys(pdict)
        pdict[String(name)]
    else
        getfield(lo,name)
    end
end

Base.setproperty!(lo::LarkOptions,name::Symbol,value) = begin
    pdict = getfield(lo,:options_dict)
    if String(name) in keys(pdict)
        pdict[String(name)] = value
    else
        throw(KeyError(name))
    end
end

struct Lark
    options::LarkOptions
    source_path
    source_grammar
    terminals
    rules
    ignore_tokens
    lexer_conf
    parser
end

const VALID_PRIORITY_OPTIONS = ("auto", "normal", "invert", nothing)
const VALID_AMBIGUITY_OPTIONS = ("auto", "resolve", "explicit", "forest")

"""
    Lark(grammar::String;options...)

Create a Lark parser based on the Lark grammar expressed in `grammar`. Options are as
follows:

  * `parser` Decides which parser engine to use, "earley" or "lalr". (Default: "lalr")
         Note: "lalr" requires a lexer. "earley" is not currently implemented.

  * `lexer` Decides whether or not to use a lexer stage

     + "standard": Use a standard lexer

     + "contextual": Stronger lexer (only works with parser="lalr")

     + "auto" (default): Choose for me based on grammar and parser

  * `transformer` - Applies the transformer to every parse tree

  * `debug` - Affects verbosity (default: False)

  * `keep_all_tokens` - Don't automagically remove "punctuation" tokens (default: False)

  * `postlex` - Lexer post-processing (Requires standard lexer. Default: None)

  * `start` - The start symbol (Default: start)

  * `propagate_positions` - Propagates [line, column, end line, end column] attributes into all tree branches.

  * `lexer_callbacks` - Dictionary of callbacks for the lexer. May alter tokens during lexing. Use with caution.
"""
Lark(grammar::String;options...) = begin
    source = "<string>"
    cache_file = "larkcache__$(hash(grammar)%(2^32))"
    Lark(grammar,Dict{String,Any}((String(k),v) for (k,v) in options),source,cache_file)
end

"""
    Lark(grammar::IOStream,source;options...)

Create a Lark parser based on the Lark grammar found by reading `grammar`, whose source
is recorded as `source`. See above for options.
"""
Lark(grammar::IOStream,source;options...) = begin
    cache_file = "larkcache_$(basename(source))"
    textgrammar = read(grammar,String)
    Lark(textgrammar,Dict{String,Any}((String(k),v) for (k,v) in options),source,cache_file)
end

"""
       Lark(grammar::String,loptions,source,cache_file)

Create a Lark parser based on the Lark grammar contained in `grammar`, using options
found in dictionary `loptions`, recording the source as `source` and storing the parser information
in `cache_file`. `cache_file` is currently ignored. 
"""
Lark(grammar::String,loptions,source,cache_file) = begin
    options = LarkOptions(loptions)
    if options.lexer == "auto"
        if options.parser == "lalr"
            options.lexer = "contextual"
        elseif options.parser == "earley"
            options.lexer = "dynamic"
        elseif options.parser == "cyk"
            options.lexer = "standard"
        else
            @assert !options.parser
        end
    end
    
    lexer = options.lexer
    @assert lexer in ("standard", "contextual", "dynamic", "dynamic_complete") || (lexer isa Lexer)
    
    if options.ambiguity == "auto"
        if options.parser == "earley"
            options.ambiguity = "resolve"
        end
    else
        disambig_parsers = ["earley", "cyk"]
        @assert (options.parser in disambig_parsers) 
        "Only $(join(", ",disambig_parser)) supports disambiguation right now"
    end

    if !(options.priority in VALID_PRIORITY_OPTIONS)
        throw(ValueError("invalid priority option: $(options.priority). Must be one of $VALID_PRIORITY_OPTIONS"))
    end

    if !(options.ambiguity in VALID_AMBIGUITY_OPTIONS)
        throw(ValueError("invalid ambiguity option: $(options.ambiguity). Must be one of $VALID_AMBIGUITY_OPTIONS"))
    end

    grammar = load_grammar(grammar, source, options.import_paths, options.keep_all_tokens)
    
    if options.postlex !== nothing
        terminals_to_keep = Set(options.postlex.always_accept)
    else
        terminals_to_keep = Set()
    end
    
    # Compile the EBNF grammar into BNF
    try
        terminals, rules, ignore_tokens = compile(grammar,options.start,terminals_to_keep)
        if options.edit_terminals !== nothing
            for t in terminals
                options.edit_terminals(t)
            end
        end
        _terminals_dict = Dict([(t.name => t) for t in terminals])

        if options.priority == "invert"
            for rule in rules
                if rule.options.priority !== nothing
                    rule.options.priority = -rule.options.priority
                end
            end
        elseif options.priority ===  nothing
            for rule in rules
                if rule.options.priority !== nothing
                    rule.options.priority = nothing
                end
            end
        end
        lexer_callbacks = Dict() #options.transformer !== nothing ? _get_lexer_callbacks(options.transformer,terminals) : Dict()
        merge!(lexer_callbacks,options.lexer_callbacks)
        lexer_conf = LexerConf(terminals,ignore=ignore_tokens,postlex=options.postlex,
                               callbacks=lexer_callbacks,
                               g_regex_flags=options.g_regex_flags)
        ## Note that lexer is not preserved so will do nothing.
        if options.parser !== nothing
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

#TODO: integrate transformer calls into create_callback
_prepare_callbacks(options,rules) = begin
    parser_class = get_frontend(options.parser,options.lexer)
    _callbacks = nothing
    if options.ambiguity != "forest"
        _parse_tree_builder = ParseTreeBuilder(rules,
                                               propagate_positions = options.propagate_positions,
                                               ambiguous = options.parser != "lalr" && options.ambiguity=="explicit",
                                               maybe_placeholders=options.maybe_placeholders)
        _callbacks = create_callback(_parse_tree_builder) #,transformer=options.transformer)
    end
    return parser_class,_callbacks
end

_build_parser(options,rules,lexer_conf) = begin
    parser_class,callbacks = _prepare_callbacks(options,rules)
    parser_conf = ParserConf(rules,callbacks,options.start)
    return parser_class(lexer_conf,parser_conf,options=options)
end

"""
    open(grammar_filename;rel_to=nothing,options...)
    
Open `grammar_filename` and read in a Lark grammar, returning a Lark parser. If
`rel_to` is not `nothing`, `grammar_filename` is relative to `rel_to`. Options
are the same as for [`Lark(grammar::String;options...)`](@ref).
"""
open(grammar_filename;rel_to=nothing,options...) = begin
    if rel_to != nothing
        basepath = dirname(rel_to)
        grammar_filename = joinpath(basepath,grammar_filename)
    end
    Lark(Base.open(grammar_filename),grammar_filename,options...)
end

show(io::IOStream,l::Lark) = begin
    print(io,"Lark(open($(l.source)), parser = $(l.options["parser"]), lexer = $(l.options["lexer"]), ...)")
end

lex(l::Lark, text) = begin
    stream = lex(l.lexer,text)
    if l.options.postlex
        return process(l.options.postlex,stream)
    end
    return stream
end

get_terminal(l::Lark,name) = l._terminals_dict[name]

"""
    parse(l::Lark,text;start=nothing,on_error=nothing)

Parse `text` using parser `l`, returning a parse tree. If `start` is
not nothing, it identifies the start symbol. `on_error` is currently
ignored.

If `text` does not conform to the grammar, either 
[`UnexpectedToken`](@ref) or [`UnexpectedCharacters`](@ref) 
is raised.
"""
parse(l::Lark,text;start=nothing,on_error=nothing) = begin
    try
        parse(l.parser,text,start=start)
    catch ex
        if ex isa TaskFailedException
            rethrow(ex.task.exception)
        else
            rethrow(ex)
        end
    end
    # TODO Add lark 0.11.0 fancy error handling
end

