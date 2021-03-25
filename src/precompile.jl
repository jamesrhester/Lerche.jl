# Use
#    @warnpcfail precompile(args...)
# if you want to be warned when a precompile directive fails
macro warnpcfail(ex::Expr)
    modl = __module__
    file = __source__.file === nothing ? "?" : String(__source__.file)
    line = __source__.line
    quote
        $(esc(ex)) || @warn """precompile directive
     $($(Expr(:quote, ex)))
 failed. Please report an issue in $($modl) (after checking for duplicates) or remove this directive.""" _file=$file _line=$line
    end
end


const __bodyfunction__ = Dict{Method,Any}()

# Find keyword "body functions" (the function that contains the body
# as written by the developer, called after all missing keyword-arguments
# have been assigned values), in a manner that doesn't depend on
# gensymmed names.
# `mnokw` is the method that gets called when you invoke it without
# supplying any keywords.
function __lookup_kwbody__(mnokw::Method)
    function getsym(arg)
        isa(arg, Symbol) && return arg
        @assert isa(arg, GlobalRef)
        return arg.name
    end

    f = get(__bodyfunction__, mnokw, nothing)
    if f === nothing
        fmod = mnokw.module
        # The lowered code for `mnokw` should look like
        #   %1 = mkw(kwvalues..., #self#, args...)
        #        return %1
        # where `mkw` is the name of the "active" keyword body-function.
        ast = Base.uncompressed_ast(mnokw)
        if isa(ast, Core.CodeInfo) && length(ast.code) >= 2
            callexpr = ast.code[end-1]
            if isa(callexpr, Expr) && callexpr.head == :call
                fsym = callexpr.args[1]
                if isa(fsym, Symbol)
                    f = getfield(fmod, fsym)
                elseif isa(fsym, GlobalRef)
                    if fsym.mod === Core && fsym.name === :_apply
                        f = getfield(mnokw.module, getsym(callexpr.args[2]))
                    elseif fsym.mod === Core && fsym.name === :_apply_iterate
                        f = getfield(mnokw.module, getsym(callexpr.args[3]))
                    else
                        f = getfield(fsym.mod, fsym.name)
                    end
                else
                    f = missing
                end
            else
                f = missing
            end
        else
            f = missing
        end
        __bodyfunction__[mnokw] = f
    end
    return f
end

function _precompile_()
    ccall(:jl_generating_output, Cint, ()) == 1 || return nothing
    Base.precompile(Tuple{Core.kwftype(typeof(Type)),NamedTuple{(:order, :alias, :options),Tuple{Int64,String,RuleOptions}},Type{Rule},NonTerminal,Array{Any,1}})
    Base.precompile(Tuple{Type{PrepareAnonTerminals},Array{TerminalDef,1},Set{String},Dict{Any,Any},Int64,Nothing})
    Base.precompile(Tuple{Type{TraditionalLexer},LexerConf})
    Base.precompile(Tuple{typeof(_call_userfunc),SimplifyRule_Visitor,Tree})
    Base.precompile(Tuple{typeof(_create_unless),Array{TerminalDef,1},String})
    Base.precompile(Tuple{typeof(_find_used_symbols),Tree})
    Base.precompile(Tuple{typeof(_init_builders),Array{Rule,1},Bool,Bool,Bool})
    Base.precompile(Tuple{typeof(_transform_children),ApplyTemplates,Array{Any,1}})
    Base.precompile(Tuple{typeof(_transform_children),EBNF_to_BNF,Array{Any,1}})
    Base.precompile(Tuple{typeof(_transform_children),PrepareLiterals,Array{Any,1}})
    Base.precompile(Tuple{typeof(_transform_children),PrepareSymbols,Array{Any,1}})
    Base.precompile(Tuple{typeof(_transform_children),RuleTreeToText,Array{Any,1}})
    Base.precompile(Tuple{typeof(calculate_sets),Array{Rule,1}})
    Base.precompile(Tuple{typeof(compute_lalr1_states),LALR_Analyzer})
    Base.precompile(Tuple{typeof(compute_reads_relations),LALR_Analyzer})
    Base.precompile(Tuple{typeof(expand_rule),LALR_Analyzer,NonTerminal,Nothing})
    Base.precompile(Tuple{typeof(from_ParseTable),ParseTable})
    Base.precompile(Tuple{typeof(maybe_create_child_filter),Array{LarkSymbol,1},Bool,Bool,Nothing})
    Base.precompile(Tuple{typeof(token_func),PrepareGrammar,Val{Symbol("\"234\"")},Token})
    Base.precompile(Tuple{typeof(transformer_func),ApplyTemplates,Val{:template_usage},Meta,Array{Any,1}})
    Base.precompile(Tuple{typeof(transformer_func),EBNF_to_BNF,Val{:expr},Tree,Token})
    Base.precompile(Tuple{typeof(transformer_func),TerminalTreeToPattern,Val{:expansions},Meta,Array{Any,1}})
    Base.precompile(Tuple{typeof(traverse),Tuple{LR0ItemSet,NonTerminal},Array{Any,1},Dict{Any,Int64},Array{Any,1},DefaultDict{Any,Set},DefaultDict{Any,Set},Dict{Any,Any}})
    isdefined(Lerche, Symbol("#128#137")) && Base.precompile(Tuple{getfield(Lerche, Symbol("#128#137")),Pair{String,Rule}})
    isdefined(Lerche, Symbol("#129#138")) && Base.precompile(Tuple{getfield(Lerche, Symbol("#129#138")),String})
    isdefined(Lerche, Symbol("#141#143")) && Base.precompile(Tuple{getfield(Lerche, Symbol("#141#143")),Channel{Any}})
    isdefined(Lerche, Symbol("#153#158")) && Base.precompile(Tuple{getfield(Lerche, Symbol("#153#158")),Channel{Any}})
    isdefined(Lerche, Symbol("#2#3")) && Base.precompile(Tuple{getfield(Lerche, Symbol("#2#3")),Channel{Any}})
    isdefined(Lerche, Symbol("#52#53")) && Base.precompile(Tuple{getfield(Lerche, Symbol("#52#53")),TerminalDef})
    isdefined(Lerche, Symbol("#75#76")) && Base.precompile(Tuple{getfield(Lerche, Symbol("#75#76")),Array{Any,1}})
    isdefined(Lerche, Symbol("#81#82")) && Base.precompile(Tuple{getfield(Lerche, Symbol("#81#82")),Array{Any,1}})
    isdefined(Lerche, Symbol("#get_namespace_name#257")) && Base.precompile(Tuple{getfield(Lerche, Symbol("#get_namespace_name#257")),Token,Nothing})
    let fbody = try __lookup_kwbody__(which(create_callback, (ParseTreeBuilder,))) catch missing end
        if !ismissing(fbody)
            precompile(fbody, (Nothing,typeof(create_callback),ParseTreeBuilder,))
        end
    end
    let fbody = try __lookup_kwbody__(which(init_analyser!, (LALR_Analyzer,ParserConf,))) catch missing end
        if !ismissing(fbody)
            precompile(fbody, (Bool,typeof(init_analyser!),LALR_Analyzer,ParserConf,))
        end
    end
end
