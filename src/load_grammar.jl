const EXT = ".lark"

const IMPORT_PATHS = [joinpath(@__DIR__,"grammars")]

const _RE_FLAGS = "imslux"

const _EMPTY = Terminal("__empty__")

# is_terminal(sym) = isupper(sym)

const _TERMINAL_NAMES = Dict(
    "." => "DOT",
    "," => "COMMA",
    ":" => "COLON",
    ";" => "SEMICOLON",
    "+" => "PLUS",
    "-" => "MINUS",
    "*" => "STAR",
    "/" => "SLASH",
    "\\" => "BACKSLASH",
    "|" => "VBAR",
    "?" => "QMARK",
    "!" => "BANG",
    "@" => "AT",
    "#" => "HASH",
    "\$" => "DOLLAR",
    "%" => "PERCENT",
    "^" => "CIRCUMFLEX",
    "&" => "AMPERSAND",
    "_" => "UNDERSCORE",
    "<" => "LESSTHAN",
    ">" => "MORETHAN",
    "=" => "EQUAL",
    "\"" => "DBLQUOTE",
    "'" => "QUOTE",
    "`" => "BACKQUOTE",
    "~" => "TILDE",
    "(" => "LPAR",
    ")" => "RPAR",
    "{" => "LBRACE",
    "}" => "RBRACE",
    "[" => "LSQB",
    "]" => "RSQB",
    "\n" => "NEWLINE",
    "\r\n" => "CRLF",
    "\t" => "TAB",
    " " => "SPACE",
)

# Grammar Parser
const TERMINALS = Dict(
    "_LPAR"=> raw"\(",
    "_RPAR"=> raw"\)",
    "_LBRA"=> raw"\[",
    "_RBRA"=> raw"\]",
    "_LBRACE"=> raw"{",
    "_RBRACE"=> raw"}",
    "OP"=> "[+*]|[?](?![a-z])",
    "_COLON"=> ":",
    "_COMMA"=> ",",
    "_OR"=> raw"\|",
    "_DOT"=> raw"\.(?!\.)",
    "_DOTDOT"=> raw"\.\.",
    "TILDE"=> "~",
    "RULE"=> "!?[_?]?[a-z][_a-z0-9]*",
    "TERMINAL"=> "_?[A-Z][_A-Z0-9]*",
    "STRING"=> raw""""(\\\\"|\\\\|[^"\n])*?"i?""",
    "REGEXP"=> raw"/(?!/)(\\/|\\\\|[^/])*?/[" * _RE_FLAGS * "]*",
    "_NL"=> raw"(\r?\n)+\s*",
    "WS"=> raw"[ \t]+",
    "COMMENT"=> raw"\s*//[^\n]*",
    "_TO"=> "->",
    "_IGNORE"=> raw"%ignore",
    "_DECLARE"=> raw"%declare",
    "_IMPORT"=> raw"%import",
    "NUMBER"=> raw"[+-]?\d+",
)

const RULES = Dict(
    "start"=> ["_list"],
    "_list"=>  ["_item", "_list _item"],
    "_item"=>  ["rule", "term", "statement", "_NL"],

    "rule"=> ["RULE template_params _COLON expansions _NL",
              "RULE template_params _DOT NUMBER _COLON expansions _NL"],
    "template_params"=> ["_LBRACE _template_params _RBRACE",
                     ""],
    "_template_params" => ["RULE",
                       "_template_params _COMMA RULE"],
    "expansions"=> ["alias",
                   "expansions _OR alias",
                   "expansions _NL _OR alias"],

    "?alias"=>     ["expansion _TO RULE", "expansion"],
    "expansion"=> ["_expansion"],

    "_expansion"=> ["", "_expansion expr"],

    "?expr"=> ["atom",
              "atom OP",
              "atom TILDE NUMBER",
              "atom TILDE NUMBER _DOTDOT NUMBER",
              ],

    "?atom"=> ["_LPAR expansions _RPAR",
              "maybe",
              "value"],

    "value"=> ["terminal",
              "nonterminal",
              "literal",
               "range",
               "template_usage"],

    "terminal"=> ["TERMINAL"],
    "nonterminal"=> ["RULE"],

    "?name"=> ["RULE", "TERMINAL"],

    "maybe"=> ["_LBRA expansions _RBRA"],
    "range"=> ["STRING _DOTDOT STRING"],
    "template_usage"=> ["RULE _LBRACE _template_args _RBRACE"],
    "_template_args"=> ["value",
                       "_template_args _COMMA value"],

    "term"=> ["TERMINAL _COLON expansions _NL",
              "TERMINAL _DOT NUMBER _COLON expansions _NL"],
    "statement"=> ["ignore", "import", "declare"],
    "ignore"=> ["_IGNORE expansions _NL"],
    "declare"=> ["_DECLARE _declare_args _NL"],
    "import"=> ["_IMPORT _import_path _NL",
               "_IMPORT _import_path _LPAR name_list _RPAR _NL",
               "_IMPORT _import_path _TO name _NL"],

    "_import_path"=> ["import_lib", "import_rel"],
    "import_lib"=> ["_import_args"],
    "import_rel"=> ["_DOT _import_args"],
    "_import_args"=> ["name", "_import_args _DOT name"],

    "name_list"=> ["_name_list"],
    "_name_list"=> ["name", "_name_list _COMMA name"],

    "_declare_args"=> ["name", "_declare_args name"],
    "literal"=> ["REGEXP", "STRING"],
)


mutable struct EBNF_to_BNF <: Transformer_InPlace
    new_rules::Array{Tuple{String,Tree,Any}}
    rules_by_expr::Dict{Union{String,LarkSymbol,Tree},NonTerminal}
    prefix::String
    i::Int
    rule_options::Union{Nothing,RuleOptions}
end

EBNF_to_BNF() = EBNF_to_BNF([],Dict(),"anon",0,nothing)

_add_recurse_rule(etb::EBNF_to_BNF,type_,expr) = begin
    if expr in keys(etb.rules_by_expr)
        return etb.rules_by_expr[expr]
    end
    new_name = "__$(etb.prefix)_$(type_)_$(etb.i)"
    etb.i += 1
    t = NonTerminal(new_name)
    tree = Tree("expansions",[Tree("expansion",[expr]),Tree("expansion",[t,expr])])
    push!(etb.new_rules,(new_name,tree,etb.rule_options))
    etb.rules_by_expr[expr] = t
    return t
end

@inline_rule expr(etb::EBNF_to_BNF,rule,op,args...) = begin
    if op.value == "?"
        return Tree("expansions",[rule,Tree("expansion",[])])
    elseif op.value == "+"
        return _add_recurse_rule(etb,"plus",rule)
    elseif op.value == "*"
        new_name = _add_recurse_rule(etb,"star",rule)
        return Tree("expansions",[new_name, Tree("expansion",[])])
    elseif op.value == "~"
        if length(args) == 1
            mn = mx = Base.parse(Int64,args[1])
        else
            mn,mx = Base.parse.(Int,args)
            if mx < mn || mn < 0
                throw(GrammarError("Bad range for $rule ($mn..$mx isn't allowed)"))
            end
        end
        return Tree("expansions", [Tree("expansion",fill(rule,n)) for n in mn:mx])
    end
end

@inline_rule maybe(etb::EBNF_to_BNF,rule) = begin
    keep_all_tokens = etb.rule_options !== nothing && etb.rule_options.keep_all_tokens
    will_not_get_removed(sym) = begin
        if sym isa NonTerminal
            return !(sym.name[1] == '_')
        end
        if sym isa Terminal
            return keep_all_tokens || !sym.filter_out
        end
        @assert false
    end
    
    if any(scan_values(rule,will_not_get_removed))
        empty = _EMPTY
    else
        empty = Tree("expansion",[])
    end
    return Tree("expansions",[rule,empty])
end


struct SimplifyRule_Visitor <: Visitor end

# So what this does is to find children with the same data type as the parent,
# and bring them to the same level of the tree
_flatten!(tree::Tree) = begin
    while true
        to_expand = [i for (i,child) in enumerate(tree.children) if child isa Tree && child.data == tree.data]
        if length(to_expand) == 0 break end
        expand_kids_by_index!(tree,to_expand)
    end
end

# Changes the argument but we can't add exclamation marks or it will
# spoil the link between rule and function
@rule expansion(srv::SimplifyRule_Visitor,tree::Tree) = begin
    _flatten!(tree)
    for (i,child) in enumerate(tree.children)
        if child isa Tree && child.data == "expansions"
            tree.data = "expansions"
            new_children = []
            for option in unique(child.children)
                temp_tree = Tree("expansion", [if i==j 
                                           option
                                           else
                                           other
                                           end
                                               for (j,other) in enumerate(tree.children)])
                visitation = visit(srv,temp_tree)
                new_children = push!(new_children,visitation)
            end
            tree.children = new_children
            _flatten!(tree)
            break
        end
    end
    #println("After expansion:\n $(pretty(tree))")
end

@rule alias(srv::SimplifyRule_Visitor,tree::Tree) = begin
    rule,alias_name = tree.children
    if rule.data == "expansions"
        aliases = []
        for child in tree.children[1].children
            push!(aliases,Tree("alias",[child,alias_name]))
        end
        tree.data = "expansions"
        tree.children = aliases
    end
end

@rule expansions(srv::SimplifyRule_Visitor,tree) = begin
    _flatten!(tree)
    unique!(tree.children)
end

struct RuleTreeToText <: Transformer end

@rule expansions(rtt::RuleTreeToText,x) = begin
    return x
end

@rule expansion(rtt::RuleTreeToText,symbols) = (symbols,nothing)

@rule alias(rtt::RuleTreeToText,x) = begin
    (expansion, _alias), alias = x
    @assert _alias == nothing
    return expansion, alias.value
end

struct CanonizeTree <: Transformer_InPlace end

@inline_rule maybe(ct::CanonizeTree,expr) = Tree("expr",[expr, Token("OP","?",pos_in_stream=-1)])

@inline_rule tokenmods(ct::CanonizeTree,args...) = begin
    if length(args) == 1
        return Array(args)
    end
    tokenmods, value = args   #What does this mean people?
    return push!(tokenmods,value)
end

"""
Create a unique list of anonymous terminals. Attempt to give 
meaningful names to them when we add them
"""
mutable struct PrepareAnonTerminals <: Transformer_InPlace
    terminals
    term_set::Set
    term_reverse::Dict{Pattern,TerminalDef}
    i
    rule_options
end

PrepareAnonTerminals(terminals) = PrepareAnonTerminals(terminals,Set([td.name for td in terminals]),
                                                       Dict([td.pattern => td for td in terminals]),
                                                       0,nothing)

@inline_rule pattern(panon::PrepareAnonTerminals,p) = begin
    value = p.value
    if p in collect(keys(panon.term_reverse)) && p.flags != panon.term_reverse[p].pattern.flags
        throw(GrammarError("Conflicting flags for the same terminal: $p"))
    end
    term_name = nothing
    if p isa PatternStr
        try
            term_name = panon.term_reverse[p].name
        catch e
            if e isa KeyError
                try
                    term_name = _TERMINAL_NAMES[value]
                catch f
                    if f isa KeyError
                        # following test to make sure the group name in regular expression is allowed
                        if all(x->isletter(x)||isnumeric(x),value) && isletter(value[1]) && !(uppercase(value) in panon.term_set)
                            term_name = uppercase(value)
                        end
                    else
                        rethrow(f)
                    end
                end
                if term_name in panon.term_set
                    term_name = nothing
                end
            else   #not a key error
                rethrow(e)
            end
        end
    elseif p isa PatternRE
        if p in keys(panon.term_reverse)
            term_name = panon.term_reverse[p].name
        end
    else
        @assert false p
    end

    if term_name == nothing
        term_name = "__ANON_$(panon.i)"
        panon.i += 1
    end

    if !(term_name in panon.term_set)
        @assert !(p in keys(panon.term_reverse))
        push!(panon.term_set,term_name)
        termdef = TerminalDef(term_name,p)
        panon.term_reverse[p] = termdef
        push!(panon.terminals,termdef)
    end

    filter_out = panon.rule_options !== nothing && panon.rule_options.keep_all_tokens ? false : p isa PatternStr
    return Terminal(term_name,filter_out = filter_out)
end

mutable struct _ReplaceSymbols <: Transformer_InPlace
    names::Dict
end

_ReplaceSymbols() = _ReplaceSymbols(Dict())

@rule value(rs::_ReplaceSymbols,c) = begin
    if length(c) == 1 && c[1] isa Token && c[1].value in keys(rs.names)
        return rs.names[c[1].value]
    end
    return __default__(rs,"value",c,nothing)
end

@rule template_usage(rs::_ReplaceSymbols,c) = begin
    if c[1] in keys(rs.names)
        return __default__(rs,"template_usage", vcat([rs.names[c[1]].name], c[2:end]),nothing)
    end
    return __default__(rs,"template_usage",c,nothing)
end

"""Apply the templates, creating new rules that represent the used templates"""
mutable struct ApplyTemplates <: Transformer_InPlace
    rule_defs
    replacer::_ReplaceSymbols
    created_templates
end

ApplyTemplates(rule_defs) = ApplyTemplates(rule_defs,_ReplaceSymbols(),Set())

@rule template_usage(at::ApplyTemplates, c) = begin
    name = c[1]
    args = c[2:end]
    result_name = "$name{$(join(args,","))}"
    if !(result_name in at.created_templates)
        push!(at.created_templates,result_name)
        (_n, params, tree, options) = first(t for t in at.rule_defs if t[1] == name)
        @assert length(params) == length(args) args
        result_tree = deepcopy(tree)
        at.replacer.names = Dict(zip(params, args))
        transform(at.replacer,result_tree)
        push!(at.rule_defs,(result_name, [], result_tree, deepcopy(options)))
    end
    return NonTerminal(result_name)
end

_rfind(s,choices) = begin
    biggest = -1
    for c in choices
        p = findlast("$c",s)
        if !isnothing(p)
            biggest = max(biggest,p[1])
        end
    end
    return biggest
end

#==
Fixing escaping.

The python code iterates over each character of the supplied string,
appending it to the output string.

If that character is a backslash, the next character is also added. If
this second character is also a backslash, 2 backslashes are inserted
before it. If the second letter is anything other than 'unftr', only a
single further backslash is inserted.

So a double backslash on input becomes 4 on output. An unrecognised
backslash combination becomes double backslash - character. A
recognised combination remains as-is.

Following these shenanigans, any quotes are escaped and the string
is evaluated as a unicode string literal.

After calling this routine in _literal_to_pattern, double backslashes
are removed for strings, but remain for regular expressions.

Julia does not eval the string. However, we still need to
be able to make substitutions for common escapes \ + n/f/t/r.

The point of this routine is to interpret the escapes correctly,
after they have been read in (not when they are read in). So the

==#
_fix_escaping(s) = begin
    w = ""
    i = iterate(s)
    while i != nothing
        n,state = i
        w *= n
        if n == '\\'
            n2,state = iterate(s,state)
            if n2 == '\\'
                w *= "\\\\"
            elseif !(occursin(n2,"unftr\""))
                w *= "\\"
            end
            w *= n2
        end
        i = iterate(s,state)
    end
    new_s = unescape_string(w)
    return new_s
end

## TODO write this properly
_literal_to_pattern(literal) = begin
    v = literal.value
    flag_start = _rfind(v, "/\"") + 1
    @assert flag_start > 0
    flags = v[flag_start:end]
    @assert all(f -> occursin(f, _RE_FLAGS), flags)
    if literal.type_ == "STRING" && occursin('\n',v)
        throw(GrammarError("You cannot put newlines in string literals"))
    end
    if literal.type_ == "REGEXP" && occursin('\n',v) && !(occursin('x',flags))
        throw(GrammarError("You can only use newlines in regular expressions with the `x` (verbose) flag"))
    end
    v = v[1:flag_start-1]
    @assert v[1] == v[end] && occursin(v[1], "\"/")
    x = v[2:prevind(v,end,1)]   #drop delimiters
    ### TODO fix escaping
    s = _fix_escaping(x)
    if literal.type_ == "STRING"
        s = replace(s,"\\\\"=>"\\")
        return PatternStr(s,flags)
    elseif literal.type_ == "REGEXP"
        return PatternRE(s,flags)
    else
        @assert false, "Invariant failed: literal.type_ not in ['String','Regexp']"
    end
end

struct PrepareLiterals <: Transformer_InPlace end

@inline_rule literal(pl::PrepareLiterals, literal) = Tree("pattern",[_literal_to_pattern(literal)])

@inline_rule range(pl::PrepareLiterals,start,rend) = begin
    @assert start.type_ == rend.type_ == "STRING"
    start = start.value[2:end-1]
    rend = rend.value[2:end-1]
    @assert length(start) == length(rend) == 1
    regexp = "[$start-$rend]"    ###Make sure _repr_ and _str_ work here
    return Tree("pattern",[PatternRE(regexp)])
end

_make_joined_pattern(regexp,flags_set) = begin
    flags = ()
    if length(flags_set) > 1
        throw(GrammarError("Lerche doesn't support joining terminals with conflicting flags"))
    elseif length(flags_set) == 1
        flags = first(flags_set)
    end
    return PatternRE(regexp,flags)
end

struct TerminalTreeToPattern <: Transformer end

@rule pattern(tttp::TerminalTreeToPattern, ps) = begin
    p = ps[1]
    return p
end

@rule expansion(tttp::TerminalTreeToPattern, items) = begin
    if length(items) == 1
        return items[1]
    end
    pattern = "$(join((to_regexp(i) for i in items),""))"
    return _make_joined_pattern(pattern,Set([i.flags for i in items]))
end

@rule expansions(tttp::TerminalTreeToPattern, exps) = begin
    if length(exps) == 1
        return exps[1]
    end
    pattern = "(?:$(join((to_regexp(i) for i in exps),"|")))"
    return _make_joined_pattern(pattern, Set([i.flags for i in exps]))
end

@rule expr(tttp::TerminalTreeToPattern,args) = begin
    inner,op = args[1:2]
    if op == "~"
        if length(args) == 3
            op = "{$(Base.parse(Int64,args[3]))}"
        else
            mn,mx = Base.parse.(Int64,args[3:end])
            if mx < mn
                throw(GrammarError("Bad range for $inner ($mn..$mx isn't allowed)"))
            end
            op = "{$mn,$mx}"
        end
    else
        @assert length(args) == 2
        op = op.value
    end
    f = PatternRE("(?:$(to_regexp(inner)))$op",inner.flags)
    return f
end

@rule alias(tttp::TerminalTreeToPattern,t) = throw(GrammarError("Aliasing not allowed in terminals (You used -> in the wrong place)"))

@rule value(tttp::TerminalTreeToPattern,v) = v[1]

struct PrepareSymbols <: Transformer_InPlace end

@rule value(ps::PrepareSymbols,v) = begin
    v = v[1]
    if v isa Tree
        return v
    elseif v.type_ == "RULE"
        return NonTerminal(v.value)
    elseif v.type_ == "TERMINAL"
        return Terminal(v.value, filter_out = startswith(v.value,"_"))
    end
    @assert false
end

_choice_of_rules(rules) = Tree("expansions",[Tree("expansion",[Token("RULE",name)]) for name in rules])

nr_deepcopy_tree(t) = transform(Transformer_NonRecursive(),t)

# Note order of fields matches call order
mutable struct Grammar
    rule_defs::Array{Tuple{String,Array{Any,1},Tree,RuleOptions}}
    term_defs::Array{Tuple{String,Tuple{Tree,Int}}}
    ignore::Array{String}
end

compile(g::Grammar, start, terminals_to_keep) = begin
    # Deepcopy to allow multiple calling
    term_defs = deepcopy(g.term_defs)
    rule_defs = [(n,p,nr_deepcopy_tree(t),o) for (n,p,t,o) in g.rule_defs]

    # ===================
    #  Compile Terminals
    # ===================
    
    # Convert terminal-trees to strings/regexps
    for (name, (term_tree, priority)) in term_defs
        if term_tree == nothing  # Terminal added through %declare
            continue
        end
        expansions = collect(find_data(term_tree,"expansion"))
        if length(expansions) == 1 && length(expansions[1].children) == 0
            throw(GrammarError("Terminals cannot be empty ($name)"))
        end
    end
    transformer = PrepareLiterals() * TerminalTreeToPattern()
    terminals = [TerminalDef(name, transform(transformer,term_tree), priority)
                 for (name, (term_tree, priority)) in term_defs if term_tree!=[]]

    # =================
    #  Compile Rules
    # =================

    # 1. Pre-process terminals
    anon_tokens_transf = PrepareAnonTerminals(terminals)
    transformer = PrepareLiterals() * PrepareSymbols() * anon_tokens_transf   # Adds to terminals

    # 2. Inline Templates

    transformer *= ApplyTemplates(rule_defs)
    
    # 3. Convert EBNF to BNF (and apply step 1 and 2)
    ebnf_to_bnf = EBNF_to_BNF()
    rules = []
    i = 1
    while i <= length(rule_defs)  #instead of for loop as rule_defs might grow
        name, params, rule_tree, options = rule_defs[i]
        i +=1
        if length(params) != 0   #Don't transform templates
            continue
        end
        rule_options = options.keep_all_tokens ? RuleOptions(keep_all_tokens = true) : nothing
        ebnf_to_bnf.rule_options = rule_options  #drop check that options != nothing
        ebnf_to_bnf.prefix = name
        anon_tokens_transf.rule_options = rule_options
        tree = transform(transformer,rule_tree)
        push!(rules,(name, transform(ebnf_to_bnf,tree), options))
    end
   
    append!(rules, ebnf_to_bnf.new_rules)
    
    @assert length(rules) == length(Set(name for (name, _t, _o) in rules)) "Whoops, name collision"

    # 4. Compile tree to Rule objects
    rule_tree_to_text = RuleTreeToText()
    
    simplify_rule = SimplifyRule_Visitor()
    compiled_rules = Rule[]
    for (name, tree, options) in rules
        visit(simplify_rule,tree)
        #println("Tree after simplification:\n$tree")
        expansions = transform(rule_tree_to_text,tree)
        #println("Tree after expansions:\n$tree, returned $expansions")
        for (i,(expansion, alias)) in enumerate(expansions)
            if alias != nothing && first(name) =='_'
                throw(GrammarError("Rule $name is marked for expansion (it starts with an underscore) and isn't allowed to have aliases (alias=$alias)"))
            end
            empty_indices = [x==_EMPTY for x in expansion]
            if any(empty_indices)
                exp_options = options !== nothing ? copy(options) : RuleOptions()
                exp_options.empty_indices = empty_indices
                expansion = [x for x in expansion if x!=_EMPTY]
            else
                exp_options = options
            end
                
            @assert all(x-> x isa LarkSymbol,expansion) expansion

            rule = Rule(NonTerminal(name), expansion, order=i,alias=alias, options=exp_options)
            push!(compiled_rules,rule)
        end
    end

    if length(Set(compiled_rules)) != length(compiled_rules)
        duplicates = classify(compiled_rules, x->x)
        if dups in values(duplicates)
            if length(dups) > 1
                if dups[1].expansion != []
                    throw(error("Rules defined twice: $dups\n\n(Might happen due to colliding expansion of optionals: [] or ?)"))
                end
                @assert length(unique(((r.alias,r.order,r.options) for r in dups))) == length(dups)
            end
        end
        compiled_rules = collect(Set(compiled_rules))

    end

    # Filter out unused rules
    while true
        c = length(compiled_rules)
        used_rules = Set([s for r in compiled_rules
                            for s in r.expansion
                            if s isa NonTerminal
                            && s != r.origin])
        union!(used_rules, Set([NonTerminal(s) for s in start]))
        compiled_rules, unused = classify_bool(compiled_rules, r -> r.origin in used_rules)
        #println("$(typeof(compiled_rules))")
        for r in unused
            @debug "Unused rule: $r"
        end
        if length(compiled_rules) == c
            break
        end
    end
    
    # Filter out unused terminals
    used_terms = Set([t.name for r in compiled_rules
                      for t in r.expansion
                      if t isa Terminal])
    terminals, unused = classify_bool(terminals, t -> t.name in used_terms || t.name in g.ignore || t.name in terminals_to_keep)
    if length(unused) > 0
        @debug "Unused terminals: $([t.name for t in unused])"
    end
    
    return terminals, compiled_rules, g.ignore
end

const _imported_grammars = Dict()

    
import_from_grammar_into_namespace(grammar,namespace,aliases) = begin
    imported_terms = Dict(grammar.term_defs)
    imported_rules = Dict(n => (n,p,deepcopy(t),o) for (n,p,t,o) in grammar.rule_defs)
    term_defs = []
    rule_defs = []

    rule_dependencies(symbol) = begin
        if symbol.type_ != "RULE"
            return []
        end
        params = nothing
        tree = nothing
        try
            _, params, tree, _ = imported_rules[symbol]
        catch e
            if e isa KeyError
                throw(GrammarError("Missing symbol '$symbol' in grammar $namespace"))
            else
                rethrow(e)
            end
        end
        return setdiff(_find_used_symbols(tree),Set(params))
    end
    
    get_namespace_name(name,params) = begin
        if params !== nothing
            try
                return params[name]
            catch e
                if !(e isa KeyError)
                    rethrow(e)
                end
            end
        end
        try
            return aliases[name].value
        catch e
            if e isa KeyError
                if name[1] == '_'
                    return "_$(namespace)__$(name[2:end])"
                end
                return "$(namespace)__$name"
            end
            rethrow(e)
        end
    end

    to_import = collect(bfs(keys(aliases), rule_dependencies))
    for symbol in to_import
        if symbol.type_ == "TERMINAL"
            push!(term_defs,(get_namespace_name(symbol, nothing), imported_terms[symbol]))
        else
            @assert symbol.type_ == "RULE"
            _, params, tree, options = imported_rules[symbol]
            params_map = Dict([p => (p[1]!='_' ? "$(namespace)__$p" : "_$(namespace)__$p") for p in params])
            for t in tree
                for (i, c) in enumerate(t.children)
                    if c isa Token && c.type_ in ("RULE", "TERMINAL")
                        t.children[i] = Token(c.type_, get_namespace_name(c, params_map))
                    end
                end
            end
            params = [params_map[p] for p in params]
            push!(rule_defs,(get_namespace_name(symbol,params_map), params, tree, options))
        end
    end
    return term_defs, rule_defs
end

    
resolve_term_references(term_defs) = begin
    term_dict = Dict(k=>t for (k,(t,_p)) in term_defs)
    @assert length(term_dict) == length(term_defs) "Same name defined twice?"
    while true
        changed = false
        for (name, (token_tree, _p)) in term_defs
            if token_tree == nothing  # Terminal added through %declare
                continue
            end
            
            for exp in find_data(token_tree,"value")
                item ,= exp.children
                if item isa Token
                    if item.type_ == "RULE"
                        throw(GrammarError("Rules aren't allowed inside terminals ($item in $name)"))
                    end
                    if item.type_ == "TERMINAL"
                        term_value = term_dict[item]
                        @assert term_value != nothing
                        exp.children[1] = term_value
                        changed = true
                    end
                end
            end
        end
        
        if !changed
            break
        end
    end
    for (name, term) in term_dict
        if term != nothing    # Not just declared
            for child in term.children
                # Python version uses `id` here; we can't
                if term in child   #iterate for trees is iter_subtree
                    throw(GrammarError("Recursion in terminal '$name' (recursion is only allowed in rules, not terminals)"))
                end
            end
        end
    end
end


options_from_rule(name, params, x...) = begin
    if length(x) > 1
        priority, expansions = x
        priority = parse(Int,priority)
    else
        expansions = x[1]
        priority = nothing
    end
    params = params !== nothing ? [t.value for t in params.children] : []

    keep_all_tokens = startswith(name,"!")
    name = lstrip(name,'!')
    expand1 = startswith(name,"?")
    name = lstrip(name,'?')

    return name, params, expansions, RuleOptions(keep_all_tokens=keep_all_tokens, expand1=expand1, priority=priority, template_source=(params !== nothing ? name : nothing))
end

symbols_from_strcase(x) = begin
    if isupper(x)
        return Terminal(x, filter_out=first(x)=='_')
    else
        return NonTerminal(x)
    end
end

# emulate Python isupper, which returns true if all cased characters are uppercase.
# It returns false if there are no letters at all
# We interpret this as all letters are uppercase.  We differ from Python in that
# we allow strings with all non-letters to return "true".

isupper(s) = begin
    return all(y -> !isletter(y) || isuppercase(y),s)
end

struct PrepareGrammar <:Transformer_InPlace end

@inline_rule terminal(pg::PrepareGrammar, name) = name

@inline_rule nonterminal(pg::PrepareGrammar, name) = name

_find_used_symbols(tree) = begin
    @assert tree.data == "expansions"
    return Set([t for x in find_data(tree,"expansion") for t in scan_values(x,t -> t.type_ in ("RULE","TERMINAL"))])
end

const ERRORS = [
    ("Unclosed parenthesis", ["a: (\n"]),
    ("Unmatched closing parenthesis", ["a: )\n", "a: [)\n", "a: (]\n"]),
    ("Expecting rule or terminal definition (missing colon)", ["a\n", "A\n", "a->\n", "A->\n", "a A\n"]),
    ("Illegal name for rules or terminals", ["Aa:\n"]),
    ("Alias expects lowercase name", ["a: -> \"a\"\n"]),
    ("Unexpected colon", ["a::\n", "a: b:\n", "a: B:\n", "a: \"a\":\n"]),
    ("Misplaced operator", ["a: b??", "a: b(?)", "a:+\n", "a:?\n", "a:*\n", "a:|*\n"]),
    ("Expecting option ('|') or a new rule or terminal definition", ["a:a\n()\n"]),
    ("Terminal names cannot contain dots", ["A.B\n"]),
    ("%import expects a name", ["%import \"a\"\n"]),
    ("%ignore expects a value", ["%ignore %import\n"]),
]

"""
The GrammarLoader class is responsible for loading in the parser for the Lark EBNF itself.
"""
struct GrammarLoader
    parser
    global_keep_all_tokens
end

GrammarLoader(global_keep_all_tokens) = begin
    terminals = [TerminalDef(name, PatternRE(value)) for (name, value) in TERMINALS]
    rules = [options_from_rule(name, nothing, x) for (name, x) in  RULES]
    rules = [Rule(NonTerminal(r), symbols_from_strcase.(split(x)),order=i, alias=nothing, options=o) for (r, _p, xs, o) in rules for (i,x) in enumerate(xs)]
    #== debugging
    for r in rules
        println("Rules: $r")
        println("===")
    end
    ==#
    callback = create_callback(ParseTreeBuilder(rules))
    lexer_conf = LexerConf(terminals, ignore=["WS", "COMMENT"])
    parser_conf = ParserConf(rules, callback, ["start"])
    GrammarLoader(LALR_TraditionalLexer(lexer_conf, parser_conf),global_keep_all_tokens)
end

# Lark cycles through all paths, suppressing IOErrors, and if none are
# successful it deliberately opens a file to create a new IOError.
import_grammar(gl::GrammarLoader,grammar_path;base_path=nothing, import_paths=[]) = begin
    if !(grammar_path in keys(_imported_grammars))
        # import_paths take priority over base_path since they should handle relative imports and ignore everything else.
        to_try = vcat(import_paths, base_path !== nothing ? [base_path] : IMPORT_PATHS)
        text = nothing
        joined_path = nothing  #scope
        for source in to_try
            try
                joined_path = joinpath(source,grammar_path)
                text = read(joined_path,String)
            catch e
                if !(e isa SystemError)
                    rethrow(e)
                end
                continue
            end
            if isnothing(text) continue end
            grammar = load_grammar(gl, text, grammar_name=joined_path, import_paths=import_paths)
            _imported_grammars[grammar_path] = grammar
            break
        end
        if text === nothing   #failed, raise not found error
            Base.open(grammar_path,"r")
            @assert false
        end
    end
    return _imported_grammars[grammar_path]
end

"Parse grammar_text, verify, and create Grammar object. Display nice messages on error."
load_grammar(gl::GrammarLoader, grammar_text; grammar_name="<?>", import_paths=[]) = begin
    try
        tree = transform(CanonizeTree(),parse(gl.parser,grammar_text*"\n") )
    catch e
        if e isa UnexpectedCharacters
            context = get_context(e,grammar_text)
            throw(GrammarError("Unexpected input at line $(e.line) column $e.column in $(grammar_name): \n\n$context"))
            
        elseif e isa UnexpectedToken
            context = get_context(e,grammar_text)
            error = match_examples(e,partial(parse,gl.parser), ERRORS, use_accepts=true)
            if error != nothing
                throw(GrammarError("$error at line $(e.line) column $(e.column)\n\n$(context)"))
                
            elseif occursin("STRING",e.expected)
                throw(GrammarError("Expecting a value at line $(e.line) column $(e.column)\n\n$context"))
            end
        end
        rethrow(e)
    end

    tree = transform(PrepareGrammar(),tree)
    
    # Extract grammar items
    defs = classify(tree.children, key=c -> c.data, value=c -> c.children)
    term_defs = pop!(defs,"term", [])
    rule_defs = pop!(defs,"rule", [])
    statements = pop!(defs,"statement", [])
    @assert length(defs) == 0

    term_defs = [if length(td)==3 td else [td[1], 1, td[2]] end for td in term_defs]
    term_defs = [(name.value, (t, if !(typeof(p) <: Int) Base.parse(Int64,p.value) else p end)) for (name, p, t) in term_defs]
    rule_defs = Any[options_from_rule(x[1],x[2:end]...) for x in rule_defs]

    #println("Check: term_defs $term_defs\nrule_defs $rule_defs")
    
    # Execute statements
    ignore, imports = [] , Dict()
    for a_stmt in statements
        @assert length(a_stmt) == 1
        stmt = a_stmt[1]
        if stmt.data == "ignore"
            t = stmt.children[1]
            push!(ignore,t)
        elseif stmt.data == "import"
            if length(stmt.children) > 1
                path_node, arg1 = stmt.children
            else
                path_node = stmt.children[1]
                arg1 = nothing
            end
            if arg1 isa Tree  # Multi import
                dotted_path = tuple(path_node.children)
                names = arg1.children
                aliases = Dict(zip(names,names))  # Can't have aliased multi import, so all aliases will be the same as names
            else  # Single import
                dotted_path = tuple(path_node.children[1:end-1])
                name = path_node.children[end]  # Get name from dotted path
                aliases = Dict(name=> arg1 !== nothing ? arg1 : name )
            end # Aliases if exist

            if path_node.data == "import_lib"  # Import from library
                base_path = nothing
            else  # Relative import
                if grammar_name == "<string>"  # Import relative to script file path if grammar is coded in script
                    base_file = abspath(PROGRAM_FILE)
                else
                    base_file = grammar_name  # Import relative to grammar file path if external grammar file
                end
                if base_file !== nothing
                    base_path = first(splitdir(base_file))
                else
                    base_path = pwd()
                end
            end

            try
                import_base_path, import_aliases = imports[dotted_path]
                @assert base_path == import_base_path "Inconsistent base path: $base_path != $import_base_path"
                merge!(import_aliases,aliases)
            catch e
                if e isa KeyError
                    imports[dotted_path] = (base_path, aliases)
                else
                    rethrow(e)
                end
            end
        elseif stmt.data == "declare"
            for t in stmt.children
                push!(term_defs,[t.value, (nothing,nothing)])
            end
        else
            @assert false stmt
        end
    end
        
    # import grammars
    for (dotted_path, ba) in imports
        base_path, aliases = ba
        grammar_path = joinpath([x.value for x in dotted_path...]...) * EXT
        g = import_grammar(gl, grammar_path, base_path=base_path, import_paths=import_paths)
        new_td, new_rd = import_from_grammar_into_namespace(g, join([x.value for x in dotted_path...],"__"), aliases)

        append!(term_defs,new_td)
        append!(rule_defs,new_rd)
    end

    # Verify correctness 1
    for (name, _) in term_defs
        if startswith(name,"__")
            throw(GrammarError("Names starting with double-underscore are reserved (Error at $name)"))
        end
    end
    

    # Handle ignore tokens
    # XXX A slightly hacky solution. Recognition of %ignore TERMINAL as separate comes from the lexer's
    #     inability to handle duplicate terminals (two names, one value)
    ignore_names = []
    for t in ignore
        if t.data=="expansions" && length(t.children) == 1
            t2 = t.children[1]
            if t2.data=="expansion" && length(t2.children) == 1
                item = t2.children[1]
                if item.data == "value"
                    item = item.children[1]
                    if item isa Token && item.type_ == "TERMINAL"
                        push!(ignore_names,item.value)
                        continue
                    end
                end
            end
        end
        

        name = "__IGNORE_$(length(ignore_names))"
        push!(ignore_names,name)
        push!(term_defs,(name, (t, 0)))
    end
    # Verify correctness 2
    terminal_names = Set()
    for (name, _) in term_defs
        if name in terminal_names
            throw(GrammarError("Terminal '$name' defined more than once"))
        end
    
        push!(terminal_names,name)
    end

    if Set(ignore_names) > terminal_names
        throw(GrammarError("Terminals $(setdiff(ignore_names,terminal_names)) were marked to ignore but were not defined!"))
    end

    resolve_term_references(term_defs)
    rules = rule_defs
    rule_names = Dict()
    # Keep RuleOptions immutable by creating a whole new instance
    if gl.global_keep_all_tokens
        rules = [(a,b,c,RuleOptions(d,true)) for (a,b,c,d) in rules]
    end
    for (name, params, _x, option) in rules
        if gl.global_keep_all_tokens
            option = RuleOptions(option,true)
        end
        if startswith(name, "__")
            throw(GrammarError("Names starting with double-underscore are reserved (Error at $name)"))
        end
            
        if name in keys(rule_names)
            throw(GrammarError("Rule '$name' defined more than once"))
        end
        rule_names[name] = length(params)
    end

    #println("Rule names: $rule_names")

    for (name, params, expansions, _o) in rules
        for (i,p) in enumerate(params)
            if p in keys(rule_names)
                throw(GrammarError("Template Parameter conflicts with rule $p (in template $name)"))
            end
            if p in params[1:i-1]
                throw(GrammarError("Duplicate Template Parameter $p (in template $name)"))
            end
        end
        for temp in find_data(expansions,"template_usage")
            sym = temp.children[1]
            args = temp.children[2:end]
            if !(sym in params)
                if !(sym in keys(rule_names))
                    throw(GrammarError("Template '$sym' used but not defined (in rule $name)"))
                end
                if length(args) != rule_names[sym]
                    throw(GrammarError("Wrong number of template arguments used for $sym (expected $(rule_names[sym]), got $(length(args))) (in rule $name)"))
                end
            end
        end

        for sym in _find_used_symbols(expansions)
            if sym.type_ == "TERMINAL"
                if !(sym in terminal_names)
                    throw(GrammarError("Token '$sym' used but not defined (in rule $name)"))
                end
            else
                if !(sym in keys(rule_names)) && !(sym in params)
                    throw(GrammarError("Rule '$sym' used but not defined (in rule $name)"))
                end
            end
        end
    end
    return Grammar(rules, term_defs, ignore_names)

end

# Load grammar from a string
load_grammar(grammar,source,import_paths,global_keep_all_tokens) = begin
    if global_keep_all_tokens
        load_grammar(_lark_grammar_t,grammar,grammar_name=source,import_paths=import_paths)
    else
        load_grammar(_lark_grammar_f,grammar,grammar_name=source,import_paths=import_paths)
    end
end
