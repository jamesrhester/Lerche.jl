const EXT = ".lark"

const _RE_FLAGS = "imslux"

is_terminal(sym) = isuppercase(sym)

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
    "\'" => "QUOTE",
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
    "OP"=> "[+*][?]?|[?](?![a-z])",
    "_COLON"=> ":",
    "_COMMA"=> ",",
    "_OR"=> raw"\|",
    "_DOT"=> raw"\.",
    "TILDE"=> "~",
    "RULE"=> "!?[_?]?[a-z][_a-z0-9]*",
    "TERMINAL"=> "_?[A-Z][_A-Z0-9]*",
    "STRING"=> raw""""("|\\\\|[^"\n])*?"i?""",
    "REGEXP"=> raw"/(?!/)(\\/|\\\\|[^/\n])*?/[" * _RE_FLAGS * "]*",
    "_NL"=> raw"(\r?\n)+\s*",
    "WS"=> raw"[ \t]+",
    "COMMENT"=> raw"//[^\n]*",
    "_TO"=> "->",
    "_IGNORE"=> raw"%ignore",
    "_DECLARE"=> raw"%declare",
    "_IMPORT"=> raw"%import",
    "NUMBER"=> raw"\d+",
)

const RULES = Dict(
    "start"=> ["_list"],
    "_list"=>  ["_item", "_list _item"],
    "_item"=>  ["rule", "term", "statement", "_NL"],

    "rule"=> ["RULE _COLON expansions _NL",
             "RULE _DOT NUMBER _COLON expansions _NL"],
    "expansions"=> ["alias",
                   "expansions _OR alias",
                   "expansions _NL _OR alias"],

    "?alias"=>     ["expansion _TO RULE", "expansion"],
    "expansion"=> ["_expansion"],

    "_expansion"=> ["", "_expansion expr"],

    "?expr"=> ["atom",
              "atom OP",
              "atom TILDE NUMBER",
              "atom TILDE NUMBER _DOT _DOT NUMBER",
              ],

    "?atom"=> ["_LPAR expansions _RPAR",
              "maybe",
              "value"],

    "value"=> ["terminal",
              "nonterminal",
              "literal",
              "range"],

    "terminal"=> ["TERMINAL"],
    "nonterminal"=> ["RULE"],

    "?name"=> ["RULE", "TERMINAL"],

    "maybe"=> ["_LBRA expansions _RBRA"],
    "range"=> ["STRING _DOT _DOT STRING"],

    "term"=> ["TERMINAL _COLON expansions _NL",
              "TERMINAL _DOT NUMBER _COLON expansions _NL"],
    "statement"=> ["ignore", "import", "declare"],
    "ignore"=> ["_IGNORE expansions _NL"],
    "declare"=> ["_DECLARE _declare_args _NL"],
    "import"=> ["_IMPORT _import_path _NL",
               "_IMPORT _import_path _LPAR name_list _RPAR _NL",
               "_IMPORT _import_path _TO TERMINAL _NL"],

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
    new_rules
    rules_by_expr
    prefix
    i::Int
    rule_options
end

EBNF_to_BNF() = EBNF_to_BNF([],Dict(),"anon",0,nothing)

_add_recurse_rule(etb::EBNF_to_BNF,type_,expr) = begin
    if expr in collect(keys(etb.rules_by_expr))
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
            mn = mx = Int(args[1])
        else
            mn,mx = parse.(Int,args)
            if mx < mn
                error("Bad range for $rule ($mn..$mx isn't allowed)")
            end
        end
        return Tree("expansions", [Tree("expansion",fill(rule,n)) for n in mn:mx])
    end
end

struct SimplifyRule_Visitor <: Visitor end

_flatten!(srv::SimplifyRule_Visitor,tree) = begin
    while true
        to_expand = [i for (i,child) in enumerate(tree.children) if child isa Tree && child.data == tree.data]
        if length(to_expand) == 0 break end
        expand_kids_by_index!(tree,to_expand...)
    end
end

# Changes the argument but we can't add exclamation marks or it will
# spoil the link between rule and function
@rule expansion(srv::SimplifyRule_Visitor,tree) = begin
    _flatten!(srv,tree)
    for (i,child) in enumerate(tree.children)
        if child isa Tree && child.data == "expansions"
            tree.data = "expansions"
            tree.children = [visit(srv,Tree("expansion",
                                           [if i==j option else other end
                                            for (j,other) in enumerate(tree.children)]))
                             for option in Set(child.children)]
            _flatten!(srv,tree)
            break
        end
    end
end

@rule alias(srv::SimplifyRule_Visitor,tree) = begin
    rule,alias_name = tree.children
    if rule.data == "expansions"
        aliases = []
        for child in tree.children[1].children
            append!(aliases,Tree("alias",[child,alias_name]))
        end
        tree.data = "expansions"
        tree.children = aliases
    end
end

@rule expansions(srv::SimplifyRule_Visitor,tree) = begin
    _flatten!(srv,tree)
    tree.children = unique(tree.children)
end

struct RuleTreeToText <: Transformer end

@rule expansions(rtt::RuleTreeToText,x) = begin
    println("Pass through $x")
    return x
end

@rule expansion(rtt::RuleTreeToText,symbols) = (symbols,nothing)

@rule alias(rtt::RuleTreeToText,x) = begin
    (expansion, _alias), alias = x
    @assert _alias == nothing
    return expansion, alias.value
end

struct CanonizeTree <: Transformer_InPlace end

@inline_rule maybe(ct::CanonizeTree,expr) = Tree("expr",[expr, Token("OP","?",-1)])

@inline_rule tokenmods(ct::CanonizeTree,args...) = begin
    if length(args) == 1
        return Array(args)
    end
    tokenmods, value = args   #What does this mean people?
    return push!(tokenmods,value)
end

mutable struct PrepareAnonTerminals <: Transformer_InPlace
    terminals
    term_set::Set
    term_reverse::Dict
    i
end

PrepareAnonTerminals(terminals) = PrepareAnonTerminals(terminals,Set([td.name for td in terminals]),
                                                       Dict([td.pattern => td for td in terminals]),
                                                       0)

@inline_rule pattern(panon::PrepareAnonTerminals,p) = begin
    value = p.value
    if p in collect(keys(panon.term_reverse)) && p.flags != panon.term_reverse[p].pattern.flags
        throw(GrammarError("Conflicting flags for the same terminal: $p"))
    end
    term_name = nothing
    if p isa PatternStr
        try
            term_name = panon.term_reverse[p].name
        catch KeyError
            try
                term_name = _TERMINAL_NAMES[value]
            catch KeyError
                if !(uppercase(value) in panon.term_set)
                    term_name = uppercase(value)
                end
            end
            if term_name in panon.term_set
                term_name = nothing
            end
        end
    elseif p isa PatternRE
        if p in collect(keys(panon.term_reverse))
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

    return Terminal(term_name,filter_out = p isa PatternStr)
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


_fix_escaping(s) = error("Cannot fix escaping")

## TODO write this properly
_literal_to_pattern(literal) = begin
    println("Converting $literal to pattern")
    v = literal.value
    flag_start = _rfind(v, "/\"") + 1
    @assert flag_start > 0
    flags = v[flag_start:end]
    @assert all(f -> occursin(f, _RE_FLAGS), flags)
    v = v[1:flag_start-1]
    @assert v[1] == v[end] && occursin(v[1], "\"/")
    x = v[2:end-1]   #drop delimiters
    ### TODO fix escaping
    s = x
    if literal.type_ == "STRING"
        s = replace(s,"\\\\"=>"\\")
    end
    return Dict("STRING"=> PatternStr,
                "REGEXP" => PatternRE)[literal.type_](s,flags)
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

struct TerminalTreeToPattern <: Transformer end

@rule pattern(tttp::TerminalTreeToPattern, ps) = begin
    p = ps[1]
    return p
end

@rule expansion(tttp::TerminalTreeToPattern, items) = begin
    if length(items) == 1
        return items[1]
    end
    if length(Set(i.flags for i in items)) > 1
        throw(GrammarError("Lark doesn't support joining terminals with conflicting flags!"))
    end
    return PatternRE("(?:$(join("|",(to_regexp(i) for i in items))))",
                     if length(items) > 0 items[1].flags else () end)
end

@rule expansions(tttp::TerminalTreeToPattern, exps) = begin
    if length(exps) == 1
        return items[1]
    end
    if length(Set(i.flags for i in exps)) > 1
        throw(GrammarError("Lark doesn't support joining terminals with conflicting flags!"))
    end
    return PatternRE("(?:$(join("|",(to_regexp(i) for i in exps))))", exps[1].flags)
end

@rule expr(tttp::TerminalTreeToPattern,args) = begin
    inner,op = args[1:2]
    if op == "~"
        if length(args) == 3
            op = "{$(parse(Int,args[3]))}"
        else
            mn,mx = parse.(Int,args[3:end])
            if mx < mn
                throw(GrammarError("Bad range for $inner ($mn..$mx isn't allowed)"))
            end
            op = "{$mn,$mx}"
        end
    else
        @assert length(args) == 2
    end
    return PatternRE("(?:$(to_regexp(inner)))$(inner.flags)")
end

@rule alias(tttp::TerminalTreeToPattern,t) = throw(GrammarError("Aliasing not allowed in terminals (You used -> in the wrong place)"))

@rule value(tttp::TerminalTreeToPattern,v) = v[1]

struct PrepareSymbols <: Transformer_InPlace end

@rule value(ps::PrepareSymbols,v) = begin
    print("PrepareSymbols/value called with $v")
    v = v[1]
    if v isa Tree
        return v
    elseif v.type_ == "RULE"
        return NonTerminal(v.value)
    elseif v.type_ == "TERMINAL"
        return Terminal(v.value, filter_out = x -> startswith(x,"_"))
    end
    @assert false
end

_choice_of_rules(rules) = Tree("expansions",[Tree("expansion",[Token("RULE",name)]) for name in rules])

# Note order of fields matches call order
mutable struct Grammar
    rule_defs
    term_defs
    ignore
end

compile(g::Grammar) = begin
    # Deepcopy to allow multiple calling
    term_defs = deepcopy(g.term_defs)
    rule_defs = deepcopy(g.rule_defs)

    # ===================
    #  Compile Terminals
    # ===================
    
    # Convert terminal-trees to strings/regexps
    transformer = PrepareLiterals() * TerminalTreeToPattern()
    for (name, (term_tree, priority)) in term_defs
        if term_tree == nothing  # Terminal added through %declare
            continue
        end
        expansions = collect(find_data(term_tree,"expansion"))
        if length(expansions) == 1 && length(expansions[1].children) == 0
            throw(GrammarError("Terminals cannot be empty ($name)"))
        end
    end
    terminals = [TerminalDef(name, transform(transformer,term_tree), priority)
                 for (name, (term_tree, priority)) in term_defs if length(term_tree)>0]

    # =================
    #  Compile Rules
    # =================

    # 1. Pre-process terminals
    transformer = PrepareLiterals() * PrepareSymbols() * PrepareAnonTerminals(terminals)   # Adds to terminals

    # 2. Convert EBNF to BNF (and apply step 1)
    ebnf_to_bnf = EBNF_to_BNF()
    rules = []
    for (name, rule_tree, options) in rule_defs
        ebnf_to_bnf.rule_options = if options.keep_all_tokens  #drop check that options != nothing
            RuleOptions(keep_all_tokens=true)
        else
            nothing
        end
        tree = transform(transformer,rule_tree)
        push!(rules,(name, transform(ebnf_to_bnf,tree), options))
    end

    println("Transformed rules: $rules")
    
    append!(rules, ebnf_to_bnf.new_rules)
    
    @assert length(rules) == length(Set(name for (name, _t, _o) in rules)) "Whoops, name collision"

    # 3. Compile tree to Rule objects
    rule_tree_to_text = RuleTreeToText()
    
    simplify_rule = SimplifyRule_Visitor()
    compiled_rules = []
    for (name, tree, options) in rules
        visit(simplify_rule,tree)
        println("Tree after simplification:\n$tree")
        expansions = transform(rule_tree_to_text,tree)
        println("Tree after expansions:\n$tree, returned $expansions")
        for (expansion, alias) in expansions
            if alias != nothing && first(name) =='_'
                throw(GrammarError("Rule $name is marked for expansion (it starts with an underscore) and isn't allowed to have aliases (alias=$alias)"))
            end
            

            @assert all(x-> x isa LarkSymbol,expansion) expansion

            rule = Rule(NonTerminal(name), expansion, alias, options)
            push!(compiled_rules,rule)
        end
    end
    return terminals, compiled_rules, g.ignore
end

# TODO: port over importation of grammars

resolve_term_references(term_defs) = begin
    token_dict = Dict(k=>t for (k,(t,_p)) in term_defs)
    @assert length(token_dict) == length(term_defs) "Same name defined twice?"
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
                        exp.children[1] = token_dict[item]
                        changed = true
                    end
                end
            end
        end
        
        if !changed
            break
        end
    end
end


options_from_rule(name, x...) = begin
    if length(x) > 1
        priority, expansions = x
        priority = parse(Int,priority)
    else
        expansions = x[1]
        priority = nothing
    end
    

    keep_all_tokens = startswith(name,"!")
    name = lstrip(name,'!')
    expand1 = startswith(name,"?")
    name = lstrip(name,'?')

    return name, expansions, RuleOptions(keep_all_tokens=keep_all_tokens, expand1=expand1, priority=priority)
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


mutable struct GrammarLoader
    parser
    canonize_tree
end

GrammarLoader() = begin
    terminals = [TerminalDef(name, PatternRE(value)) for (name, value) in TERMINALS]
    rules = [options_from_rule(name, x) for (name, x) in  RULES]
    rules = [Rule(NonTerminal(r), symbols_from_strcase.(split(x)), nothing, o) for (r, xs, o) in rules for x in xs]
    #== debugging
    for r in rules
        println("Rules: $r")
        println("===")
    end
    ==#
    callback = create_callback(ParseTreeBuilder(rules))
    println("All keys for calling back:")
    for k in keys(callback)
        println("$k")
    end
    lexer_conf = LexerConf(terminals, ignore=["WS", "COMMENT"])

    parser_conf = ParserConf(rules, callback, "start")
    GrammarLoader(LALR_TraditionalLexer(lexer_conf, parser_conf),CanonizeTree())
end

"Parse grammar_text, verify, and create Grammar object. Display nice messages on error."
load_grammar(gl::GrammarLoader, grammar_text, grammar_name="<?>") = begin
    try
        parsetree = parse(gl.parser,grammar_text*"\n")
        println("Original parse tree:\n$parsetree")
        tree = transform(gl.canonize_tree,parse(gl.parser,grammar_text*"\n") )
    catch e
        if e isa UnexpectedCharacters
            context = get_context(e,grammar_text)
            throw(GrammarError("Unexpected input at line $(e.line) column $e.column in $(grammar_name): \n\n$context"))
            
        elseif e isa UnexpectedToken
            context = get_context(e,grammar_text)
            error = match_examples(e,gl, Dict(
                "Unclosed parenthesis"=> ["a: (\n"],
                "Umatched closing parenthesis"=> ["a: )\n", "a: [)\n", "a: (]\n"],
                "Expecting rule or terminal definition (missing colon)"=> ["a\n", "a->\n", "A->\n", "a A\n"],
                "Alias expects lowercase name"=> ["a: -> \"a\"\n"],
                "Unexpected colon"=> ["a::\n", "a: b:\n", "a: B:\n", "a: \"a\":\n"],
                "Misplaced operator"=> ["a: b??", "a: b(?)", "a:+\n", "a:?\n", "a:*\n", "a:|*\n"],
                "Expecting option (\"|\") or a new rule or terminal definition"=> ["a:a\n()\n"],
                "%import expects a name"=> ["%import \"a\"\n"],
                "%ignore expects a value"=> ["%ignore %import\n"],
            ))
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

    term_defs = [if length(td)==3 td else (td[1], 1, td[2]) end for td in term_defs]
    term_defs = [(name.value, (t, int(p))) for (name, p, t) in term_defs]
    rule_defs = [options_from_rule(x[1],x[2:end]...) for x in rule_defs]

    println("Check: term_defs $term_defs\nrule_defs $rule_defs")
    
    # Execute statements
    ignore = []
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
                dotted_path = path_node.children
                names = arg1.children
                aliases = names  # Can't have aliased multi import, so all aliases will be the same as names
            else  # Single import
                dotted_path = path_node.children[:end-1]
                names = [path_node.children[end]]  # Get name from dotted path
                aliases = if arg1 != nothing
                    [arg1]
                else
                    names
                end # Aliases if exist
            end
            
            grammar_path = joinpath(dotted_path...) * EXT

            if path_node.data == "import_lib"  # Import from library
                g = import_grammar(grammar_path)
            else  # Relative import
                if grammar_name == "<string>"  # Import relative to script file path if grammar is coded in script
                    base_file = abspath(@__FILE__)
                else
                    base_file = grammar_name  # Import relative to grammar file path if external grammar file
                end
                
                base_path = splitpath(base_file)[1]
                g = import_grammar(grammar_path, base_paths=[base_path])
            end
            
            aliases_dict = Dict(zip(names, aliases))
            new_td, new_rd = import_from_grammar_into_namespace(g, join(".",dotted_path), aliases_dict)

            append!(term_defs, new_td)
            append!(rule_defs, new_rd)
        elseif stmt.data == "declare"
            for t in stmt.children
                push!(term_defs,[t.value, (nothing, nothing)])
            end
            
        else
            @assert false stmt
        end
    end
    

    # Verify correctness 1
    for (name, _) in term_defs
        if name[1:2]=="__"
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
    rule_names = Set()
    for (name, _x, _o) in rules
        if startswith(name, "__")
            throw(GrammarError("Names starting with double-underscore are reserved (Error at $name)"))
        end
            
        if name in rule_names
            throw(GrammarError("Rule '$name' defined more than once"))
        end
        push!(rule_names,name)
    end

    println("Rule names: $rule_names")

    for (name, expansions, _o) in rules
        used_symbols = Set([t for x in find_data(expansions,"expansion")
                            for t in scan_values(x,t -> t.type_ in ("RULE", "TERMINAL"))])
        println("Symbols used for $name: $used_symbols")
        for sym in used_symbols
            if sym.type_ == "TERMINAL"
                if !(sym in terminal_names)
                    throw(GrammarError("Token '$sym' used but not defined (in rule $name)"))
                end
            else
                if !(sym in rule_names)
                    throw(GrammarError("Rule '$sym' used but not defined (in rule $name)"))
                end
            end
        end
    end
                    
# TODO don't include unused terminals, they can only cause trouble!

    println("Baking it in, baby: Rules $rules\n\nTerm defs: $term_defs\n\nIgnoring: $ignore_names")
    return Grammar(rules, term_defs, ignore_names)

end

load_grammar(text::String;options...) = load_grammar(GrammarLoader(),text,options...)
