#==
    Propagate positions
    g = Lark("""start: a
                    a: "a"
                 """, lexer="standard",parser="lalr",propagate_positions=true)

    r = Lerche.parse(g,"a")
    println("$(Lerche.meta(r.children[1]))")
    @test Lerche.meta(r.children[1]).line == 1

end
==#

#== The following tests copy those in lark.py with the following exceptions:
(1) No unicode tests (Julia is natively unicode)
(2) No CYK/Earley/Dynamic lexer/skipped tests (not implemented in Lerche)
(3) 

==#

# This doesn't seem to work in Lark 0.7 either.

#==
@testset "Propagate positions" begin
    g = Lark("""start: a
                    a: "a"
                 """, lexer="standard",parser="lalr",propagate_positions=true)

    r = Lerche.parse(g,"a")
    @test_skip Lerche.meta(r.children[1]).line == 1

end
==#
@testset "Big list" begin
    g = "start: " * join(["\"$i\"" for i in 1:250])
    Lark(g,parser="lalr")
end

@testset "Infinite recursion" begin

    g = """start: a
           a: a | "a"
        """
    @test_throws Exception Lark(g,parser="lalr")
end

# For use in token transform test
# Two concrete types distinguished by whether or not
# to handle tokens
abstract type TVT <: Transformer end
struct TVTT <: TVT end
struct TVTF <: TVT end
@rule a(t::TVT,children) = children[1]*"!"
@terminal A(t::TVT,tok) = Lerche.update(tok,value=uppercase(tok.value))
Lerche.visit_tokens(t::TVTF) = false
Lerche.visit_tokens(t::TVTT) = true #default anyway

make_parser_test(lexer,parser) = begin
    make_lark(grammar;kwargs...) = begin
        Lark(grammar,lexer=lexer,parser=parser,propagate_positions=true;kwargs...)
    end
    @testset "test_terminal_matching_order" begin
        g = Lark("""
object: 
        | SIGNED_FLOAT
        | INT
        | true
        | false
        | null

true : "true"
false : "false"
null : "null"

%import common.INT
%import common.SIGNED_FLOAT
%import common.WS
%ignore WS
""", start="object", parser="lalr")
        r = Lerche.parse(g,"2.0")
        @test r.children[1].value == "2.0"
    end

    @testset "Test expansions" begin
    #Test expand1
    g = Lark("""start: a
                    ?a: b
                    b: "x"
                 """,parser="lalr",lexer="standard",debug=true)

    r = Lerche.parse(g,"x")
    @test r.children[1].data == "b"
    
    g = Lark("""start: a
                    ?a: b -> c
                    b: "x"
                 """,parser="lalr",lexer="standard")

    r = Lerche.parse(g,"x")
    
    @test r.children[1].data == "c"

    g = Lark("""start: a
                        ?a: B -> c
                        B: "x"
                     """,parser="lalr",lexer="standard")
    r = Lerche.parse(g,"x")
    @test r.children[1].data == "c"

    g = Lark("""start: a
                    ?a: b b -> c
                    b: "x"
                 """,parser="lalr",lexer="standard")
    r = Lerche.parse(g,"xx")
    @test r.children[1].data == "c" 

    end

    @testset "test comment in rule definition" begin
        g = Lark("""start: a
                a: "a"
                // A comment
                // Another comment
                | "b"
                // Still more
                c: "unrelated"
            """)
        r = Lerche.parse(g,"b")
        @test r.children[1].data == "a"
    end

    @testset "test visit tokens" begin
        
        g = """start: a
            a : A
            A: "x"
            """
        p = Lark(g, parser="lalr")
        tf = TVTF()
        r = transform(tf, Lerche.parse(p,"x"))
        @test r.children == ["x!"]
        tt = TVTT()
        r = transform(tt,Lerche.parse(p,"x"))
        @test r.children == ["X!"]

        # Test internal transformer
        p = Lark(g, parser="lalr", transformer=TVTT())
        r = Lerche.parse(p,"x")
        @test r.children == ["X!"]
    end
    
    @testset "Test basic 1 ($lexer, $parser)" begin
    g = make_lark("""start: a+ b a* "b" a*
                        b: "b"
                        a: "a"
                     """,debug=true)

    r = Lerche.parse(g,"aaabaab")
    @test join([x.data for x in r.children],"") == "aaabaa"
    r = Lerche.parse(g,"aaabaaba")
    @test join([x.data for x in r.children],"") == "aaabaaa"    
    @test_throws UnexpectedToken Lerche.parse(g, "aaabaa")
    end
    
    @testset "Test_basic2 ($lexer, $parser)" begin
            # Multiple parsers and colliding tokens
        g = make_lark("""start: B A
                         B: "12"
                         A: "1" """)
        x = Lerche.parse(g,"121")
        @test x.data == "start"
        @test x.children == ["12", "1"]

        g2 = make_lark("""start: B A
                         B: "12"
                         A: "2" """)
        x = Lerche.parse(g2,"122")
        @test x.data == "start" && x.children == ["12", "2"]
    end

    @testset "Test unicode" begin
        g = make_lark("""start: B A
                         B: "♫♪"
                         A: "♫" """)
        x = Lerche.parse(g,"♫♪♫")
    end
    
    @testset "EBNF stack depth" begin
        g = make_lark("""start: a+
                         a : "a" """)

        Lerche.parse(g, repeat("a",500))
    end

    @testset "Expand lists with one item" begin
        g = make_lark("""start: list
                            ?list: item+
                            item : A
                            A: "a"
                        """)
        r = Lerche.parse(g,"a")

        # because 'list' is an expand-if-contains-one rule and we only provided one element it should have expanded to 'item'
        @test [subtree.data for subtree in r.children] == ["item"]

        # regardless of the amount of items: there should be only *one* child in 'start' because 'list' isn't an expand-all rule
        @test length(r.children) == 1
    end
    @testset "test_expand1_lists_with_one_item_2" begin
            g = make_lark("""start: list
                            ?list: item+ "!"
                            item : A
                            A: "a"
                        """)
            r = Lerche.parse(g,"a!")

            # because 'list' is an expand-if-contains-one rule and we only provided one element it should have expanded to 'item'
        @test [subtree.data for subtree in r.children] == ["item"]

        # regardless of the amount of items: there should be only *one* child in 'start' because 'list' isn't an expand-all rule
        
        @test length(r.children)== 1
    end
    
    @testset "test_dont_expand1_lists_with_multiple_items" begin
        g = make_lark("""start: list
                                ?list: item+
                                item : A
                                A: "a"
                            """)
        r = Lerche.parse(g,"aa")

        # because 'list' is an expand-if-contains-one rule and we've provided more than one element it should *not* have expanded
        @test [subtree.data for subtree in r.children] == ["list"]

        # regardless of the amount of items: there should be only *one* child in 'start' because 'list' isn't an expand-all rule
        @test length(r.children)== 1

        # Sanity check: verify that 'list' contains the two 'item's we've given it
        list = r.children[1]
        @test [item.data for item in list.children] == ["item", "item"]
    end
    
    @testset "test_dont_expand1_lists_with_multiple_items_2" begin
        g = make_lark("""start: list
                                ?list: item+ "!"
                                item : A
                                A: "a"
                            """)
        r = Lerche.parse(g,"aa!")

        # because 'list' is an expand-if-contains-one rule and we've provided more than one element it should *not* have expanded
        @test [subtree.data for subtree in r.children] == ["list"]

        # regardless of the amount of items: there should be only *one* child in 'start' because 'list' isn't an expand-all rule
        @test length(r.children)==1

        # Sanity check: verify that 'list' contains the two 'item's we've given it
        list = r.children[1]
        @test [item.data for item in list.children] == ["item", "item"]
    end

    @testset "test_empty_expand1_list" begin
        g = make_lark("""start: list
                                ?list: item*
                                item : A
                                A: "a"
                             """)
        r = Lerche.parse(g,"")

        # because 'list' is an expand-if-contains-one rule and we've provided less than one element (i.e. none) it should *not* have expanded
        @test [subtree.data for subtree in r.children] == ["list"]

        # regardless of the amount of items: there should be only *one* child in 'start' because 'list' isn't an expand-all rule
        @test length(r.children)== 1

        # Sanity check: verify that 'list' contains no 'item's as we've given it none
        list = r.children[1]
        @test [item.data for item in list.children] == []
    end

    @testset "test_empty_expand1_list_2" begin
            g = make_lark("""start: list
                            ?list: item* "!"?
                            item : A
                            A: "a"
                         """)
            r = Lerche.parse(g,"")
        # because 'list' is an expand-if-contains-one rule and we've provided less than one element (i.e. none) it should *not* have expanded
        @test [subtree.data for subtree in r.children] == ["list"]

        # regardless of the amount of items: there should be only *one* child in 'start' because 'list' isn't an expand-all rule
        @test length(r.children)== 1

        # Sanity check: verify that 'list' contains no 'item's as we've given it none
        list = r.children[1]
        @test [item.data for item in list.children] == []
    end

    @testset "test_empty_flatten_list" begin
            g = make_lark("""start: list
                            list: | item "," list
                            item : A
                            A: "a"
                         """)
            r = Lerche.parse(g,"")

            # Because 'list' is a flatten rule it's top-level element should *never* be expanded
            @test [subtree.data for subtree in r.children] == ["list"]

            # Sanity check: verify that 'list' contains no 'item's as we've given it none
            list = r.children[1]
            @test [item.data for item in list.children] == []
    end


    @testset "test_token_collision" begin
            g = make_lark(raw"""start: "Hello" NAME
                        NAME: /\w/+
                        %ignore " "
                    """)
            x = Lerche.parse(g,"Hello World")
            @test x.children == ["World"]
            x = Lerche.parse(g,"Hello HelloWorld")
            @test x.children == ["HelloWorld"]
    end
  
    @testset "test_token_collision_WS" begin
            g = make_lark(raw"""start: "Hello" NAME
                        NAME: /\w/+
                        %import common.WS
                        %ignore WS
                    """)
            x = Lerche.parse(g,"Hello World")
            @test x.children == ["World"]
            x = Lerche.parse(g,"Hello HelloWorld")
        @test x.children == ["HelloWorld"]
    end

    @testset "test_token_collision2" begin
            g = make_lark("""
                    !start: "starts"
                    %import common.LCASE_LETTER
                    """)
            x = Lerche.parse(g,"starts")
        @test x.children == ["starts"]
    end

    @testset "test_templates" begin
            g = make_lark(raw"""
                       start: "[" sep{NUMBER, ","} "]"
                       sep{item, delim}: item (delim item)*
                       NUMBER: /\d+/
                       %ignore " "
                       """)
            x = Lerche.parse(g,"[1, 2, 3, 4]")
            @test x.children == [Tree("sep", ["1", "2", "3", "4"])]
            x = Lerche.parse(g,"[1]")
        @test x.children == [Tree("sep", ["1"])]
    end

    @testset "test_templates_recursion" begin
            g = make_lark(raw"""
                       start: "[" _sep{NUMBER, ","} "]"
                       _sep{item, delim}: item | _sep{item, delim} delim item
                       NUMBER: /\d+/
                       %ignore " "
                       """)
            x = Lerche.parse(g,"[1, 2, 3, 4]")
            @test x.children == ["1", "2", "3", "4"]
            x = Lerche.parse(g,"[1]")
        @test x.children == ["1"]
    end

    @testset "test_templates_import" begin
            g = Lerche.open("test_templates_import.lark", rel_to=@__FILE__)
            x = Lerche.parse(g,"[1, 2, 3, 4]")
            @test x.children == [Tree("sep", ["1", "2", "3", "4"])]
            x = Lerche.parse(g,"[1]")
            @test x.children == [Tree("sep", ["1"])]
    end

    @testset "test_templates_alias" begin
            g = make_lark(raw"""
                       start: expr{"C"}
                       expr{t}: "A" t
                              | "B" t -> b
                       """)
            x = Lerche.parse(g,"AC")
            @test x.children == [Tree("expr", [])]
            x = Lerche.parse(g,"BC")
            @test x.children == [Tree("b", [])]
        end

        @testset "test_templates_modifiers" begin
            g = make_lark(raw"""
                       start: expr{"B"}
                       !expr{t}: "A" t
                       """)
            x = Lerche.parse(g,"AB")
            @test x.children == [Tree("expr", ["A", "B"])]
            g = make_lark(raw"""
                       start: _expr{"B"}
                       !_expr{t}: "A" t
                       """)
            x = Lerche.parse(g,"AB")
            @test x.children == ["A", "B"]
            g = make_lark(raw"""
                       start: expr{b}
                       b: "B"
                       ?expr{t}: "A" t
                       """)
            x = Lerche.parse(g,"AB")
            @test x.children == [Tree("b",[])]
        end

        @testset "test_templates_templates" begin
            g = make_lark("""start: a{b}
                         a{t}: t{"a"}
                         b{x}: x""")
            x = Lerche.parse(g,"a")
            @test x.children == [Tree("a", [Tree("b",[])])]
        end

        @testset "test_g_regex_flags" begin
            g = make_lark("""
                    start: "a" /b+/ C
                    C: "C" | D
                    D: "D" E
                    E: "e"
                    """, g_regex_flags=2)   #re.I
            x1 = Lerche.parse(g,"ABBc")
            x2 = Lerche.parse(g,"abdE")
        end

    @testset "test_undefined_rule" begin
           @test_throws GrammarError  make_lark("""start: a""")
    end
    
    @testset "test_undefined_token" begin
           @test_throws GrammarError make_lark("""start: A""")
    end
    
    @testset "test_rule_collision" begin
            g = make_lark("""start: "a"+ "b"
                             | "a"+ """)
            x = Lerche.parse(g,"aaaa")
            x = Lerche.parse(g,"aaaab")
    end
    
    @testset "test_rule_collision2" begin
            g = make_lark("""start: "a"* "b"
                             | "a"+ """)
            x = Lerche.parse(g,"aaaa")
            x = Lerche.parse(g,"aaaab")
            x = Lerche.parse(g,"b")
    end

    @testset "test_token_not_anon" begin
            """Tests that "a" is matched as an anonymous token, and not A.
            """

            g = make_lark("""start: "a"
                        A: "a" """)
            x = Lerche.parse(g,"a")
            @test length(x.children)==0 #(" \"a\" should be considered anonymous")

            g = make_lark("""start: "a" A
                        A: "a" """)
            x = Lerche.parse(g,"aa")
            @test length(x.children)==1 #("only \"a\" should be considered anonymous")
            @test x.children[1].type_== "A"

            g = make_lark("""start: /a/
                        A: /a/ """)
            x = Lerche.parse(g,"a")
            @test length(x.children)== 1
            @test x.children[1].type_== "A" # ("A isn't associated with /a/")
    end
    
        @testset "test_maybe" begin
            g = make_lark("""start: ["a"] """)
            x = Lerche.parse(g,"a")
            x = Lerche.parse(g,"")
        end
            
        @testset "test_start" begin
            g = make_lark("""a: "a" a? """, start="a")
            x = Lerche.parse(g,"a")
            x = Lerche.parse(g,"aa")
            x = Lerche.parse(g,"aaa")
        end
    
        @testset "test_alias" begin
            g = make_lark("""start: "a" -> b """)
            x = Lerche.parse(g,"a")
            @test x.data== "b"
        end
    
        @testset "test_token_ebnf" begin
            g = make_lark("""start: A
                      A: "a"* ("b"? "c".."e")+
                      """)
            x = Lerche.parse(g,"abcde")
            x = Lerche.parse(g,"dd")
        end
    
    @testset "test_backslash" begin
        # Note extra backslashes relative to Python
        # due to different treatment of raw strings
            g = make_lark(raw"""start: "\\\\" "a"
                      """)
            x = Lerche.parse(g,raw"\a")

            g = make_lark(raw"""start: /\\/ /a/
                      """)
            x = Lerche.parse(g,raw"\a")
        end
    
        @testset "test_special_chars" begin
            g = make_lark(raw"""start: "\n"
                      """)
            x = Lerche.parse(g,"\n")

            g = make_lark(raw"""start: /\n/
                      """)
            x = Lerche.parse(g,"\n")
        end

    @testset "test_backslash2" begin
        # Note extra backslashes relative to Python
        # due to different treatment of raw strings
            g = make_lark(raw"""start: "\\"" "-"
                      """)
            x = Lerche.parse(g,"\"-")

            g = make_lark(raw"""start: /\// /-/
                      """)
            x = Lerche.parse(g,"/-")
        end
    
        # @testset "test_token_recurse" begin
        #     g = make_lark("""start: A
        #                  A: B
        #                  B: A
        #               """)

        @testset "test_empty" begin
            # Fails an Earley implementation without special handling for empty rules,
            # or re-processing of already completed rules.
            g = make_lark(raw"""start: _empty a "B"
                          a: _empty "A"
                          _empty:
                            """)
            x = Lerche.parse(g,"AB")
        end
    
        @testset "test_regex_quote" begin
            g = raw"""
            start: SINGLE_QUOTED_STRING | DOUBLE_QUOTED_STRING
            SINGLE_QUOTED_STRING  : /'[^']*'/
            DOUBLE_QUOTED_STRING  : /"[^"]*"/
            """

            g = make_lark(g)
            @test  Lerche.parse(g,"\"hello\"").children == ["\"hello\""]
            @test  Lerche.parse(g,"'hello'").children == ["'hello'"]
        end

        @testset "test_float_without_lexer" begin
            g = make_lark("""start: ["+"|"-"] float
                         float: digit* "." digit+ exp?
                              | digit+ exp
                         exp: ("e"|"E") ["+"|"-"] digit+
                         digit: "0"|"1"|"2"|"3"|"4"|"5"|"6"|"7"|"8"|"9"
                      """)
            Lerche.parse(g,"1.2")
            Lerche.parse(g,"-.2e9")
            Lerche.parse(g,"+2e-9")
            @test_throws UnexpectedToken Lerche.parse(g,"+2e-9e")
        end
            
        @testset "test_keep_all_tokens" begin
            l = make_lark("""start: "a"+ """, keep_all_tokens=true)
            tree = Lerche.parse(l,"aaa")
            @test tree.children == ["a", "a", "a"]
        end

        @testset "test_token_flags" begin
            l = make_lark("""!start: "a"i+
                      """
                      )
            tree = Lerche.parse(l,"aA")
            @test tree.children == ["a", "A"]

            l = make_lark("""!start: /a/i+
                      """
                      )
            tree = Lerche.parse(l,"aA")
            @test tree.children == ["a", "A"]

            # g = """!start: "a"i "a"
            #     """
            # self.assertRaises(GrammarError, make_lark, g)

            # g = """!start: /a/i /a/
            #     """
            # self.assertRaises(GrammarError, make_lark, g)

            g = """start: NAME "," "a"
                   NAME: /[a-z_]/i /[a-z0-9_]/i*
                """
            l = make_lark(g)
            tree = Lerche.parse(l,"ab,a")
            @test tree.children == ["ab"]
            tree = Lerche.parse(l,"AB,a")
            @test tree.children == ["AB"]
        end

        @testset "test_token_flags3" begin
            l = make_lark("""!start: ABC+
                      ABC: "abc"i
                      """
                      )
            tree = Lerche.parse(l,"aBcAbC")
            @test tree.children == ["aBc", "AbC"]
        end

        @testset "test_token_flags2" begin
            g = """!start: ("a"i | /a/ /b/?)+
                """
            l = make_lark(g)
            tree = Lerche.parse(l,"aA")
            @test tree.children == ["a", "A"]
        end

        @testset "test_token_flags_verbose" begin
            g = make_lark(raw"""start: NL | ABC
                          ABC: / [a-z] /x
                          NL: /\n/
                      """)
            x = Lerche.parse(g,"a")
            @test x.children == ["a"]
        end

        @testset "test_token_flags_verbose_multiline" begin
            g = make_lark(raw"""start: ABC
                          ABC: /  a      b c
                               d
                                e f
                           /x
                       """)
            x = Lerche.parse(g,"abcdef")
            @test x.children == ["abcdef"]
        end

        @testset "test_token_multiline_only_works_with_x_flag" begin
            g = raw"""start: ABC
                    ABC: /  a      b c
                              d
                                e f
                            /i
                      """
            @test_throws GrammarError make_lark(g)
        end

        @testset "test_twice_empty" begin
            g = """!start: ("A"?)?
                """
            l = make_lark(g)
            tree = Lerche.parse(l,"A")
            @test tree.children == ["A"]

            tree = Lerche.parse(l,"")
            @test tree.children == []
        end

        @testset "test_undefined_ignore" begin
            g = """!start: "A"

                %ignore B
                """
            @test_throws GrammarError  make_lark( g)
        end

        @testset "test_alias_in_terminal" begin
            g = """start: TERM
                TERM: "a" -> alias
                """
            @test_throws  Exception make_lark(g)
        end

        @testset "test_line_and_column" begin
            g = raw"""!start: "A" bc "D"
                !bc: "B\nC"
                """
            l = make_lark(g)
            a, bc, d = Lerche.parse(l,"AB\nCD").children
            @test a.line== 1
            @test a.column== 1

            bc = bc.children[1]
            @test bc.line== 1
            @test bc.column== 2

            @test d.line== 2
            @test d.column== 2

            @test a.end_line== 1
            @test a.end_column== 2
            @test bc.end_line== 2
            @test bc.end_column== 2
            @test d.end_line== 2
            @test d.end_column== 3
        end

        @testset "test_reduce_cycle" begin
            """Tests an edge-condition in the LALR parser, in which a transition state looks exactly like the end state.
            It seems that the correct solution is to explicitely distinguish finalization in the reduce() function.
            """

            l = make_lark("""
                term: A
                    | term term

                A: "a"

            """, start="term")

            tree = Lerche.parse(l,"aa")
            @test length(tree.children)== 2
        end


# Only for standard lexer
        @testset "test_lexer_prioritization" begin
            "Tests effect of priority on result"

            grammar = """
            start: A B | AB
            A.2: "a"
            B: "b"
            AB: "ab"
            """
            l = make_lark(grammar)
            res = Lerche.parse(l,"ab")

            @test res.children== ["a", "b"]
            @test res.children!= ["ab"]

            grammar = """
            start: A B | AB
            A: "a"
            B: "b"
            AB.3: "ab"
            """
            l = make_lark(grammar)
            res = Lerche.parse(l,"ab")

            @test res.children != ["a", "b"]
            @test res.children == ["ab"]

            grammar = """
            start: A B | AB
            A: "a"
            B.-20: "b"
            AB.-10: "ab"
            """
            l = make_lark(grammar)
            res = Lerche.parse(l,"ab")
            @test res.children == ["a", "b"]

            # Lark difference: priority goes in an Int64 so can't be as big as
            # in Python
            grammar = """
            start: A B | AB
            A.-99999999999999: "a"
            B: "b"
            AB: "ab"
            """
            l = make_lark(grammar)
            res = Lerche.parse(l,"ab")

            @test res.children == ["ab"]

        end

        @testset "test_import" begin
            grammar = """
            start: NUMBER WORD

            %import common.NUMBER
            %import common.WORD
            %import common.WS
            %ignore WS

            """
            l = make_lark(grammar)
            x = Lerche.parse(l,"12 elephants")
            @test x.children== ["12", "elephants"]
        end

        @testset "test_relative_import" begin
            grammar = """
            start: NUMBER WORD

            %import .grammars.test.NUMBER
            %import common.WORD
            %import common.WS
            %ignore WS

            """
            l = make_lark(grammar)
            x = Lerche.parse(l,"12 lions")
            @test x.children == ["12", "lions"]
        end

        @testset "test_multi_import" begin
            grammar = """
            start: NUMBER WORD

            %import common (NUMBER, WORD, WS)
            %ignore WS

            """
            l = make_lark(grammar)
            x = Lerche.parse(l,"12 toucans")
            @test x.children == ["12", "toucans"]
        end

        @testset "test_relative_multi_import" begin
            grammar = """
           start: NUMBER WORD

           %import .grammars.test (NUMBER, WORD, WS)
           %ignore WS

           """
            l = make_lark(grammar)
            x = Lerche.parse(l,"12 capybaras")
            @test x.children== ["12", "capybaras"]
        end

        @testset "test_import_errors" begin
            grammar = """
            start: NUMBER WORD

            %import .grammars.bad_test.NUMBER
            """
            @test_throws SystemError make_lark(grammar)

            grammar = """
            start: NUMBER WORD

            %import bad_test.NUMBER
            """
            @test_throws SystemError make_lark(grammar)
        end

        @testset "test_utf8" begin
            g = """start: a
                   a: "±a"
                """
            l = make_lark(g)
            @test Lerche.parse(l,"±a") == Tree("start", [Tree("a", [])])

            g = """start: A
                   A: "±a"
                """
            l = make_lark(g)
            @test Lerche.parse(l,"±a") ==Tree("start", [Token("A","±a")])
        end

        @testset "test_ignore" begin
            g = raw"""
            COMMENT: /(!|(\/\/))[^\n]*/
            %ignore COMMENT
            %import common.WS -> _WS
            %import common.INT
            start: "INT"i _WS+ INT _WS*
            """

            p = make_lark(g)

            tree = Lerche.parse(p,"int 1 ! This is a comment\n")
            @test tree.children == ["1"]
            tree = Lerche.parse(p,"int 1 ! This is a comment")    # A trailing ignore token can be tricky!
            @test tree.children == ["1"]

            p = make_lark(raw"""
                start : "a"*
                %ignore "b" """)
            tree = Lerche.parse(p,"bb")
            
            @test tree.children == []
        end


        @testset "test_regex_escaping" begin
            g = make_lark("start: /[ab]/")
            Lerche.parse(g,"a")
            Lerche.parse(g,"b")

            @test_throws  UnexpectedInput Lerche.parse(g, "c")

            g = make_lark(raw"start: /\w/")
            Lerche.parse(g,"a")

            g = make_lark(raw"start: /\\w/")
            @test_throws  UnexpectedInput Lerche.parse(g,"a")
            Lerche.parse(g,raw"\w")

            Lerche.parse(make_lark(raw"""start: /\[/"""),"[")

            Lerche.parse(make_lark(raw"start: /\//"),"/")

            Lerche.parse(make_lark(raw"start: /\\/"),"\\")

            Lerche.parse(make_lark(raw"start: /\[ab]/"),"[ab]")

            Lerche.parse(make_lark(raw"start: /\\[ab]/"),"\\a")

            Lerche.parse(make_lark(raw"start: /\t/"),"\t")

            Lerche.parse(make_lark(raw"start: /\\t/"),"\\t")

            Lerche.parse(make_lark(raw"start: /\\\t/"),"\\\t")

            Lerche.parse(make_lark(raw"""start: "\t" """),"\t")

            Lerche.parse(make_lark(raw"""start: "\\t" """),"\\t")

            Lerche.parse(make_lark(raw"""start: "\\\t" """),"\\\t")
        end

        @testset "test_ranged_repeat_rules" begin
            g = """!start: "A"~3
                """
            l = make_lark(g)
            @test Lerche.parse(l,"AAA")== Tree("start", ["A", "A", "A"])
            @test_throws UnexpectedInput Lerche.parse(l, "AA")
            @test_throws UnexpectedInput Lerche.parse(l, "AAAA")

            g = """!start: "A"~0..2
                """
            l = make_lark(g)
            @test Lerche.parse(l,"")== Tree("start", [])
            @test Lerche.parse(l,"A")== Tree("start", ["A"])
            @test Lerche.parse(l,"AA")== Tree("start", ["A", "A"])
            @test_throws UnexpectedInput Lerche.parse(l, "AAA")

            g = """!start: "A"~3..2
                """
            @test_throws GrammarError make_lark( g)

            g = """!start: "A"~2..3 "B"~2
                """
            l = make_lark(g)
            @test Lerche.parse(l,"AABB")== Tree("start", ["A", "A", "B", "B"])
            @test Lerche.parse(l,"AAABB")== Tree("start", ["A", "A", "A", "B", "B"])
            @test_throws UnexpectedInput Lerche.parse(l, "AAAB")
            @test_throws UnexpectedInput Lerche.parse(l, "AAABBB")
            @test_throws UnexpectedInput Lerche.parse(l, "ABB")
            @test_throws UnexpectedInput Lerche.parse(l, "AAAABB")
        end

        @testset "test_ranged_repeat_terms" begin
            g = """!start: AAA
                    AAA: "A"~3
                """
            l = make_lark(g)
            @test Lerche.parse(l,"AAA")== Tree("start", ["AAA"])
            @test_throws UnexpectedInput Lerche.parse(l, "AA")
            @test_throws UnexpectedInput Lerche.parse(l, "AAAA")

            g = """!start: AABB CC
                    AABB: "A"~0..2 "B"~2
                    CC: "C"~1..2
                """
            l = make_lark(g)
            @test Lerche.parse(l,"AABBCC")== Tree("start", ["AABB", "CC"])
            @test Lerche.parse(l,"BBC")== Tree("start", ["BB", "C"])
            @test Lerche.parse(l,"ABBCC")== Tree("start", ["ABB", "CC"])
            @test_throws UnexpectedInput Lerche.parse(l, "AAAB")
            @test_throws UnexpectedInput Lerche.parse(l, "AAABBB")
            @test_throws UnexpectedInput Lerche.parse(l, "ABB")
            @test_throws UnexpectedInput Lerche.parse(l, "AAAABB")
        end

        @testset "test_priority_vs_embedded" begin
            g = """
            A.2: "a"
            WORD: ("a".."z")+

            start: (A | WORD)+
            """
            l = make_lark(g)
            t = Lerche.parse(l,"abc")
            @test t.children== ["a", "bc"]
            @test t.children[1].type_ == "A"
        end

        @testset "test_line_counting" begin
            p = make_lark("start: /[^x]+/")

            text = "hello\nworld"
            t = Lerche.parse(p,text)
            tok = t.children[1]
            @test tok== text
            @test tok.line== 1
            @test tok.column== 1
            @test tok.end_line== 2
            @test tok.end_column== 6
        end

        @testset "test_empty_end" begin
            p = make_lark("""
                start: b c d
                b: "B"
                c: | "C"
                d: | "D"
            """)
            res = Lerche.parse(p,"B")
            @test length(res.children)== 3
        end

        @testset "test_multi_start" begin
            p = make_lark("""
                a: "x" "a"?
                b: "x" "b"?
                """, start=["a", "b"])

            @test Lerche.parse(p,"xa", start="a") == Tree("a", Union{Token,Tree}[])
            @test Lerche.parse(p,"xb", start="b") == Tree("b", Union{Token,Tree}[])
        end

        @testset "test_lexer_detect_newline_tokens" begin
            # Detect newlines in regular tokens
            println("lexer is $lexer, parser is $parser")
            test_gram = raw"""start: "go" tail*
            !tail : SA "@" | SB "@" | SC "@" | SD "@"
            SA : "a" /\n/
            SB : /b./s
            SC : "c" /[^a-z]/
            SD : "d" /\s/
            """
            g = make_lark(test_gram)
            a,b,c,d = [x.children[2] for x in Lerche.parse(g,"goa\n@b\n@c\n@d\n@").children]
            @test a.line == 2
            @test b.line == 3
            @test c.line == 4
            @test d.line == 5

            # Detect newlines in ignored tokens
            for re in ["/\\n/", "/[^a-z]/", "/\\s/"]
                g = make_lark("""!start: "a" "a"
                             %ignore $re""")
                a, b = Lerche.parse(g,"a\na").children
                @test a.line == 1
                @test b.line == 2
            end
        end
end

const TO_TEST = [("standard","lalr"),
                 ("contextual","lalr")
                 ]

for (lexer,parser) in TO_TEST
    make_parser_test(lexer,parser)
end
