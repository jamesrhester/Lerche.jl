#==
@testset "Test Lark parser" begin

    #Infinite recursion test
    
    g = """start: a
           a: a | "a"
        """
    @test_throws GrammarError Lark(g,parser="lalr")

    Propagate positions
    g = Lark("""start: a
                    a: "a"
                 """, lexer="standard",parser="lalr",propagate_positions=true)

    r = Lerchen.parse(g,"a")
    println("$(Lerchen.meta(r.children[1]))")
    @test Lerchen.meta(r.children[1]).line == 1

end
==#

#== The following tests copy those in lark.py with the following exceptions:
(1) No unicode tests (Julia is natively unicode)
(2) No CYK/Earley/Dynamic lexer/skipped tests (not implemented in Lerchen)
(3) 

==#

make_parser_test(lexer,parser) = begin
    make_lark(grammar;kwargs...) = begin
        Lark(grammar,lexer=lexer,parser=parser,propagate_positions=true;kwargs...)
    end


        @testset "test_ranged_repeat_terms" begin
            g = """!start: AAA
                    AAA: "A"~3
                """
            l = make_lark(g)
            @test Lerchen.parse(l,"AAA")== Tree("start", ["AAA"])
            @test_throws UnexpectedInput Lerchen.parse(l, "AA")
            @test_throws UnexpectedInput Lerchen.parse(l, "AAAA")

            g = """!start: AABB CC
                    AABB: "A"~0..2 "B"~2
                    CC: "C"~1..2
                """
            l = make_lark(g)
            @test Lerchen.parse(l,"AABBCC")== Tree("start", ["AABB", "CC"])
            @test Lerchen.parse(l,"BBC")== Tree("start", ["BB", "C"])
            @test Lerchen.parse(l,"ABBCC")== Tree("start", ["ABB", "CC"])
            @test_throws UnexpectedInput Lerchen.parse(l, "AAAB")
            @test_throws UnexpectedInput Lerchen.parse(l, "AAABBB")
            @test_throws UnexpectedInput Lerchen.parse(l, "ABB")
            @test_throws UnexpectedInput Lerchen.parse(l, "AAAABB")
        end

        @testset "test_priority_vs_embedded" begin
            g = """
            A.2: "a"
            WORD: ("a".."z")+

            start: (A | WORD)+
            """
            l = make_lark(g)
            t = Lerchen.parse(l,"abc")
            @test t.children== ["a", "bc"]
            @test t.children[1].type_ == "A"
        end

        @testset "test_line_counting" begin
            p = make_lark("start: /[^x]+/")

            text = "hello\nworld"
            t = Lerchen.parse(p,text)
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
            res = Lerchen.parse(p,"B")
            @test length(res.children)== 3
        end

    # First batch of tests starts here ======
    
    @testset "Test basic 1 ($lexer, $parser)" begin
    g = make_lark("""start: a+ b a* "b" a*
                        b: "b"
                        a: "a"
                     """,debug=true)

    r = Lerchen.parse(g,"aaabaab")
    @test join([x.data for x in r.children],"") == "aaabaa"
    r = Lerchen.parse(g,"aaabaaba")
    @test join([x.data for x in r.children],"") == "aaabaaa"    
    @test_throws UnexpectedToken Lerchen.parse(g, "aaabaa")
    end
    
    @testset "Test_basic2 ($lexer, $parser)" begin
            # Multiple parsers and colliding tokens
        g = make_lark("""start: B A
                         B: "12"
                         A: "1" """)
        x = Lerchen.parse(g,"121")
        @test x.data == "start"
        @test x.children == ["12", "1"]

        g2 = make_lark("""start: B A
                         B: "12"
                         A: "2" """)
        x = Lerchen.parse(g2,"122")
        @test x.data == "start" && x.children == ["12", "2"]
    end

    @testset "EBNF stack depth" begin
        g = make_lark("""start: a+
                         a : "a" """)

        Lerchen.parse(g, repeat("a",500))
    end

    @testset "Expand lists with one item" begin
        g = make_lark("""start: list
                            ?list: item+
                            item : A
                            A: "a"
                        """)
        r = Lerchen.parse(g,"a")

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
            r = Lerchen.parse(g,"a!")

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
        r = Lerchen.parse(g,"aa")

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
        r = Lerchen.parse(g,"aa!")

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
        r = Lerchen.parse(g,"")

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
            r = Lerchen.parse(g,"")
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
            r = Lerchen.parse(g,"")

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
            x = Lerchen.parse(g,"Hello World")
            @test x.children == ["World"]
            x = Lerchen.parse(g,"Hello HelloWorld")
            @test x.children == ["HelloWorld"]
    end
  
    @testset "test_token_collision_WS" begin
            g = make_lark(raw"""start: "Hello" NAME
                        NAME: /\w/+
                        %import common.WS
                        %ignore WS
                    """)
            x = Lerchen.parse(g,"Hello World")
            @test x.children == ["World"]
            x = Lerchen.parse(g,"Hello HelloWorld")
        @test x.children == ["HelloWorld"]
    end

    @testset "test_token_collision2" begin
            g = make_lark("""
                    !start: "starts"
                    %import common.LCASE_LETTER
                    """)
            x = Lerchen.parse(g,"starts")
        @test x.children == ["starts"]
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
            x = Lerchen.parse(g,"aaaa")
            x = Lerchen.parse(g,"aaaab")
    end
    
    @testset "test_rule_collision2" begin
            g = make_lark("""start: "a"* "b"
                             | "a"+ """)
            x = Lerchen.parse(g,"aaaa")
            x = Lerchen.parse(g,"aaaab")
            x = Lerchen.parse(g,"b")
    end

    @testset "test_token_not_anon" begin
            """Tests that "a" is matched as an anonymous token, and not A.
            """

            g = make_lark("""start: "a"
                        A: "a" """)
            x = Lerchen.parse(g,"a")
            @test length(x.children)==0 #(" \"a\" should be considered anonymous")

            g = make_lark("""start: "a" A
                        A: "a" """)
            x = Lerchen.parse(g,"aa")
            @test length(x.children)==1 #("only \"a\" should be considered anonymous")
            @test x.children[1].type_== "A"

            g = make_lark("""start: /a/
                        A: /a/ """)
            x = Lerchen.parse(g,"a")
            @test length(x.children)== 1
            @test x.children[1].type_== "A" # ("A isn't associated with /a/")
    end
    
        @testset "test_maybe" begin
            g = make_lark("""start: ["a"] """)
            x = Lerchen.parse(g,"a")
            x = Lerchen.parse(g,"")
        end
            
        @testset "test_start" begin
            g = make_lark("""a: "a" a? """, start="a")
            x = Lerchen.parse(g,"a")
            x = Lerchen.parse(g,"aa")
            x = Lerchen.parse(g,"aaa")
        end
    
        @testset "test_alias" begin
            g = make_lark("""start: "a" -> b """)
            x = Lerchen.parse(g,"a")
            @test x.data== "b"
        end
    
        @testset "test_token_ebnf" begin
            g = make_lark("""start: A
                      A: "a"* ("b"? "c".."e")+
                      """)
            x = Lerchen.parse(g,"abcde")
            x = Lerchen.parse(g,"dd")
        end
    
    @testset "test_backslash" begin
        # Note extra backslashes relative to Python
        # due to different treatment of raw strings
            g = make_lark(raw"""start: "\\\\" "a"
                      """)
            x = Lerchen.parse(g,raw"\a")

            g = make_lark(raw"""start: /\\/ /a/
                      """)
            x = Lerchen.parse(g,raw"\a")
        end
    
        @testset "test_special_chars" begin
            g = make_lark(raw"""start: "\n"
                      """)
            x = Lerchen.parse(g,"\n")

            g = make_lark(raw"""start: /\n/
                      """)
            x = Lerchen.parse(g,"\n")
        end

    @testset "test_backslash2" begin
        # Note extra backslashes relative to Python
        # due to different treatment of raw strings
            g = make_lark(raw"""start: "\\"" "-"
                      """)
            x = Lerchen.parse(g,"\"-")

            g = make_lark(raw"""start: /\// /-/
                      """)
            x = Lerchen.parse(g,"/-")
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
            x = Lerchen.parse(g,"AB")
        end
    
        @testset "test_regex_quote" begin
            g = raw"""
            start: SINGLE_QUOTED_STRING | DOUBLE_QUOTED_STRING
            SINGLE_QUOTED_STRING  : /'[^']*'/
            DOUBLE_QUOTED_STRING  : /"[^"]*"/
            """

            g = make_lark(g)
            @test  Lerchen.parse(g,"\"hello\"").children == ["\"hello\""]
            @test  Lerchen.parse(g,"'hello'").children == ["'hello'"]
        end

        @testset "test_float_without_lexer" begin
            g = make_lark("""start: ["+"|"-"] float
                         float: digit* "." digit+ exp?
                              | digit+ exp
                         exp: ("e"|"E") ["+"|"-"] digit+
                         digit: "0"|"1"|"2"|"3"|"4"|"5"|"6"|"7"|"8"|"9"
                      """)
            Lerchen.parse(g,"1.2")
            Lerchen.parse(g,"-.2e9")
            Lerchen.parse(g,"+2e-9")
            @test_throws UnexpectedToken Lerchen.parse(g,"+2e-9e")
        end
            
        @testset "test_keep_all_tokens" begin
            l = make_lark("""start: "a"+ """, keep_all_tokens=true)
            tree = Lerchen.parse(l,"aaa")
            @test tree.children == ["a", "a", "a"]
        end

        @testset "test_token_flags" begin
            l = make_lark("""!start: "a"i+
                      """
                      )
            tree = Lerchen.parse(l,"aA")
            @test tree.children == ["a", "A"]

            l = make_lark("""!start: /a/i+
                      """
                      )
            tree = Lerchen.parse(l,"aA")
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
            tree = Lerchen.parse(l,"ab,a")
            @test tree.children == ["ab"]
            tree = Lerchen.parse(l,"AB,a")
            @test tree.children == ["AB"]
        end

        @testset "test_token_flags3" begin
            l = make_lark("""!start: ABC+
                      ABC: "abc"i
                      """
                      )
            tree = Lerchen.parse(l,"aBcAbC")
            @test tree.children == ["aBc", "AbC"]
        end

        @testset "test_token_flags2" begin
            g = """!start: ("a"i | /a/ /b/?)+
                """
            l = make_lark(g)
            tree = Lerchen.parse(l,"aA")
            @test tree.children == ["a", "A"]
        end

        @testset "test_twice_empty" begin
            g = """!start: [["A"]]
                """
            l = make_lark(g)
            tree = Lerchen.parse(l,"A")
            @test tree.children == ["A"]

            tree = Lerchen.parse(l,"")
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
            @test_throws  GrammarError make_lark(g)
        end

        @testset "test_line_and_column" begin
            g = raw"""!start: "A" bc "D"
                !bc: "B\nC"
                """
            l = make_lark(g)
            a, bc, d = Lerchen.parse(l,"AB\nCD").children
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

            tree = Lerchen.parse(l,"aa")
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
            res = Lerchen.parse(l,"ab")

            @test res.children== ["a", "b"]
            @test res.children!= ["ab"]

            grammar = """
            start: A B | AB
            A: "a"
            B: "b"
            AB.3: "ab"
            """
            l = make_lark(grammar)
            res = Lerchen.parse(l,"ab")

            @test res.children != ["a", "b"]
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
            x = Lerchen.parse(l,"12 elephants")
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
            x = Lerchen.parse(l,"12 lions")
            @test x.children == ["12", "lions"]
        end

        @testset "test_multi_import" begin
            grammar = """
            start: NUMBER WORD

            %import common (NUMBER, WORD, WS)
            %ignore WS

            """
            l = make_lark(grammar)
            x = Lerchen.parse(l,"12 toucans")
            @test x.children == ["12", "toucans"]
        end

        @testset "test_relative_multi_import" begin
            grammar = """
           start: NUMBER WORD

           %import .grammars.test (NUMBER, WORD, WS)
           %ignore WS

           """
            l = make_lark(grammar)
            x = Lerchen.parse(l,"12 capybaras")
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
            @test Lerchen.parse(l,"±a") == Tree("start", [Tree("a", [])])

            g = """start: A
                   A: "±a"
                """
            l = make_lark(g)
            @test Lerchen.parse(l,"±a") ==Tree("start", [Token("A","±a")])
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

            tree = Lerchen.parse(p,"int 1 ! This is a comment\n")
            @test tree.children == ["1"]
            tree = Lerchen.parse(p,"int 1 ! This is a comment")    # A trailing ignore token can be tricky!
            @test tree.children == ["1"]

            p = make_lark(raw"""
                start : "a"*
                %ignore "b" """)
            tree = Lerchen.parse(p,"bb")
            
            @test tree.children == []
        end


        @testset "test_regex_escaping" begin
            g = make_lark("start: /[ab]/")
            Lerchen.parse(g,"a")
            Lerchen.parse(g,"b")

            @test_throws  UnexpectedInput Lerchen.parse(g, "c")

            g = make_lark(raw"start: /\w/")
            Lerchen.parse(g,"a")

            g = make_lark(raw"start: /\\w/")
            @test_throws  UnexpectedInput Lerchen.parse(g,"a")
            Lerchen.parse(g,raw"\w")

            Lerchen.parse(make_lark(raw"""start: /\[/"""),"[")

            Lerchen.parse(make_lark(raw"start: /\//"),"/")

            Lerchen.parse(make_lark(raw"start: /\\/"),"\\")

            Lerchen.parse(make_lark(raw"start: /\[ab]/"),"[ab]")

            Lerchen.parse(make_lark(raw"start: /\\[ab]/"),"\\a")

            Lerchen.parse(make_lark(raw"start: /\t/"),"\t")

            Lerchen.parse(make_lark(raw"start: /\\t/"),"\\t")

            Lerchen.parse(make_lark(raw"start: /\\\t/"),"\\\t")

            Lerchen.parse(make_lark(raw"""start: "\t" """),"\t")

            Lerchen.parse(make_lark(raw"""start: "\\t" """),"\\t")

            Lerchen.parse(make_lark(raw"""start: "\\\t" """),"\\\t")
        end

        @testset "test_ranged_repeat_rules" begin
            g = """!start: "A"~3
                """
            l = make_lark(g)
            @test Lerchen.parse(l,"AAA")== Tree("start", ["A", "A", "A"])
            @test_throws UnexpectedInput Lerchen.parse(l, "AA")
            @test_throws UnexpectedInput Lerchen.parse(l, "AAAA")

            g = """!start: "A"~0..2
                """
            l = make_lark(g)
            @test Lerchen.parse(l,"")== Tree("start", [])
            @test Lerchen.parse(l,"A")== Tree("start", ["A"])
            @test Lerchen.parse(l,"AA")== Tree("start", ["A", "A"])
            @test_throws UnexpectedInput Lerchen.parse(l, "AAA")

            g = """!start: "A"~3..2
                """
            @test_throws GrammarError make_lark( g)

            g = """!start: "A"~2..3 "B"~2
                """
            l = make_lark(g)
            @test Lerchen.parse(l,"AABB")== Tree("start", ["A", "A", "B", "B"])
            @test Lerchen.parse(l,"AAABB")== Tree("start", ["A", "A", "A", "B", "B"])
            @test_throws UnexpectedInput Lerchen.parse(l, "AAAB")
            @test_throws UnexpectedInput Lerchen.parse(l, "AAABBB")
            @test_throws UnexpectedInput Lerchen.parse(l, "ABB")
            @test_throws UnexpectedInput Lerchen.parse(l, "AAAABB")
        end

end

const TO_TEST = [#("standard","lalr"),
                 ("contextual","lalr")
                 ]

for (lexer,parser) in TO_TEST
    make_parser_test(lexer,parser)
end

#== Not sure if these are supposed to work for lalr
@testset "Test expansions" begin
    #Test expand1
    g = Lark("""start: a
                    ?a: b
                    b: "x"
                 """,parser="lalr",debug=true)

    r = Lerchen.parse(g,"x")
    @test r.children[1].data == "b"
    
    g = Lark("""start: a
                    ?a: b -> c
                    b: "x"
                 """)

    r = Lerchen.parse("x")
    
    @test r.children[1].data == "c"

    g = Lark("""start: a
                        ?a: B -> c
                        B: "x"
                     """)
    r = Lerchen.parse("x")
    @test r.children[1].data == "c"

    g = Lark("""start: a
                    ?a: b b -> c
                    b: "x"
                 """)
    r = Lerchen.parse("xx")
    @test r.children[1].data, "c" )

end
==#
