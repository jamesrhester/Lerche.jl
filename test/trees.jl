@rule_holder

setup_tree() = Tree("a",[Tree(x,y) for (x,y) in zip("bcd","xyz")])

@testset "Interpreter testing" begin
    t = Tree("a",[Tree("b",[]), Tree("c",[]),"d"])
    @contains_rules struct Interp1 <: Interpreter end
    @rule a(i::Interp1,tree) = append!(visit_children(i,tree), ["e"])
    @rule b(i::Interp1,tree) = "B"
    @rule c(i::Interp1,tree) = "C"

    @test visit(Interp1(),t) == ["B","C","d","e"]

    @contains_rules struct Interp3 <: Interpreter end
    @rule b(i::Interp3, tree) = "B"
    @rule c(i::Interp3, tree) = "C"

    @test visit(Interp3(),t) == ["B","C","d"]
end

@testset "Transformer testing" begin
    t = Tree("add", [Tree("sub", [Tree("i", ["3"]), Tree("f", ["1.1"])]), Tree("i", ["1"])])

    @contains_rules struct T <: Transformer end

    @inline_rule i(t::T,args...) = Base.parse.(Int64,args...)
    @inline_rule f(t::T,args...) = Base.parse.(Float64,args...)
    @inline_rule sub(t::T,args...) = args[1] - args[2]
    @inline_rule add(t::T,args...) = sum(args)

    @test transform(T(),t) == 2.9
end

