setup_tree() = Tree("a",[Tree(x,y) for (x,y) in zip("bcd","xyz")])

@testset "Interpreter testing" begin
    t = Tree("a",[Tree("b",[]), Tree("c",[]),"d"])
    struct Interp1 <: Interpreter end
    @rule a(i::Interp1,tree) = append!(visit_children(i,tree), ["e"])
    @rule b(i::Interp1,tree) = "B"
    @rule c(i::Interp1,tree) = "C"

    @test visit(Interp1(),t) == ["B","C","d","e"]

    struct Interp3 <: Interpreter end
    @rule b(i::Interp3, tree) = "B"
    @rule c(i::Interp3, tree) = "C"

    @test visit(Interp3(),t) == ["B","C","d"]
end

@testset "Transformer testing" begin
    t = Tree("add", [Tree("sub", [Tree("i", ["3"]), Tree("f", ["1.1"])]), Tree("i", ["1"])])

    struct T <: Transformer end

    @rule i(t::T,args...) = parse.(Int,args...)
    @rule f(t::T,args...) = parse.(Float64,args...)
    @rule sub(t::T,args...) = args[1] - args[2]
    @rule add(t::T,args...) = sum(args)

    @test transform(T(),t) == 2.9
end

