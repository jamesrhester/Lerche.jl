setup_tree() = Tree("a",[Tree(x,y) for (x,y) in zip("bcd","xyz")])

@testset "Interpreter testing" begin
    t = Tree("a",[Tree("b",[]), Tree("c",[]),"d"])
    struct Interp1 <: Interpreter end
    a(i::Interp1,tree) = visit_children(tree) + ["e"]
    b(i::Interp1,tree) = "B"
    c(i::Interp1,tree) = "C"

    @test visit(Interp1(),t) == ["B","C","d","e"]
end

