using Documenter, Lerche

makedocs(sitename="Lerche documentation",
	  format = Documenter.HTML(
				   prettyurls = get(ENV,"CI",nothing) == "true"
				   ),
         pages = [
             "Overview and Guide" => "index.md",
	     "Grammar reference" => "grammar.md",
             "API" => "api.md"
             ],
         #doctest = :fix
	  )

deploydocs(
    repo = "github.com/jamesrhester/Lerche.jl.git",
)
