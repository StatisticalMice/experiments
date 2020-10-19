### A Pluto.jl notebook ###
# v0.12.4

using Markdown
using InteractiveUtils

# ╔═╡ d5257f82-11de-11eb-0176-b1858e7894ae
begin
	using DataFrames
	using Pipe
	using Gadfly
	using CSV
	using BenchmarkTools
end

# ╔═╡ a612e268-11e9-11eb-2305-0175760c625d
HTML("""
		<style>
			main {
				max-width: 90%;
			}
		</style>
	""")

# ╔═╡ e401509a-11de-11eb-2139-f90fc50ae6cf
begin
	datadirectory = "../data/titanic/"
	train_df = CSV.read(datadirectory * "train.csv")
	describe(train_df)
end

# ╔═╡ a6b3a7a4-11ea-11eb-0d7c-0797092fec7e
ENV["COLUMNS"] = 10

# ╔═╡ Cell order:
# ╠═a612e268-11e9-11eb-2305-0175760c625d
# ╠═d5257f82-11de-11eb-0176-b1858e7894ae
# ╠═e401509a-11de-11eb-2139-f90fc50ae6cf
# ╠═a6b3a7a4-11ea-11eb-0d7c-0797092fec7e
