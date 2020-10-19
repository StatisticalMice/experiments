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

# ╔═╡ e401509a-11de-11eb-2139-f90fc50ae6cf
begin
	datadirectory = "../data/titanic/"
	train_df = CSV.read(datadirectory * "train.csv")
	describe(train_df)
end

# ╔═╡ Cell order:
# ╠═d5257f82-11de-11eb-0176-b1858e7894ae
# ╠═e401509a-11de-11eb-2139-f90fc50ae6cf
