module MRBC

using CategoricalArrays
using DataFrames
using NSGAII
using Random
using StatsBase

import Base: copyto!, eltype, isequal, iterate, getindex, length, firstindex, lastindex, ndims, size, show, hash
import StatsBase: sample

export readARFF, sample, paa, transform!, flatten!, CV, rules_crossover!, rules_mutation!, z, init, H, @grammar, _print_rules, _print_rule, _print_antecedent, _print_consequent

include("dataset.jl")
include("logic.jl")
include("genetic_operators.jl")
include("core_functions.jl")
# include("grammar.jl")

train = readARFF("data/RacketSports/RacketSports_TRAIN.arff");
# train2 = readARFF("data/RacketSports/RacketSports_TRAIN.arff");
transform!(train,paa,1;n_chunks=5);
# transform!(train2, 1, [paa,paa], [(;n_chunks=2, f=mean),(;n_chunks=2, f=StatsBase.var)])

# H = Dict{UInt, Dict{UInt, Dict{Int, Dict{NTuple{2,Int}, Float64}}}}()

H = Dict{Tuple{UInt, UInt, UInt, Tuple{UInt, UInt}}, Float64}()

end
