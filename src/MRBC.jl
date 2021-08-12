module MRBC

using CategoricalArrays
using DataFrames
using NSGAII
using Random
using StatsBase

import Base: copyto!, eltype, isequal, iterate, getindex, length, firstindex, lastindex, ndims, size, show, hash
import StatsBase: sample

export readARFF, sample, paa, transform!, CV, rules_crossover!, rules_mutation!, z, init, H

include("dataset.jl")
include("logic.jl")
include("genetic_operators.jl")
include("core_functions.jl")

train = readARFF("data/RacketSports/RacketSports_TRAIN.arff");
# transform!(train,paa,1;n_chunks=10);

# H = Dict{UInt, Dict{UInt, Dict{Int, Dict{NTuple{2,Int}, Float64}}}}()

H = Dict{Tuple{UInt, UInt, UInt, Tuple{UInt, UInt}}, Float64}()

end
