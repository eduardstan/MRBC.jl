module MRBC

using CategoricalArrays
using DataFrames
using NSGAII
using Random
using StatsBase

import Base: copyto!, eltype, isequal, iterate, getindex, length, ndims, size, show, hash
import StatsBase: sample

export readARFF, sample, paa, transform!, CV, rules_crossover!, rules_mutation!, z, init

include("dataset.jl")
include("logic.jl")
include("genetic_operators.jl")
include("core_functions.jl")

train = readARFF("data/RacketSports/RacketSports_TRAIN.arff");
transform!(train,paa,1;n_chunks=5);

M = begin
    Random.seed!(1)
    # global H = Dict{UInt, Dict{UInt, Dict{Int, Dict{NTuple{2,Int}, Float64}}}}()
    # lk = Base.Threads.SpinLock()
    # @show H
    @time M = nsga_max(100,100,z,init,fCV=CV,fcross=rules_crossover!,fmut=rules_mutation!,pmut=0.3)
    return M
end

end
