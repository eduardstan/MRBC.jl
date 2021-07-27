using MRBC; using Random; using NSGAII; using BenchmarkTools; using JLD2;

# global H = Dict{UInt, Dict{UInt, Dict{Int, Dict{NTuple{2,Int}, Float64}}}}()

# @btime nsga_max(10,10,z,init,fCV=CV,fcross=rules_crossover!,fmut=rules_mutation!,pmut=0.3)


M = begin
    Random.seed!(1)
    # lk = Base.Threads.SpinLock()
    # @show H
    @time M = nsga_max(100,100,z,init,fCV=CV,fcross=rules_crossover!,fmut=rules_mutation!,pmut=0.3)
    return M
end

@save "model.jld2" M