using MRBC; using Random; using NSGAII; using BenchmarkTools; using JLD2; using Serialization;

# global H = Dict{UInt, Dict{UInt, Dict{Int, Dict{NTuple{2,Int}, Float64}}}}()

# @btime nsga_max(10,10,z,init,fCV=CV,fcross=rules_crossover!,fmut=rules_mutation!,pmut=0.3)


M = begin
    Random.seed!(1)
    # lk = Base.Threads.SpinLock()
    # @show H
<<<<<<< HEAD
    @time M = nsga_max(2,5,z,init,fCV=CV,fcross=rules_crossover!,fmut=rules_mutation!,pmut=0.3)
=======
    @time M = nsga_max(100,100,z,init,fCV=CV,fcross=rules_crossover!,fmut=rules_mutation!,pmut=0.3)
>>>>>>> dev_grammar
    return M
end

# @save "model.jld2" M

serialize("model.mm", M)

# old version: H = Dict{UInt, Dict{UInt, Dict{Int, Dict{NTuple{2,Int}, Float64}}}}()
# 16.428 s (604568605 allocations: 12.88 GiB)

# new version: H = Dict{Tuple{UInt, UInt, UInt, Tuple{UInt, UInt}}, Float64}()
# 15.376 s (535684825 allocations: 11.40 GiB)
# 10.489 s (435886655 allocations: 9.23 GiB)