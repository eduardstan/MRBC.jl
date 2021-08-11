struct ModalInstance <: AbstractVector{SubDataFrame}
    # worlds::Set{<:AbstractOntology} # TODO add set of worlds, for the semantics (DT)
    rows::Vector{DataFrameRow}
end

ModalInstance() = ModalInstance(DataFrameRow[])
DataFrames.nrow(::Array{DataFrameRow,1}) = 1

function show(io::IO, ::MIME"text/plain", mi::ModalInstance)
    println(io, "Modal instance with $(length(mi.rows)) frames")
    for i in 1:length(mi.rows)
        println(io, "Frame #$i")
        println(io, mi.rows[i])
    end
end

size(mi::ModalInstance) = (nrow(mi.rows),)
getindex(mi::ModalInstance, i::Int) = mi.rows[i]
length(mi::ModalInstance, fid::Int) = max([length(mi[fid][i]) for i in 1:length(mi[fid])]...)

mutable struct ModalFrame <: AbstractVector{SubDataFrame}
    # name::String # TODO add name?
    dimension::Int  # (0=static, 1=timeseries, 2=images, ecc.)
    data::DataFrame
    views::Vector{DataFrameRow} # intraframe instances (i.e., first, second, .., series)
end

dimension(mf::ModalFrame) = mf.dimension

@inline function ModalFrame(data::DataFrame)
    colnames = names(data)

    @assert length(colnames) > 0 && nrow(data) > 0 "There must be at least one column and one instance in data to compute its dimension."

    dimension = ndims(data[1, Symbol(colnames[1])])

    for colname in colnames
        curr_dimension = ndims(data[1, Symbol(colname)])
        @assert dimension == curr_dimension "Each attribute must have the same dimension."
    end

    # if dimension > 0
    #     typ = eltype(data[1, Symbol(colnams[1])])
    #     for colname in colnames
    #         curr_typ = eltype(data[1, Symbol(colname)])
    #         @assert typ == cur_typ "Each modal frame with higher-than-zero dimension must have the same element types."
    #     end
    # end

    views = Vector{DataFrameRow}(undef, nrow(data))
    for i in 1:nrow(data)
        views[i] = view(data, i, :)
    end
    ModalFrame(dimension, data, views)
end

size(mf::ModalFrame) = (nrow(mf.data),)
getindex(mf::ModalFrame, i::Int) = mf.views[i]

struct ClassificationDataset
    ldim::Int
    hdim::Int
    frames::Vector{ModalFrame}
    instances::Vector{ModalInstance}
    domains::Dict{Int, Dict{Symbol, Vector{Number}}}
    classes::CategoricalArray
    unique_classes::Vector
end

function ClassificationDataset(frames::Vector{ModalFrame}, classes::CategoricalArray)
    instances = ModalInstance[]
    domains = Dict{Int, Dict{Symbol, Vector{Number}}}()

    n_instances = size(frames[1])

    for i in 1:length(frames)
        curr_n_instances = size(frames[i])
        @assert n_instances == curr_n_instances "Each frame must have the same number of (intraframe) instances!"
    end

    @assert n_instances[1] == length(classes) "Each instance must have one and only one class!"

    # Computing the instances.
    # for each instance
    for i in 1:n_instances[1]
        instance = ModalInstance()
        # for each frame
        for f in 1:length(frames)
            # push the instance
            push!(instance.rows, frames[f][i])
        end
        push!(instances, instance)
    end

    # Computing the domains.
    # for each frame
    for f in 1:length(frames)
        attrs = names(frames[f][1])
        # for each attribute
        attr_domain = Dict{Symbol, Vector{Number}}()
        for attr in attrs
            # convert to symbol
            A = Symbol(attr)
            # for each instance
            # for i in 1:n_instances[1]

            # end
            attr_domain[A] = collect(Set([v for i in 1:n_instances[1] for v in frames[f][i][A]]))
            domains[f] = attr_domain
        end
    end

    ldim, hdim = extrema([dimension(i) for i in frames])

    ClassificationDataset(ldim, hdim, frames, instances, domains, classes, unique(classes))
end

instance(cs::ClassificationDataset, i::Int) = cs.instances[i] # TODO cs -> ds (either sup or unsupervised)
instance(cs::ClassificationDataset, i::Int, f::Int) = cs.instances[i][f]
function attributes(ds::ClassificationDataset)
    d = Dict{Int,Array{String,1}}()
    for (fid,frame) in enumerate(ds.frames)
        d[fid] = names(frame.data)
    end
    return d
end
attributes(ds::ClassificationDataset, fid::Int) = names(ds.frames[fid].data)

length(ds::ClassificationDataset) = length(ds.instances)

function show(io::IO, ::MIME"text/plain", ds::ClassificationDataset)
    println(io, "Classification dataset with $(length(ds.instances)) instances")

    for (i, inst) in enumerate(ds.instances)
        println(io, "Instance #$(i):")
        println(io, instance(ds, i))
        println(io)
    end
end

# TODO
# possible test: all([auslan.instances[i].rows[1][attr] === auslan.frames[1].data[i,attr] for i in 1:length(auslan.instances) for attr in attributes(auslan, 1)])
function transform!(ds::ClassificationDataset, f::Function, fid::Int; kwargs...)
    for attr in attributes(ds, fid)
        for i in 1:nrow(ds.frames[fid].data)
            ds.frames[fid].data[i,attr] = f(ds.frames[fid].data[i,attr]; kwargs...)
        end
    end
    return ds
end

# function Base.minimum(ds::ClassificationDataset)
#     d = Dict{Int,Array{Float64,1}}()

#     for (fid,frame) in enumerate(ds.frames)
#         frame = ds.frames[fid]
#         if frame.dimension == 0
#             d[fid] = missing
#         else
#             d[fid] = [Base.minimum(vcat(frame.data[!,attr]...)) for attr in attributes(ds, fid)]
#         end
#     end
#     return d
# end

# function Base.maximum(ds::ClassificationDataset)
#     d = Dict{Int,Array{Float64,1}}()

#     for (fid,frame) in enumerate(ds.frames)
#         frame = ds.frames[fid]
#         if frame.dimension == 0
#             d[fid] = missing
#         else
#             d[fid] = [Base.maximum(vcat(frame.data[!,attr]...)) for attr in attributes(ds, fid)]
#         end
#     end
#     return d
# end

# TODO fix problem with select_attributes!
# TODO make it with bang!
function select_attributes(ds::ClassificationDataset, frame_attributes::Dict{Int, Array{Int, 1}})
    frames = collect(keys(frame_attributes))

    # Frame level.
    for i in 1:length(frames)
        ds.frames[frames[i]].data = ds.frames[frames[i]].data[:, frame_attributes[frames[i]]]
    end

    modal_frames = ModalFrame[]
    for i in 1:length(frames)
        push!(modal_frames, ModalFrame(ds.frames[frames[i]].data))
    end

    return ClassificationDataset(modal_frames, ds.classes)
end

# function transform!(ds::ClassificationDataset, f::Function; kwargs...)

# using MultivariateTimeSeries
# X, y = read_data_labeled(joinpath(dirname(pathof(GBDTs)), "..", "data", "auslan_youtube8"));
#
# # adesso voglio passare da MTS a ClassificationDataset
#
# my_frame = DataFrame()
#
# for attr in names(X)
#     insertcols!(my_frame, attr => Array{Float64,1}[])
# end
#
# # for each instance
# for i in 1:length(X)
#     array_data = Array{Float64, 2}(X[i])
#     push!(my_frame, [array_data[:, j] for j in 1:length(names(X))])
# end
#
# auslan = ClassificationDataset([ModalFrame(my_frame)], CategoricalArray(y))



function sample(ds::ClassificationDataset,
        range::Union{UnitRange{Int},Nothing};
        invert::Bool=false)

    n_instances = length(ds.instances)

    @assert range ⊆ 1:n_instances "The range must be a subset of 1:$(n_instances)."

    if ismissing(range)
        return ds
    else

        output_classes = Int[]
        timeseries_df = DataFrame()

        attrs = attributes(ds, 1) # TODO up to now, only timeseries

        for attr in attrs
            insertcols!(timeseries_df, attr => Array{Float64, 1}[]) # TODO also here Array{Float64, 1}[], i.e., timeseries
        end

        if invert == false
            for idx in range
                push!(timeseries_df, ds.instances[idx][1]) # ds.instances[idx][1] <-- [1] means the first frame TODO in future
                push!(output_classes, ds.classes[idx])
            end
        else
            for idx in setdiff(1:n_instances,range)
                push!(timeseries_df, ds.instances[idx][1]) # ds.instances[idx][1] <-- [1] means the first frame TODO in future
                push!(output_classes, ds.classes[idx])
            end
        end
    end

    return ClassificationDataset([ModalFrame(timeseries_df)], CategoricalArray(output_classes))
end

function sample(ds::ClassificationDataset,
    percentage::Float64; 
    seed::Int=1,
    bias::Float64=0.0, # bias to uniform
    replace::Bool=true,
    invert::Bool=false)

    @assert 0 ≤ percentage && percentage ≤ 1 "Percentage p must be 0 ≤ p ≤ 1."

    n_instances = length(ds.instances)
    n_instances_per_class = countmap(ds.classes)
    classes = keys(n_instances_per_class)
    n_classes = length(n_instances_per_class)

    instances_per_class = Dict{CategoricalArrays.CategoricalValue{String, UInt32}, Array{Int, 1}}()

    for class in classes
        instances_per_class[class] = ModalInstance[]
    end

    instances_to_sample = Dict{CategoricalArrays.CategoricalValue{String, UInt32}, Int64}()

    for class in classes
        sample_size = Int(round(percentage * ((1-bias)*n_instances_per_class[class] + bias*n_instances / n_classes)))
        instances_to_sample[class] = sample_size
    end

    for i in 1:n_instances
        push!(instances_per_class[ds.classes[i]], i)
    end

    r = Random.seed!(seed)

    output_classes = String[]
    timeseries_df = DataFrame()

    attrs = attributes(ds, 1) # TODO up to now, only timeseries
    
    for attr in attrs
        insertcols!(timeseries_df, attr => Array{Float64, 1}[]) # TODO also here Array{Float64, 1}[], i.e., timeseries
    end

    if replace
        for class in classes
            for i in 1:instances_to_sample[class]
                idx = instances_per_class[class][rand(r, 1:n_instances_per_class[class])]
                push!(timeseries_df, ds.instances[idx][1]) # ds.instances[idx][1] <-- [1] means the first frame TODO in future
                push!(output_classes, ds.classes[idx])
                # @show idx
                # @show ds.classes[idx]
            end
        end
    else
        for class in classes
            n_eligible = length(instances_per_class[class])
            selected = instances_per_class[class]

            # the selected elements are those at the end of then selected array
            for i in 1:instances_to_sample[class]
                idx = rand(r, 1:n_eligible)
                selected[idx], selected[n_eligible] = selected[n_eligible], selected[idx]
                n_eligible -= 1
            end

            # take the last instances_to_sample[class] instances
            if invert == false
                for i in n_instances_per_class[class]-instances_to_sample[class]+1:n_instances_per_class[class]
                    idx = selected[i]
                    push!(timeseries_df, ds.instances[idx][1]) # ds.instances[idx][1] <-- [1] means the first frame TODO in future
                    push!(output_classes, ds.classes[idx])
                end
            # take the first n_instances_per_class[class]-instances_to_sample[class] instances
            else
                for i in 1:n_instances_per_class[class]-instances_to_sample[class]
                    idx = selected[i]
                    push!(timeseries_df, ds.instances[idx][1]) # ds.instances[idx][1] <-- [1] means the first frame TODO in future
                    push!(output_classes, ds.classes[idx])
                end
            end
        end
    end

    return ClassificationDataset([ModalFrame(timeseries_df)], CategoricalArray(output_classes))
end

function paa(x::AbstractArray{T} where T <: Real;
        n_chunks::Union{Missing,Int}=missing,
        f::Function=mean,
        decdigits::Int=4,
        kwargs...)
    N = length(x)
    if ismissing(n_chunks) || n_chunks ≥ N return x
    else
        @assert 0 ≤ n_chunks && n_chunks ≤ N "The number of chunks must be in [0,$(N)]"

        y = Array{Float64}(undef, n_chunks) # TODO Float64?
        for i in 1:n_chunks
            l = Int(ceil((N*(i-1)/n_chunks) + 1))
            h = Int(ceil(N*i/n_chunks))
            y[i] = round(f(x[l:h]; kwargs...), digits=decdigits)
        end
        return y
    end
end

######################################################
const _ARFF_SPACE       = UInt8(' ')
const _ARFF_COMMENT     = UInt8('%')
const _ARFF_AT          = UInt8('@')
const _ARFF_SEP         = UInt8(',')
const _ARFF_NEWLINE     = UInt8('\n')
const _ARFF_NOMSTART    = UInt8('{')
const _ARFF_NOMEND      = UInt8('}')
const _ARFF_ESC         = UInt8('\\')
const _ARFF_MISSING     = UInt8('?')
const _ARFF_RELMARK     = UInt8('\'')

function readARFF(p::String)
    open(p, "r") do io
        df = DataFrame()
        classes = String[]
        lines = readlines(io)
        for i in 1:length(lines)
            line = lines[i]
            # If not empty line or comment
            if !isempty(line)
                if UInt8(line[1]) != _ARFF_COMMENT
                    sline = split(line, " ")
                    # println(sline[1][1])
                    # If the first symbol is @
                    if UInt8(sline[1][1]) == _ARFF_AT
                        # If @relation
                        if sline[1][2:end] == "relation"
                            # println("Relation: " * sline[2])
                        end

                        # if sline[1][2:end] == "attribute" && sline[2] == "class"
                        #     classes = sline[3][2:end-1]
                        #     println(classes)
                        # end
                    # data, first char is '
                    elseif UInt8(sline[1][1]) == _ARFF_RELMARK
                        sline[1] = sline[1][2:end]
                        data_and_class = split(sline[1],"\'")
                        string_data = split(data_and_class[1], "\\n")
                        class = data_and_class[2][2:end]
                        
                        if isempty(names(df))
                            for i in 1:length(string_data)
                                insertcols!(df, Symbol("A" * string(i)) => Array{Float64, 1}[]) # add the attributes as 1,2,3,ecc.
                            end
                        end

                        float_data = Dict{Int,Vector{Float64}}()

                        for i in 1:length(string_data)
                            float_data[i] = map(x->parse(Float64,x), split(string_data[i], ","))
                        end

                        # @show float_data

                        
                        push!(df, [float_data[i] for i in 1:length(string_data)])
                        push!(classes, class)
                        # @show data
                        # @show class
                    end
                end
            end
            #error(1)
        end
        return ClassificationDataset([ModalFrame(df)], CategoricalArray(classes))
    end
end