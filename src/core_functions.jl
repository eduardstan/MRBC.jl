##########################
# @@@ CORE FUNCTIONS @@@ #
##########################

function f̃(p::Proposition, vals::Vector{Float64})
    return sum(map(x -> p.rel(x, p.a), vals))/length(vals)
end

function ==̃ₕ(x::Int64, y::Int64; horizon::Int64=4)::Float64
    # |x-y| ≥ h
    if abs(x-y) ≥ horizon
        return float(0)
    # |x-y| < h
    else
        return (horizon - abs(x-y))/horizon
    end
end

function <̃ₕ(x::Int64, y::Int64; horizon::Int64=4)::Float64
    # y-x > h
    if y-x > horizon
        return float(1)
    # y ≦ x
    elseif y ≤ x
        return float(0)
    # y-x ≦ h
    elseif y-x ≤ horizon
        return (y-x)/horizon
    end
end

function R̃ₓ(rel::AbstractIntervalRelation, 
        w₁::NTuple{2, Int64}, 
        w₂::NTuple{2, Int64};
        horizon::Int64=1)::Float64
    x, y, w, z = w₁[1], w₁[2], w₂[1], w₂[2]
    # relations
    if eltype(rel) == :L
        return <̃ₕ(y, w; horizon)
    elseif eltype(rel) == :A
        return ==̃ₕ(y, w; horizon)
    elseif eltype(rel) == :O
        return ⊓(<̃ₕ(x, w; horizon), <̃ₕ(w, y; horizon), <̃ₕ(y, z; horizon))
    elseif eltype(rel) == :E
        return ⊓(<̃ₕ(x, w; horizon), ==̃ₕ(y, z; horizon))
    elseif eltype(rel) == :D
        return ⊓(<̃ₕ(x, w; horizon), <̃ₕ(z, y; horizon))
    elseif eltype(rel) == :B
        return ⊓(==̃ₕ(x, w; horizon), <̃ₕ(z, y; horizon))
    # inverse relations
    elseif eltype(rel) == :InvL
        return <̃ₕ(z, x; horizon)
    elseif eltype(rel) == :InvA
        return ==̃ₕ(z, x; horizon)
    elseif eltype(rel) == :InvO
        return ⊓(<̃ₕ(w, x; horizon), <̃ₕ(x, z; horizon), <̃ₕ(z, y; horizon))
    elseif eltype(rel) == :InvE
        return ⊓(<̃ₕ(w, x; horizon), ==̃ₕ(y, z; horizon))
    elseif eltype(rel) == :InvD
        return ⊓(<̃ₕ(w, x; horizon), <̃ₕ(y, z; horizon))
    elseif eltype(rel) == :InvB
        return ⊓(==̃ₕ(x, w; horizon), <̃ₕ(y, z; horizon))
    end
end

# global H = Dict{UInt, Dict{UInt, Dict{Int, Dict{NTuple{2,Int}, Float64}}}}()

function mc(I::ModalInstance, φ::ClassificationRule; minh::Int=1, maxh::Int=4, horizon::Int=4, memo::Bool=false)
    if !memo
        return _mc(I,φ; horizon)
    else 
        return _mc3(I,φ,H; minh=minh, maxh=maxh, horizon=horizon)
    end
end

function _mc2(I::ModalInstance, 
        φ::ClassificationRule, 
        H::Dict{UInt, Dict{UInt, Dict{Int, Dict{NTuple{2,Int}, Float64}}}}; 
        minh::Int=1,
        maxh::Int=4,
        horizon::Int=4)::Float64

    # @show φ

    # println("#1")
    if haskey(H, hash(_antecedent(φ))) && haskey(H[hash(_antecedent(φ))], hash(I)) && haskey(H[hash(_antecedent(φ))][hash(I)], horizon) && haskey(H[hash(_antecedent(φ))][hash(I)][horizon], (1,2))
        # println("##1")
        return H[hash(_antecedent(φ))][hash(I)][horizon][(1,2)]
    end

    sub = _collect_sorted_subformulas(φ)
    # L = Dict{Tuple{Union{Node{AbstractOperator}, Node{AbstractProposition}, Node{AbstractHeytingAlgebraValue}},NTuple{2,Int64}},Float64}()
    N = length(I, 1)  # TODO works only on the first (1) frame, i.e., time series
    intervals = [(x,y) for x in 1:N for y in 1:N if x < y]
    for (i,ψ) in enumerate(sub)

        # @show ψ

        # println("#2")
        # if haskey(H, hash(ψ)) && haskey(H[hash(ψ)],hash(I)) && haskey(H[hash(ψ)][hash(I)],horizon)
        #     # println("##2")
        #     continue
        # end

        # println("#3")
        if !haskey(H, hash(ψ))
            # println("##3")
            H[hash(ψ)] = Dict{UInt, Dict{Int, Dict{NTuple{2,Int}, Float64}}}()
            H[hash(ψ)][hash(I)] = Dict{Int, Dict{NTuple{2,Int}, Float64}}()
            for h in minh:maxh
                # println("###3")
                H[hash(ψ)][hash(I)][h] = Dict{NTuple{2,Int}, Float64}()
            end
        else
            # println("#4")
            if !haskey(H[hash(ψ)], hash(I))
                # println("##4")
                H[hash(ψ)][hash(I)] = Dict{Int, Dict{NTuple{2,Int}, Float64}}()
                for h in minh:maxh
                    # println("###4")
                    H[hash(ψ)][hash(I)][h] = Dict{NTuple{2,Int}, Float64}()
                end
            end
        end

        if typeof(ψ.data) <: AbstractProposition
            # println("#prop")
            for (x, y) ∈ intervals 
                v = f̃(ψ.data, I[1][ψ.data.A][x:y])
                for h in minh:maxh
                    H[hash(ψ)][hash(I)][h][(x,y)] = v
                end
            end

        elseif typeof(ψ.data) <: Float64
            # println("#float")
            for (x, y) ∈ intervals
                for h in minh:maxh
                    H[hash(ψ)][hash(I)][h][(x,y)] = ψ.data
                end
            end

        elseif typeof(ψ.data) == ExistentialIntervalRelation{:G}
            # println("#<G>")
            for h in minh:maxh
                # println("###<G>")
                S = Float64[]
                for op in [exIntRel(:L),exIntRel(:A),exIntRel(:O),exIntRel(:E),exIntRel(:D),exIntRel(:B),exIntRel(:InvL),exIntRel(:InvL),exIntRel(:InvA),exIntRel(:InvO),exIntRel(:InvE),exIntRel(:InvD),exIntRel(:InvB)]
                    # println("####<G>")
                    for (x, y) ∈ intervals
                        # println("#####<G>")
                        s = 0.0
                        for (w, z) ∈ intervals
                            # println("#######<G>")
                            # @show H[ψ.right][I][h][(w,z)]
                            # if !haskey(H, ψ.right)
                            #     v = _mc(I,ψ.right,H; minh, maxh, horizon)
                            # else
                            #     v = H[ψ.right][I][h][(w,z)]
                            # end
                            # if !haskey(H, ψ.right)
                            #     println("@@@@@ missing ψ.right = $(ψ.right) in H -- i = $i")
                            #     sf = _collect_sorted_subformulas(φ)
                            #     for ξ in sf
                            #         @show (i,ξ)
                            #     end
                            # end
                            s = ⊔(s, ⊓(R̃ₓ(op, (x, y), (w, z); horizon=h), H[hash(ψ.right)][hash(I)][h][(w,z)]))
                        end

                        push!(S, s)
                    end
                end
                # @show S
                # if !haskey(H, hash(ψ))
                #     println("@@@@@ missing ψ = $(ψ) in H -- i = $i")
                #     sf = _collect_sorted_subformulas(φ)
                #     for ξ in sf
                #         @show ξ
                #     end
                # end
                H[hash(ψ)][hash(I)][h][(1,2)] = ⊔(S...)
                # @show H[ψ][I][h][(1,2)]
            end
            # println("##<G>")
            return H[hash(ψ)][hash(I)][horizon][(1,2)]

        elseif typeof(ψ.data) == UniversalIntervalRelation{:G}
            println("#[G]")
            for h in minh:maxh
                S = Float64[]
                for op in [univIntRel(:L),univIntRel(:A),univIntRel(:O),univIntRel(:E),univIntRel(:D),univIntRel(:B),univIntRel(:InvL),univIntRel(:InvA),univIntRel(:InvO),univIntRel(:InvE),univIntRel(:InvD),univIntRel(:InvB)]
                    for (x, y) ∈ intervals
                        s = 1.0
                        for (w, z) ∈ intervals
                            s = ⊓(s, ↦(R̃ₓ(op, (x, y), (w, z); horizon=h), H[hash(ψ.right)][hash(I)][h][(w,z)]))
                        end
                        push!(S, s)
                    end
                end
                H[hash(ψ)][hash(I)][h][(1,2)] = ⊓(S...)
            end
            println("##[G]")
            return H[hash(ψ)][hash(I)][horizon][(1,2)]

        elseif typeof(ψ.data) <: AbstractExistentialIntervalRelation
            # println("#ex")
            for h in minh:maxh
                # println("###ex")
                for (x, y) ∈ intervals
                    # println("####ex")
                    s = 0.0
                    for (w, z) ∈ intervals
                        # println("######ex")
                        s = ⊔(s, ⊓(R̃ₓ(ψ.data, (x, y), (w, z); horizon=h), ⊓(H[hash(ψ.left)][hash(I)][h][(w,z)], H[hash(ψ.right)][hash(I)][h][(w,z)])))
                    end
                    # println("####ex")
                    # @show s
                    # @show typeof(s)
                    H[hash(ψ)][hash(I)][h][(x,y)] = s
                end
                # println("###ex")
            end
            # println("##ex")

        elseif typeof(ψ.data) <: AbstractUniversalIntervalRelation
            println("#univ")
            for h in minh:maxh
                for (x, y) ∈ intervals
                    s = 1.0
                    for (w, z) ∈ intervals
                        # @show H[ψ.left][I][h][(w,z)]
                        # @show typeof(H[ψ.left][I][h][(w,z)])
                        # @show H[ψ.right][I][h][(w,z)]
                        # @show typeof(H[ψ.right][I][h][(w,z)])
                        # @show R̃ₓ(ψ.data, (x, y), (w, z); horizon=h)
                        # @show typeof(R̃ₓ(ψ.data, (x, y), (w, z); horizon=h))
                        s = ⊓(s, ↦(R̃ₓ(ψ.data, (x, y), (w, z); horizon=h), ↦(H[hash(ψ.left)][hash(I)][h][(w,z)], H[hash(ψ.right)][hash(I)][h][(w,z)])))
                    end
                    H[hash(ψ)][hash(I)][h][(x,y)] = s
                end
            end
            println("##univ")
        elseif typeof(ψ.data) <: BinaryRelation{:∧}
            # println("#∧")
            for h in minh:maxh
                # println("###∧")
                for (x, y) in intervals
                    # println("####∧")
                    H[hash(ψ)][hash(I)][h][(x,y)] = ⊓(H[hash(ψ.left)][hash(I)][h][(x,y)], H[hash(ψ.right)][hash(I)][h][(x,y)])
                end
            end
            # println("##∧")
        elseif typeof(ψ.data) <: BinaryRelation{:→}
            # println("#→")
            for h in minh:maxh
                # println("###→")
                for (x, y) in intervals
                    # println("####→")
                    # @show H[ψ.left][I][h][(x,y)]
                    # @show typeof(H[ψ.left][I][h][(x,y)])
                    # @show H[ψ.right][I][h][(x,y)]
                    # @show typeof(H[ψ.right][I][h][(x,y)])
                    # error(1)
                    H[hash(ψ)][hash(I)][h][(x,y)] = ↦(H[hash(ψ.left)][hash(I)][h][(x,y)], H[hash(ψ.right)][hash(I)][h][(x,y)])
                end
            end
            # println("##→")
        end
    end
end

function _mc3(I::ModalInstance, 
    φ::ClassificationRule, 
    H::Dict{Tuple{UInt, UInt, UInt, Tuple{UInt, UInt}}, Float64}; 
    minh::Int=1,
    maxh::Int=4,
    horizon::Int=4)::Float64

    # @show φ

    if haskey(H, (hash(_antecedent(φ)), hash(I), hash(horizon), (hash(1), hash(2))))
        return H[(hash(_antecedent(φ)), hash(I), hash(horizon), (hash(1), hash(2)))]
    end
    # # println("#1")
    # if haskey(H, hash(_antecedent(φ))) && haskey(H[hash(_antecedent(φ))], hash(I)) && haskey(H[hash(_antecedent(φ))][hash(I)], horizon) && haskey(H[hash(_antecedent(φ))][hash(I)][horizon], (1,2))
    #     # println("##1")
    #     return H[hash(_antecedent(φ))][hash(I)][horizon][(1,2)]
    # end

    sub = _collect_sorted_subformulas(φ)
    L = Dict{Tuple{UInt,NTuple{2,Int64}},Float64}()
    N = length(I, 1)  # TODO works only on the first (1) frame, i.e., time series
    intervals = [(x,y) for x in 1:N for y in 1:N if x < y] # && y-x+1 ≤ log2(N)]
    for ψ in sub

        # # Subformula already checked?
        # if haskey(H, (hash(ψ), hash(I), hash(horizon), (hash(1), hash(2))))
        #     continue
        # end

        # @show ψ

        # println("#2")
        # if haskey(H, hash(ψ)) && haskey(H[hash(ψ)],hash(I)) && haskey(H[hash(ψ)][hash(I)],horizon)
        #     # println("##2")
        #     continue
        # end

        # # println("#3")
        # if !haskey(H, hash(ψ))
        #     # println("##3")
        #     H[hash(ψ)] = Dict{UInt, Dict{Int, Dict{NTuple{2,Int}, Float64}}}()
        #     H[hash(ψ)][hash(I)] = Dict{Int, Dict{NTuple{2,Int}, Float64}}()
        #     for h in minh:maxh
        #         # println("###3")
        #         H[hash(ψ)][hash(I)][h] = Dict{NTuple{2,Int}, Float64}()
        #     end
        # else
        #     # println("#4")
        #     if !haskey(H[hash(ψ)], hash(I))
        #         # println("##4")
        #         H[hash(ψ)][hash(I)] = Dict{Int, Dict{NTuple{2,Int}, Float64}}()
        #         for h in minh:maxh
        #             # println("###4")
        #             H[hash(ψ)][hash(I)][h] = Dict{NTuple{2,Int}, Float64}()
        #         end
        #     end
        # end

        if typeof(ψ.data) <: AbstractProposition
            # println("#prop")
            for (x, y) ∈ intervals 
                v = f̃(ψ.data, I[1][ψ.data.A][x:y])
                # for h in minh:maxh
                L[(hash(ψ), (x, y))] = v
                # end
            end

        elseif typeof(ψ.data) <: Float64
            # println("#float")
            for (x, y) ∈ intervals
                # for h in minh:maxh
                L[(hash(ψ), (x, y))] = ψ.data
                # end
            end

        elseif typeof(ψ.data) == ExistentialIntervalRelation{:G}
            # println("#<G>")
            # for h in minh:maxh
                # println("###<G>")
            S = Float64[]
            for op in [exIntRel(:L),exIntRel(:A),exIntRel(:O),exIntRel(:E),exIntRel(:D),exIntRel(:B),exIntRel(:InvL),exIntRel(:InvL),exIntRel(:InvA),exIntRel(:InvO),exIntRel(:InvE),exIntRel(:InvD),exIntRel(:InvB)]
                # println("####<G>")
                for (x, y) ∈ intervals
                    # println("#####<G>")
                    s = 0.0
                    for (w, z) ∈ intervals
                        # println("#######<G>")
                        # @show H[ψ.right][I][h][(w,z)]
                        # if !haskey(H, ψ.right)
                        #     v = _mc(I,ψ.right,H; minh, maxh, horizon)
                        # else
                        #     v = H[ψ.right][I][h][(w,z)]
                        # end
                        # if !haskey(H, ψ.right)
                        #     println("@@@@@ missing ψ.right = $(ψ.right) in H -- i = $i")
                        #     sf = _collect_sorted_subformulas(φ)
                        #     for ξ in sf
                        #         @show (i,ξ)
                        #     end
                        # end
                        s = ⊔(s, ⊓(R̃ₓ(op, (x, y), (w, z); horizon=horizon), L[(hash(ψ.right), (w, z))]))
                    end

                    push!(S, s)
                end
            end
            # @show S
            # if !haskey(H, hash(ψ))
            #     println("@@@@@ missing ψ = $(ψ) in H -- i = $i")
            #     sf = _collect_sorted_subformulas(φ)
            #     for ξ in sf
            #         @show ξ
            #     end
            # end
            # H[hash(ψ)][hash(I)][horizon][(1,2)] = ⊔(S...)
            v = ⊔(S...)
            H[(hash(ψ), hash(I), hash(horizon), (hash(1), hash(2)))] = v
                # @show H[ψ][I][h][(1,2)]
            # end
            # println("##<G>")
            return v

        elseif typeof(ψ.data) == UniversalIntervalRelation{:G}
            # println("#[G]")
            # for h in minh:maxh
            S = Float64[]
            for op in [univIntRel(:L),univIntRel(:A),univIntRel(:O),univIntRel(:E),univIntRel(:D),univIntRel(:B),univIntRel(:InvL),univIntRel(:InvA),univIntRel(:InvO),univIntRel(:InvE),univIntRel(:InvD),univIntRel(:InvB)]
                for (x, y) ∈ intervals
                    s = 1.0
                    for (w, z) ∈ intervals
                        s = ⊓(s, ↦(R̃ₓ(op, (x, y), (w, z); horizon=horizon), L[(hash(ψ.right), (w, z))]))
                    end
                    push!(S, s)
                end
            end
            v = ⊓(S...)
            # H[hash(ψ)][hash(I)][horizon][(1,2)] = ⊓(S...)
            H[(hash(ψ), hash(I), hash(horizon), (hash(1), hash(2)))] = v
            # end
            # println("##[G]")
            return v

        elseif typeof(ψ.data) <: AbstractExistentialIntervalRelation
            # println("#ex")
            # for h in minh:maxh
                # println("###ex")
            for (x, y) ∈ intervals
                # println("####ex")
                s = 0.0
                for (w, z) ∈ intervals
                    # println("######ex")
                    # s = ⊔(s, ⊓(R̃ₓ(ψ.data, (x, y), (w, z); horizon=horizon), ⊓(L[(hash(ψ.left), (w, z))], L[(hash(ψ.right), (w, z))])))
                    s = ⊔(s, ⊓(R̃ₓ(ψ.data, (x, y), (w, z); horizon=horizon), L[(hash(ψ.right), (w, z))]))
                end
                # println("####ex")
                # @show s
                # @show typeof(s)
                L[(hash(ψ), (x, y))] = s
            end
                # println("###ex")
            # end
            # println("##ex")

        elseif typeof(ψ.data) <: AbstractUniversalIntervalRelation
            # println("#univ")
            # for h in minh:maxh
            for (x, y) ∈ intervals
                s = 1.0
                for (w, z) ∈ intervals
                    # @show H[ψ.left][I][h][(w,z)]
                    # @show typeof(H[ψ.left][I][h][(w,z)])
                    # @show H[ψ.right][I][h][(w,z)]
                    # @show typeof(H[ψ.right][I][h][(w,z)])
                    # @show R̃ₓ(ψ.data, (x, y), (w, z); horizon=h)
                    # @show typeof(R̃ₓ(ψ.data, (x, y), (w, z); horizon=h))
                    # s = ⊓(s, ↦(R̃ₓ(ψ.data, (x, y), (w, z); horizon=horizon), ↦(L[(hash(ψ.left), (w, z))], L[(hash(ψ.right), (w, z))])))
                    s = ⊓(s, ↦(R̃ₓ(ψ.data, (x, y), (w, z); horizon=horizon), L[(hash(ψ.right), (w, z))]))
                end
                L[(hash(ψ), (x, y))] = s
            end
            # end
            # println("##univ")
        elseif typeof(ψ.data) <: BinaryRelation{:∧}
            # println("#∧")
            # for h in minh:maxh
                # println("###∧")
            for (x, y) in intervals
                # println("####∧")
                L[(hash(ψ), (x, y))] = ⊓(L[(hash(ψ.left), (x, y))], L[(hash(ψ.right), (x, y))])
            end
            # end
            # println("##∧")
        elseif typeof(ψ.data) <: BinaryRelation{:→}
            # println("#→")
            # for h in minh:maxh
                # println("###→")
            for (x, y) in intervals
                # println("####→")
                # @show H[ψ.left][I][h][(x,y)]
                # @show typeof(H[ψ.left][I][h][(x,y)])
                # @show H[ψ.right][I][h][(x,y)]
                # @show typeof(H[ψ.right][I][h][(x,y)])
                # error(1)
                L[(hash(ψ), (x, y))] = ↦(L[(hash(ψ.left), (x, y))], L[(hash(ψ.right), (x, y))])
            end
            # end
            # println("##→")
        end
    end
    return H[hash(_antecedent(φ))][hash(I)][horizon][(1,2)]
end

function _mc(I::ModalInstance, 
        φ::ClassificationRule, 
        H::Dict{UInt, Dict{UInt, Dict{Int, Dict{NTuple{2,Int}, Float64}}}}; 
        minh::Int=1,
        maxh::Int=4,
        horizon::Int=4)::Float64

    # @show φ

    # println("#1")
    if haskey(H, hash(_antecedent(φ))) && haskey(H[hash(_antecedent(φ))], hash(I)) && haskey(H[hash(_antecedent(φ))][hash(I)], horizon) && haskey(H[hash(_antecedent(φ))][hash(I)][horizon], (1,2))
        # println("##1")
        return H[hash(_antecedent(φ))][hash(I)][horizon][(1,2)]
    end

    sub = _collect_sorted_subformulas(φ)
    L = Dict{Tuple{UInt,NTuple{2,Int64}},Float64}()
    N = length(I, 1)  # TODO works only on the first (1) frame, i.e., time series
    intervals = [(x,y) for x in 1:N for y in 1:N if x < y]
    for (i,ψ) in enumerate(sub)

        # @show ψ

        # println("#2")
        # if haskey(H, hash(ψ)) && haskey(H[hash(ψ)],hash(I)) && haskey(H[hash(ψ)][hash(I)],horizon)
        #     # println("##2")
        #     continue
        # end

        # println("#3")
        if !haskey(H, hash(ψ))
            # println("##3")
            H[hash(ψ)] = Dict{UInt, Dict{Int, Dict{NTuple{2,Int}, Float64}}}()
            H[hash(ψ)][hash(I)] = Dict{Int, Dict{NTuple{2,Int}, Float64}}()
            for h in minh:maxh
                # println("###3")
                H[hash(ψ)][hash(I)][h] = Dict{NTuple{2,Int}, Float64}()
            end
        else
            # println("#4")
            if !haskey(H[hash(ψ)], hash(I))
                # println("##4")
                H[hash(ψ)][hash(I)] = Dict{Int, Dict{NTuple{2,Int}, Float64}}()
                for h in minh:maxh
                    # println("###4")
                    H[hash(ψ)][hash(I)][h] = Dict{NTuple{2,Int}, Float64}()
                end
            end
        end

        if typeof(ψ.data) <: AbstractProposition
            # println("#prop")
            for (x, y) ∈ intervals 
                v = f̃(ψ.data, I[1][ψ.data.A][x:y])
                # for h in minh:maxh
                L[(hash(ψ), (x, y))] = v
                # end
            end

        elseif typeof(ψ.data) <: Float64
            # println("#float")
            for (x, y) ∈ intervals
                # for h in minh:maxh
                L[(hash(ψ), (x, y))] = ψ.data
                # end
            end

        elseif typeof(ψ.data) == ExistentialIntervalRelation{:G}
            # println("#<G>")
            # for h in minh:maxh
                # println("###<G>")
            S = Float64[]
            for op in [exIntRel(:L),exIntRel(:A),exIntRel(:O),exIntRel(:E),exIntRel(:D),exIntRel(:B),exIntRel(:InvL),exIntRel(:InvL),exIntRel(:InvA),exIntRel(:InvO),exIntRel(:InvE),exIntRel(:InvD),exIntRel(:InvB)]
                # println("####<G>")
                for (x, y) ∈ intervals
                    # println("#####<G>")
                    s = 0.0
                    for (w, z) ∈ intervals
                        # println("#######<G>")
                        # @show H[ψ.right][I][h][(w,z)]
                        # if !haskey(H, ψ.right)
                        #     v = _mc(I,ψ.right,H; minh, maxh, horizon)
                        # else
                        #     v = H[ψ.right][I][h][(w,z)]
                        # end
                        # if !haskey(H, ψ.right)
                        #     println("@@@@@ missing ψ.right = $(ψ.right) in H -- i = $i")
                        #     sf = _collect_sorted_subformulas(φ)
                        #     for ξ in sf
                        #         @show (i,ξ)
                        #     end
                        # end
                        s = ⊔(s, ⊓(R̃ₓ(op, (x, y), (w, z); horizon=horizon), L[(hash(ψ.right), (w, z))]))
                    end

                    push!(S, s)
                end
            end
            # @show S
            # if !haskey(H, hash(ψ))
            #     println("@@@@@ missing ψ = $(ψ) in H -- i = $i")
            #     sf = _collect_sorted_subformulas(φ)
            #     for ξ in sf
            #         @show ξ
            #     end
            # end
            H[hash(ψ)][hash(I)][horizon][(1,2)] = ⊔(S...)
                # @show H[ψ][I][h][(1,2)]
            # end
            # println("##<G>")
            return H[hash(ψ)][hash(I)][horizon][(1,2)]

        elseif typeof(ψ.data) == UniversalIntervalRelation{:G}
            # println("#[G]")
            # for h in minh:maxh
            S = Float64[]
            for op in [univIntRel(:L),univIntRel(:A),univIntRel(:O),univIntRel(:E),univIntRel(:D),univIntRel(:B),univIntRel(:InvL),univIntRel(:InvA),univIntRel(:InvO),univIntRel(:InvE),univIntRel(:InvD),univIntRel(:InvB)]
                for (x, y) ∈ intervals
                    s = 1.0
                    for (w, z) ∈ intervals
                        s = ⊓(s, ↦(R̃ₓ(op, (x, y), (w, z); horizon=horizon), L[(hash(ψ.right), (w, z))]))
                    end
                    push!(S, s)
                end
            end
            H[hash(ψ)][hash(I)][horizon][(1,2)] = ⊓(S...)
            # end
            # println("##[G]")
            return H[hash(ψ)][hash(I)][horizon][(1,2)]

        elseif typeof(ψ.data) <: AbstractExistentialIntervalRelation
            # println("#ex")
            # for h in minh:maxh
                # println("###ex")
            for (x, y) ∈ intervals
                # println("####ex")
                s = 0.0
                for (w, z) ∈ intervals
                    # println("######ex")
                    s = ⊔(s, ⊓(R̃ₓ(ψ.data, (x, y), (w, z); horizon=horizon), ⊓(L[(hash(ψ.left), (w, z))], L[(hash(ψ.right), (w, z))])))
                end
                # println("####ex")
                # @show s
                # @show typeof(s)
                L[(hash(ψ), (x, y))] = s
            end
                # println("###ex")
            # end
            # println("##ex")

        elseif typeof(ψ.data) <: AbstractUniversalIntervalRelation
            # println("#univ")
            # for h in minh:maxh
            for (x, y) ∈ intervals
                s = 1.0
                for (w, z) ∈ intervals
                    # @show H[ψ.left][I][h][(w,z)]
                    # @show typeof(H[ψ.left][I][h][(w,z)])
                    # @show H[ψ.right][I][h][(w,z)]
                    # @show typeof(H[ψ.right][I][h][(w,z)])
                    # @show R̃ₓ(ψ.data, (x, y), (w, z); horizon=h)
                    # @show typeof(R̃ₓ(ψ.data, (x, y), (w, z); horizon=h))
                    s = ⊓(s, ↦(R̃ₓ(ψ.data, (x, y), (w, z); horizon=horizon), ↦(L[(hash(ψ.left), (w, z))], L[(hash(ψ.right), (w, z))])))
                end
                L[(hash(ψ), (x, y))] = s
            end
            # end
            # println("##univ")
        elseif typeof(ψ.data) <: BinaryRelation{:∧}
            # println("#∧")
            # for h in minh:maxh
                # println("###∧")
            for (x, y) in intervals
                # println("####∧")
                L[(hash(ψ), (x, y))] = ⊓(L[(hash(ψ.left), (x, y))], L[(hash(ψ.right), (x, y))])
            end
            # end
            # println("##∧")
        elseif typeof(ψ.data) <: BinaryRelation{:→}
            # println("#→")
            # for h in minh:maxh
                # println("###→")
            for (x, y) in intervals
                # println("####→")
                # @show H[ψ.left][I][h][(x,y)]
                # @show typeof(H[ψ.left][I][h][(x,y)])
                # @show H[ψ.right][I][h][(x,y)]
                # @show typeof(H[ψ.right][I][h][(x,y)])
                # error(1)
                L[(hash(ψ), (x, y))] = ↦(L[(hash(ψ.left), (x, y))], L[(hash(ψ.right), (x, y))])
            end
            # end
            # println("##→")
        end
    end
    return H[hash(_antecedent(φ))][hash(I)][horizon][(1,2)]
end

function _mc(I::ModalInstance, φ::ClassificationRule; horizon::Int=4)
    sub = _collect_sorted_subformulas(φ)
    L = Dict{Tuple{UInt,NTuple{2,Int64}},Float64}()
    N = length(I, 1)  # TODO works only on the first (1) frame, i.e., time series
    intervals = [(x,y) for x in 1:N for y in 1:N if x < y]
    for ψ ∈ sub
        if typeof(ψ.data) <: AbstractProposition
            for (x, y) ∈ intervals # setdiff(𝕀, [(-1, 0)])
                # L[(ψ, (x, y))] = ((x, y) != (-1, 0) ? f̃(ψ.data, ts[ψ.data.attribute][x:y]) : 0.0) # TODO is (-1, 0) needed for a proposition? Perhaps not
                L[(hash(ψ), (x ,y))] = f̃(ψ.data, I[1][ψ.data.A][x:y]) # TODO works only for time series
            end
        elseif typeof(ψ.data) <: Float64
            for (x, y) ∈ intervals # setdiff(𝕀,[(-1, 0)])
                L[(hash(ψ), (x, y))] = ψ.data
            end
        elseif typeof(ψ.data) == ExistentialIntervalRelation{:G}
            S = Float64[]
            for op in [exIntRel(:L),exIntRel(:A),exIntRel(:O),exIntRel(:E),exIntRel(:D),exIntRel(:B),exIntRel(:InvL),exIntRel(:InvL),exIntRel(:InvA),exIntRel(:InvO),exIntRel(:InvE),exIntRel(:InvD),exIntRel(:InvB)]
                for (x, y) ∈ intervals
                    s = 0.0
                    for (w, z) ∈ intervals # setdiff(𝕀,[(-1, 0)])
                        s = ⊔(s, ⊓(R̃ₓ(op, (x, y), (w, z); horizon=horizon), L[(hash(ψ.right), (w, z))]))
                    end
                    push!(S, s)
                end
            end
            return ⊔(S...)
        elseif typeof(ψ.data) == UniversalIntervalRelation{:G}
            S = Float64[]
            for op in [univIntRel(:L),univIntRel(:A),univIntRel(:O),univIntRel(:E),univIntRel(:D),univIntRel(:B),univIntRel(:InvL),univIntRel(:InvA),univIntRel(:InvO),univIntRel(:InvE),univIntRel(:InvD),univIntRel(:InvB)]
                for (x, y) ∈ intervals
                    s = 1.0
                    for (w, z) ∈ intervals # setdiff(𝕀,[(-1, 0)])
                        s = ⊓(s, ↦(R̃ₓ(op, (x, y), (w, z); horizon=horizon), L[(hash(ψ.right), (w, z))]))
                    end
                    push!(S, s)
                end
            end
            return ⊓(S...)
        elseif typeof(ψ.data) <: AbstractExistentialIntervalRelation
            for (x, y) ∈ intervals # (typeof(ψ.data) === ExistentialLater ? 𝕀 : setdiff(𝕀,[(-1, 0)]))
                s = 0.0
                for (w, z) ∈ intervals # setdiff(𝕀,[(-1, 0)])
                    s = ⊔(s, ⊓(R̃ₓ(ψ.data, (x, y), (w, z); horizon=horizon), ⊓(L[(hash(ψ.left), (w, z))], L[(hash(ψ.right), (w, z))])))
                end
                L[(hash(ψ), (x, y))] = s
            end
        elseif typeof(ψ.data) <: AbstractUniversalIntervalRelation
            for (x, y) ∈ intervals # (typeof(ψ.data) === UniversalLater ? 𝕀 : setdiff(𝕀,[(-1,0)]))
                s = 1.0
                for (w, z) ∈ intervals # setdiff(𝕀,[(-1,0)])
                    s = ⊓(s, ↦(R̃ₓ(ψ.data, (x, y), (w, z); horizon=horizon), ↦(L[(hash(ψ.left), (w, z))], L[(hash(ψ.right), (w, z))])))
                end
                L[(hash(ψ), (x, y))] = s
            end
        elseif typeof(ψ.data) <: BinaryRelation{:∧}
            for (x, y) in intervals
                # println("####∧")
                L[(hash(ψ), (x, y))] = ⊓(L[(hash(ψ.left), (x, y))], L[(hash(ψ.right), (x, y))])
            end
        elseif typeof(ψ.data) <: BinaryRelation{:→}
            for (x, y) in intervals
                # println("####∧")
                L[(hash(ψ), (x, y))] = ↦(L[(hash(ψ.left), (x, y))], L[(hash(ψ.right), (x, y))])
            end
        end
    end
end


function mmmc(ℐ::ClassificationDataset, Γ::ClassificationRules)
    accuracy = 0.0

    Threads.@threads for i in 1:length(ℐ.instances)
        I = ℐ.instances[i]
        class = ℐ.classes[i]
        L = Dict{ClassificationRule, Float64}() # ρ ↦ truth value

        for ρ in Γ
            # lock(lk) do
            L[ρ] = mc(I, ρ; minh=Γ.minh, maxh=Γ.maxh, horizon=Γ.horizon, memo=true)
            # end
        end

        maxv = argmax(L)
        maxv_rules = [k for (k,v) ∈ L if v == L[maxv]]
        pred = _consequent(maxv_rules[rand(1:length(maxv_rules))])

        if pred == class
            accuracy = accuracy + 1.0
        end
    end
    return accuracy/length(ℐ)
end

function z(Γ::ClassificationRules)
    round(mmmc(Γ.ds, Γ), digits=4), 1.0 # round(1 - _complexity(Γ)/_maxsize(Γ), digits=4) # 1 - sum(sizes)/(max_size*maxr) #1 - sum(sizes)/(max_size*100) # (mean_size(crules)/max_size) * (length(classes(td))/length(rules(crules))) # 1.0 - sum(sizes)/(max_size*12)
end

function CV(Γ::ClassificationRules)
    return abs(length(Γ.ds.unique_classes) - length(_classes(Γ)))
end

function init()
    # ex_relations    = [exIntRel(s) for s in [:L,:A,:O,:E,:D,:B,:InvL,:InvA,:InvO,:InvE,:InvD,:InvB]]
    # univ_relations  = [univIntRel(s) for s in [:L,:A,:O,:E,:D,:B,:InvL,:InvA,:InvO,:InvE,:InvD,:InvB]]
    # init_relations  = [exIntRel(:G),univIntRel(:G)]
    # prop_relations  = [conjunction, implication]
    # relations       = vcat(ex_relations, univ_relations,prop_relations)
    # _rand_rules(init_relations,relations,HeytingChainAlgebra,train,[≤,<,==,>,≥]; minh=1,maxh=3,maxmd=4,maxdepth=6,minnumrules=4,maxnumrules=48)
    ex_relations    = [exIntRel(s) for s in [:L,:A,:O,:E,:D,:B,:InvL,:InvA,:InvO,:InvE,:InvD,:InvB]]
    univ_relations  = [univIntRel(s) for s in [:L,:A,:O,:E,:D,:B,:InvL,:InvA,:InvO,:InvE,:InvD,:InvB]]
    init_relations  = [exIntRel(:G),univIntRel(:G)]
    prop_relations  = [conjunction]
    relations       = vcat(ex_relations, univ_relations)
    _rand_rules(init_relations,relations,prop_relations,HeytingChainAlgebra,train,[≤,<,==,>,≥]; minh=1,maxh=3,maxmd=3,maxdepth=10,minnumrules=4,maxnumrules=48)
end

function hypervolume_indicator(y::Vector{Tuple{Float64,Float64}}; refpoint=(0,0))
    sort!(y, by=x->x[1], rev=true, alg=QuickSort)

    hi = 0.0
    # (index, tuple)
    for (i, t) in enumerate(y)
        # if it is the first, then
        if i == 1
            hi += abs(t[1] - refpoint[1]) * abs(t[2] - refpoint[2])
        else
            s = y[i-1]
            hi += abs(t[1] - refpoint[1]) * abs(t[2] - s[2])
        end
    end

    return round(hi, digits=4)
end

function _maxsize(Γ::ClassificationRules)
    maxmd = Γ.maxmd
    maxnumrules = Γ.maxnumrules
    maxdepth = Γ.maxdepth

    # 1 for the root
    # 2^(maxmd-1) for the nodes with modalities, -1 for the root; e.g., ◊(φ ∧ φ)
    # 
    return (1 + 2*(2^(maxmd-1) - 1) + 2^(maxdepth-maxmd) + 2^(maxmd-1) + 2) * maxnumrules #(1 + 2 + (maxmd > 2 ? ((maxmd-1) * 2^(maxmd-2)) : 0 ) + 2^(maxmd-1))*maxnumrules + maxnumrules*2
end

#################################
# @@@ End of CORE FUNCTIONS @@@ #
#################################