##
# Abstract relation.
abstract type AbstractRelation end

##
# Abstract proposotional relation; it is used for completeness.
abstract type AbstractPropositionalRelation     <: AbstractRelation end
# Abstract modal relation; e.g., existential relation ◊.
abstract type AbstractModalRelation             <: AbstractRelation end
# Abstract binary relation; e.g., conjunction ∧.
abstract type AbstractBinaryRelation            <: AbstractRelation end

##
# Abstract temporal relation.
abstract type AbstractTemporalRelation          <: AbstractModalRelation end
# Abstract spatial relation.
abstract type AbstractSpatialRelation           <: AbstractModalRelation end
# Abstract spatio-temporal relation.
abstract type AbstractSpatioTemporalRelation    <: AbstractModalRelation end

##
# Abstract point (temporal) relation.
abstract type AbstractPointRelation             <: AbstractTemporalRelation end
# Abstract interval (temporal) relation.
abstract type AbstractIntervalRelation          <: AbstractTemporalRelation end
# Abstract rectangle (spatial) relation.
abstract type AbstractRectangleRelation         <: AbstractSpatialRelation end
# Abstract cube (spatio-temporal) relation.
abstract type AbstractCubeRelation              <: AbstractSpatioTemporalRelation end

# Using the singleton design pattern with parametric (data) type; similar to Val{T}.
struct BinaryRelation{T}            <: AbstractBinaryRelation end
BinaryRelation(s::AbstractString)   = BinaryRelation{Symbol(s)}()
BinaryRelation(s::Symbol)           = BinaryRelation{s}()

const conjunction = BinaryRelation(:∧)
const disjunction = BinaryRelation(:∨)
const implication = BinaryRelation(:→)

show(io::IO, ::BinaryRelation{:∧})  = print(io, "∧")
show(io::IO, ::BinaryRelation{:∨})  = print(io, "∨")
show(io::IO, ::BinaryRelation{:→})  = print(io, "→")

### Interval relations

##
# Abstract existential interval (temporal) relation.
abstract type AbstractExistentialIntervalRelation   <: AbstractIntervalRelation end
# Abstract universal interval (temporal) relation.
abstract type AbstractUniversalIntervalRelation     <: AbstractIntervalRelation end

##
# 
# struct IdentityIntervalRelation         <: AbstractIntervalRelation end

# Using the singleton design pattern with parametric (data) type; similar to Val{T}.
struct ExistentialIntervalRelation{T}           <: AbstractExistentialIntervalRelation end
ExistentialIntervalRelation(s::AbstractString)  = ExistentialIntervalRelation{Symbol(s)}()
ExistentialIntervalRelation(s::Symbol)          = ExistentialIntervalRelation{s}()

struct UniversalIntervalRelation{T}             <: AbstractUniversalIntervalRelation end
UniversalIntervalRelation(s::AbstractString)    = UniversalIntervalRelation{Symbol(s)}()
UniversalIntervalRelation(s::Symbol)            = UniversalIntervalRelation{s}()

const exIntRel(s::AbstractString)   = ExistentialIntervalRelation(s)
const exIntRel(s::Symbol)           = ExistentialIntervalRelation(s)
const univIntRel(s::AbstractString) = UniversalIntervalRelation(s)
const univIntRel(s::Symbol)         = UniversalIntervalRelation(s)

eltype(::ExistentialIntervalRelation{T}) where T    = T
eltype(::UniversalIntervalRelation{T}) where T      = T

# const exG       = ExistentialIntervalRelation(:G)
# const exL       = ExistentialIntervalRelation(:L)
# const exA       = ExistentialIntervalRelation(:A)
# const exO       = ExistentialIntervalRelation(:O)
# const exE       = ExistentialIntervalRelation(:E)
# const exD       = ExistentialIntervalRelation(:D)
# const exB       = ExistentialIntervalRelation(:B)
# const exInvL    = ExistentialIntervalRelation(:InvL)
# const exInvA    = ExistentialIntervalRelation(:InvA)
# const exInvO    = ExistentialIntervalRelation(:InvO)
# const exInvE    = ExistentialIntervalRelation(:InvE)
# const exInvD    = ExistentialIntervalRelation(:InvD)
# const exInvB    = ExistentialIntervalRelation(:InvB)

# const univG     = UniversalIntervalRelation(:G)
# const univL     = UniversalIntervalRelation(:L)
# const univA     = UniversalIntervalRelation(:A)
# const univO     = UniversalIntervalRelation(:O)
# const univE     = UniversalIntervalRelation(:E)
# const univD     = UniversalIntervalRelation(:D)
# const univB     = UniversalIntervalRelation(:B)
# const univInvL  = UniversalIntervalRelation(:InvL)
# const univInvA  = UniversalIntervalRelation(:InvA)
# const univInvO  = UniversalIntervalRelation(:InvO)
# const univInvE  = UniversalIntervalRelation(:InvE)
# const univInvD  = UniversalIntervalRelation(:InvD)
# const univInvB  = UniversalIntervalRelation(:InvB)

##
# show(io::IO, ::IdentityIntervalRelation)           = print(io, "|=|")

show(io::IO, ::ExistentialIntervalRelation{:G})    = print(io, "⟨G⟩")
show(io::IO, ::ExistentialIntervalRelation{:L})    = print(io, "⟨L⟩")
show(io::IO, ::ExistentialIntervalRelation{:A})    = print(io, "⟨A⟩")
show(io::IO, ::ExistentialIntervalRelation{:O})    = print(io, "⟨O⟩")
show(io::IO, ::ExistentialIntervalRelation{:E})    = print(io, "⟨E⟩")
show(io::IO, ::ExistentialIntervalRelation{:D})    = print(io, "⟨D⟩")
show(io::IO, ::ExistentialIntervalRelation{:B})    = print(io, "⟨B⟩")
show(io::IO, ::ExistentialIntervalRelation{:InvL}) = print(io, "⟨InvL⟩")
show(io::IO, ::ExistentialIntervalRelation{:InvA}) = print(io, "⟨InvA⟩")
show(io::IO, ::ExistentialIntervalRelation{:InvO}) = print(io, "⟨InvO⟩")
show(io::IO, ::ExistentialIntervalRelation{:InvE}) = print(io, "⟨InvE⟩")
show(io::IO, ::ExistentialIntervalRelation{:InvD}) = print(io, "⟨InvD⟩")
show(io::IO, ::ExistentialIntervalRelation{:InvB}) = print(io, "⟨InvB⟩")

show(io::IO, ::UniversalIntervalRelation{:G})      = print(io, "[G]")
show(io::IO, ::UniversalIntervalRelation{:L})      = print(io, "[L]")
show(io::IO, ::UniversalIntervalRelation{:A})      = print(io, "[A]")
show(io::IO, ::UniversalIntervalRelation{:O})      = print(io, "[O]")
show(io::IO, ::UniversalIntervalRelation{:E})      = print(io, "[E]")
show(io::IO, ::UniversalIntervalRelation{:D})      = print(io, "[D]")
show(io::IO, ::UniversalIntervalRelation{:B})      = print(io, "[B]")
show(io::IO, ::UniversalIntervalRelation{:InvL})   = print(io, "[InvL]")
show(io::IO, ::UniversalIntervalRelation{:InvA})   = print(io, "[InvA]")
show(io::IO, ::UniversalIntervalRelation{:InvO})   = print(io, "[InvO]")
show(io::IO, ::UniversalIntervalRelation{:InvE})   = print(io, "[InvE]")
show(io::IO, ::UniversalIntervalRelation{:InvD})   = print(io, "[InvD]")
show(io::IO, ::UniversalIntervalRelation{:InvB})   = print(io, "[InvB]")

####################
# @@@ ALGEBRAS @@@ #
####################

# Abstract Algebra.
abstract type AbstractAlgebra end

# (Generic) Algebra.
struct Algebra <: AbstractAlgebra
    name::String
    domain::Vector{Float64}
    meet::Function
    join::Function
    impl::Function
    # eq::Function
    # lt::Function
    bottom::Float64
    top::Float64
end

_name(a::Algebra)   = a.name
_domain(a::Algebra) = a.domain
_meet(a::Algebra)   = a.meet
_join(a::Algebra)   = a.join
_impl(a::Algebra)   = a.impl
# _eq(a::Algebra)     = a.eq
# _lt(a::Algebra)     = a.lt
_bottom(a::Algebra) = a.bottom
_top(a::Algebra)    = a.top

# Boolean and: ∧ (\wedge).
∧(a::Bool,b::Bool)          = ifelse(a == true && a == b, true, false)
∧(a::Int64,b::Int64)        = ifelse(a == 1 && a == b, 1, 0)
∧(a::Float64,b::Float64)    = ifelse(a == 1.0 && a == b, 1.0, 0.0)
∧(a,b,args...)              = ∧(∧(a,b),args...)

# Boolean or: ∨ (\vee).
∨(a::Bool,b::Bool)          = ifelse(a == false && a == b, false, true)
∨(a::Int64,b::Int64)        = ifelse(a == 0 && a == b, 0, 1)
∨(a::Float64,b::Float64)    = ifelse(a == 0.0 && a == b, 0.0, 1.0)
∨(a,b,args...)              = ∨(∨(a,b),args...)

# Boolean implication: → (\to).
→(a::Bool,b::Bool)          = ifelse(a == true && b == false, false, true)
→(a::Int64,b::Int64)        = ifelse(a == 1 && b == 0, 0, 1)
→(a::Float64,b::Float64)    = ifelse(a == 1.0 && b == 0.0, 0.0, 1.0)
→(a,b,args...)              = →(→(a,b),args...)

# Heyting chain meet: ⊓ (\sqcap).
⊓(args...)                 = min(args...)

# Heyting chain join: ⊔ (\sqcup).
⊔(args...)                 = max(args...)

# Heyting chain implication: ↦ (\mapsto).
(↦)(a::Float64, b::Float64) = a ≤ b ? float(1) : b

const BooleanAlgebra = Algebra("Boolean Algebra",[0.0,1.0],∧,∨,→,0.0,1.0)
const HeytingChainAlgebra = Algebra("Heyting Chain Algebra", [x for x ∈ 0.0:0.1:1.0],min,max,↦,0.0,1.0) # TODO: 0.0:0.1:1.0 is good as range ?

###########################
# @@@ End of ALGEBRAS @@@ #
###########################

########################
# @@@ PROPOSITIONS @@@ #
########################

abstract type AbstractProposition end

struct Proposition <: AbstractProposition
    A::Int
    rel::Function
    a::Float64
end

show(io::IO, p::Proposition) = print(io, "A", p.A, p.rel, p.a)

###############################
# @@@ End of PROPOSITIONS @@@ #
###############################

#########################
# @@@ FORMULA NODES @@@ #
#########################

abstract type Node end

mutable struct FormulaNode{T} <: Node
    data::T
    parent::FormulaNode
    left::FormulaNode
    right::FormulaNode

    # Root constructor.
    FormulaNode{T}(data::T) where T = new{T}(data)
end

FormulaNode(data::T) where T = FormulaNode{T}(data)
_datatype(::FormulaNode{T}) where T = T
_parent(ν::FormulaNode) = ν.parent 
_left(ν::FormulaNode) = ν.left
_right(ν::FormulaNode) = ν.right
_parent!(ν::FormulaNode, p::FormulaNode) = ν.parent = p
_left!(ν::FormulaNode, l::FormulaNode) = ν.left = l
_right!(ν::FormulaNode, r::FormulaNode) = ν.right = r

function _size(ν::FormulaNode)
    leftsz = isdefined(ν, :left) ? _size(ν.left) : 0
    rightsz = isdefined(ν, :right) ? _size(ν.right) : 0
    # Modal relations count +2 because they are, e.g., ◊(φ₁ ∧ φ₂) so that +1 for the ◊ and +1 for the ∧;
    # then recursively compute the size for φ₁ and φ₂.
    sz = 0
    if typeof(ν.data) != ExistentialIntervalRelation{:G} && typeof(ν.data) != UniversalIntervalRelation{:G} && _datatype(ν) <: AbstractModalRelation
        sz = sz + 2
    else
        sz = sz + 1
    end
    return sz + leftsz + rightsz
end

function _upwards_md(ν::FormulaNode)
    md = isdefined(ν, :parent) ? _upwards_md(ν.parent) : 0
    md += _datatype(ν) <: AbstractModalRelation ? 1 : 0
    return md
end

function _upwards_depth(ν::FormulaNode)
    depth = isdefined(ν, :parent) ? _upwards_depth(_parent(ν)) : 0
    return depth + 1 
end

function _isleaf(ν::FormulaNode)
    return (!isdefined(ν, :left) && !isdefined(ν, :right) ? true : false)
end

################################
# @@@ End of FORMULA NODES @@@ #
################################

################################
# @@@ CLASSIFICATION RULES @@@ #
################################

# Abstract rule.
abstract type AbstractRule end

# Classification rule.
mutable struct ClassificationRule <: AbstractRule
    antecedent::FormulaNode
    consequent::String
end

_antecedent(ρ::ClassificationRule)                  = ρ.antecedent
_consequent(ρ::ClassificationRule)                  = ρ.consequent
_antecedent!(ρ::ClassificationRule, a::FormulaNode) = ρ.antecedent = a
_consequent!(ρ::ClassificationRule, c::String)      = ρ.consequent = c

_size(ρ::ClassificationRule) = _size(_antecedent(ρ)) + 2 # +2 for the ⟹ and the class.

function _print_rule(io::IO, ρ::ClassificationRule)
    _print_antecedent(io, _antecedent(ρ))
    print(io, " ⟹  ")
    _print_consequent(io, _consequent(ρ))
    println(io)
    return
end

function _print_antecedent(io::IO, ν::FormulaNode)
    if _datatype(ν) <: AbstractUniversalIntervalRelation # TODO: use traits to ask isexistential(ν) or isuniversal(ν)
        print(io, ν.data)
        print(io, "(")
        # if isdefined(ν, :left) && isdefined(ν, :right)
        #     _print_antecedent(io, ν.left)
        #     print(io, " ", implication, " ")
        #     _print_antecedent(io, ν.right)
        # elseif isdefined(ν, :right)
        #     _print_antecedent(io, ν.right)
        # end
        _print_antecedent(io, ν.right)
        print(io, ")")
        return
    elseif _datatype(ν) <: AbstractExistentialIntervalRelation # TODO
        print(io, ν.data)
        print(io, "(")
        # if isdefined(ν, :left) && isdefined(ν, :right)
        #     _print_antecedent(io, ν.left)
        #     print(io, " ", conjunction, " ")
        #     _print_antecedent(io, ν.right)
        # elseif isdefined(ν, :right)
        #     _print_antecedent(io, ν.right)
        # end
        _print_antecedent(io, ν.right)
        print(io, ")")
        return
    elseif _datatype(ν) <: AbstractBinaryRelation
        # print(io, "(")
        _print_antecedent(io, ν.left)
        print(io, " ", ν.data, " ")
        _print_antecedent(io, ν.right)
        # print(io, ")")
        return
    else
        print(io, ν.data)
        return
    end
end

function _print_consequent(io::IO, c::String)
    print(io, c)
end

# function _collect_subformulas(ρ::ClassificationRule)
#     sub = FormulaNode[]
#     _collect_subformulas(_antecedent(ρ), sub)
#     return collect(Set(sub))
# end

# function _collect_subformulas(ν::FormulaNode, sub::Vector{FormulaNode})
#     push!(sub, ν)
#     if isdefined(ν, :left) && isdefined(ν, :right)
#         _collect_subformulas(ν.left, sub)
#         _collect_subformulas(ν.right, sub)
#     end
# end

function _collect_sorted_subformulas(ρ::ClassificationRule)
    return sort!(_collect_nodes(ρ), by=x -> _size(x), alg=Base.Sort.QuickSort)
end

function _collect_nodes(ρ::ClassificationRule)
    nodes = FormulaNode[]
    _collect_nodes(_antecedent(ρ), nodes)
    return nodes
end

function _collect_nodes(ν::FormulaNode, nodes::Vector{FormulaNode})
    if isdefined(ν, :left)
        _collect_nodes(ν.left, nodes)
    end
    if isdefined(ν, :right)
        _collect_nodes(ν.right, nodes)
    end
    push!(nodes, ν)
end

show(io::IO, ν::FormulaNode) = _print_antecedent(io, ν)
show(io::IO, ρ::ClassificationRule) = _print_rule(io, ρ)

function _rand_antecedent(init_relations::Vector{<:AbstractRelation},
    relations::Vector{<:AbstractRelation},
    prop_relations::Vector{<:AbstractRelation},
    algebra::Algebra,
    ds::ClassificationDataset,
    orders::Vector{Function};
    minmd::Int=1,
    maxmd::Int=3,
    maxdepth::Int=4,
    isroot::Bool=true,
    isliteral::Bool=false)





    if maxmd ≤ 0 || maxdepth ≤ 0 # || (rand() ≤ 0.2 && minmd ≤ 0) # maxmd ≤ 0 || maxdepth ≤ 0 | (rand() ≤ 0.2 && minmd ≤ 0) 

        case = rand(1:100)
        # Proposition case.
        if case ≤ 90
            return FormulaNode(_rand_proposition(length(attributes(ds)[1]),orders,ds.domains[1])) # TODO: works only for the first frame [1]
        # Algebra value case.
        else
            return FormulaNode(rand(_domain(algebra)))
        end
    end

    maxdepth = maxdepth - 1
    case = rand(1:100) 

    if isroot
        root = FormulaNode(init_relations[rand(1:length(init_relations))])
        minmd = minmd - 1
        maxmd = maxmd - 1
        maxdepth = maxdepth - 1
        rchild = _rand_antecedent(init_relations,relations,prop_relations,algebra,ds,orders;minmd=minmd,maxmd=maxmd,maxdepth=maxdepth,isroot=false,isliteral=isliteral)
        _right!(root, rchild)
        _parent!(rchild, root)
    else 
        # Conjunction case
        if case ≤ 70 && !isliteral
            rel = prop_relations[rand(1:length(prop_relations))]
            root = FormulaNode(rel)

            lchild = _rand_antecedent(init_relations,relations,prop_relations,algebra,ds,orders;minmd=minmd,maxmd=maxmd,maxdepth=maxdepth,isroot=isroot,isliteral=false)
            rchild = _rand_antecedent(init_relations,relations,prop_relations,algebra,ds,orders;minmd=minmd,maxmd=maxmd,maxdepth=maxdepth,isroot=isroot,isliteral=false)
            _left!(root, lchild)
            _right!(root, rchild)
            _parent!(lchild, root)
            _parent!(rchild, root)
        # Literal case λ
        else
            rel = relations[rand(1:length(relations))]
            root = FormulaNode(rel)
            minmd = minmd - 1
            maxmd = maxmd - 1

            # lchild = _rand_antecedent(init_relations,relations,algebra,ds,orders;minmd=minmd,maxmd=maxmd,maxdepth=maxdepth,isroot=false)
            rchild = _rand_antecedent(init_relations,relations,prop_relations,algebra,ds,orders;minmd=minmd,maxmd=maxmd,maxdepth=maxdepth,isroot=false,isliteral=true)
            # _left!(root, lchild)
            _right!(root, rchild)
            # _parent!(lchild, root)
            _parent!(rchild, root)
        end
    end
    return root
end

function _rand_antecedent2(init_relations::Vector{<:AbstractRelation},
        relations::Vector{<:AbstractRelation},
        algebra::Algebra,
        ds::ClassificationDataset,
        orders::Vector{Function};
        minmd::Int=1,
        maxmd::Int=3,
        maxdepth::Int=4,
        isroot::Bool=true)

    if maxmd ≤ 0 || maxdepth ≤ 0 | (rand() ≤ 0.5 && minmd ≤ 0) # maxmd ≤ 0 || maxdepth ≤ 0 | (rand() ≤ 0.2 && minmd ≤ 0) 
        case = rand(1:100)
        # Proposition case.
        if case ≤ 90
            return FormulaNode(_rand_proposition(length(attributes(ds)[1]),orders,ds.domains[1])) # TODO: works only for the first frame [1]
        # Algebra value case.
        else
            return FormulaNode(rand(_domain(algebra)))
        end
    end

    if isroot
        root = FormulaNode(init_relations[rand(1:length(init_relations))])
        minmd = minmd - 1
        maxmd = maxmd - 1
        maxdepth = maxdepth - 1
        rchild = _rand_antecedent(init_relations,relations,algebra,ds,orders;minmd=minmd,maxmd=maxmd,maxdepth=maxdepth,isroot=false)
        _right!(root, rchild)
        _parent!(rchild, root)
    else
        rel = relations[rand(1:length(relations))]
        root = FormulaNode(rel)
        if typeof(rel) <: AbstractBinaryRelation
            maxdepth = maxdepth - 1
        elseif typeof(rel) <: AbstractModalRelation
            minmd = minmd - 1
            maxmd = maxmd - 1
            maxdepth = maxdepth - 1
        end
        lchild = _rand_antecedent(init_relations,relations,algebra,ds,orders;minmd=minmd,maxmd=maxmd,maxdepth=maxdepth,isroot=false)
        rchild = _rand_antecedent(init_relations,relations,algebra,ds,orders;minmd=minmd,maxmd=maxmd,maxdepth=maxdepth,isroot=false)
        _left!(root, lchild)
        _right!(root, rchild)
        _parent!(lchild, root)
        _parent!(rchild, root)
    end
    return root
end

function _rand_proposition(num_attributes::Int, 
        orders::Vector{Function},
        domains::Dict{Symbol, Vector{Number}})
    i = rand(1:num_attributes)
    return Proposition(i, orders[rand(1:length(orders))], domains[Symbol("A" * string(i))][rand(1:length(domains[Symbol("A" * string(i))]))])
end

function _rand_consequent(classes::Vector{String})
    return classes[rand(1:length(classes))]
end

function _rand_rule(init_relations::Vector{<:AbstractRelation},
        relations::Vector{<:AbstractRelation},
        prop_relations::Vector{<:AbstractRelation},
        algebra::Algebra,
        ds::ClassificationDataset,
        orders::Vector{Function};
        minmd::Int=1,
        maxmd::Int=3,
        maxdepth::Int=4,
        isroot::Bool=true,
        isliteral::Bool=false)
    antecedent = _rand_antecedent(init_relations,relations,prop_relations,algebra,ds,orders;minmd,maxmd,maxdepth,isroot,isliteral)
    consequent = _rand_consequent(ds.unique_classes)
    return ClassificationRule(antecedent,consequent)
end

mutable struct ClassificationRules
    rules::Vector{ClassificationRule}
    horizon::Int # TODO: works only for specific functions; for example, in the future, add α and β for sigmoid-like functions
    init_relations::Vector{<:AbstractRelation}
    relations::Vector{<:AbstractRelation}
    prop_relations::Vector{<:AbstractRelation}
    algebra::Algebra
    ds::ClassificationDataset
    orders::Vector{Function}
    minmd::Int
    maxmd::Int
    minnumrules::Int
    maxnumrules::Int
    minh::Int
    maxh::Int
    maxdepth::Int
end

_rules(Γ::ClassificationRules) = Γ.rules
_horizon(Γ::ClassificationRules) = Γ.horizon
_horizon!(Γ::ClassificationRules, h::Int) = Γ.horizon = h
_init_relations(Γ::ClassificationRules) = Γ.init_relations
_classes(Γ::ClassificationRules) = collect(Set([_consequent(ρ) for ρ in Γ]))
_complexity(Γ::ClassificationRules) = reduce(+, [_size(ρ) for ρ in Γ])

# Defined, otherwise it errs.
getindex(Γ::ClassificationRules, inds...) = getindex(_rules(Γ), inds...)
setindex(Γ::ClassificationRules, ρ::ClassificationRule, i::Int) = Γ[i] = ρ 
length(Γ::ClassificationRules) = length(_rules(Γ))
iterate(Γ::ClassificationRules) = iterate(_rules(Γ))
iterate(Γ::ClassificationRules, i::Int) = iterate(_rules(Γ), i)
firstindex(Γ::ClassificationRules) = 1
lastindex(Γ::ClassificationRules) = length(Γ)
ndims(::Type{ClassificationRules}) = 1
size(Γ::ClassificationRules) = (length(Γ),)
copyto!(Γ::ClassificationRules, b::Base.Broadcast.Broadcasted) = copyto!(_rules(Γ), b)

function _rand_rules(init_relations::Vector{<:AbstractRelation},
    relations::Vector{<:AbstractRelation},
    prop_relations::Vector{<:AbstractRelation},
    algebra::Algebra,
    ds::ClassificationDataset,
    orders::Vector{Function};
    minmd::Int=1,
    maxmd::Int=3,
    minnumrules::Int=2,
    maxnumrules::Int=10,
    minh::Int=1,
    maxh::Int=4,
    maxdepth::Int=4)
    numrules = rand(minnumrules:maxnumrules)
    rules = ClassificationRule[]
    horizon = rand(minh:maxh)
    for _ in 1:numrules
        push!(rules, _rand_rule(init_relations,relations,prop_relations,algebra,ds,orders;minmd=minmd,maxmd=maxmd,maxdepth=maxdepth,isroot=true,isliteral=false))
    end
    return ClassificationRules(rules,horizon,init_relations,relations,prop_relations,algebra,ds,orders,minmd,maxmd,minnumrules,maxnumrules,minh,maxh,maxdepth)
end

function _print_rules(io::IO, Γ::ClassificationRules)
    for i in 1:length(Γ)
        _print_rule(io, Γ[i])
    end
end

function show(io::IO, Γ::ClassificationRules)
    println(io, "Horizon = ", _horizon(Γ), "\n")
    _print_rules(io, Γ)
end

#######################################
# @@@ End of CLASSIFICATION RULES @@@ #
#######################################

##################
# @@@ TRAITS @@@ #
##################

# Ontological traits.
abstract type OntologicalTrait end
struct PropositionalTrait   <: OntologicalTrait end
struct PointTrait           <: OntologicalTrait end
struct IntervalTrait        <: OntologicalTrait end

# Default behavior is propositional.
OntologicalTrait(::Type) = PropositionalTrait()
# Propositional trait.
OntologicalTrait(::Type{<:AbstractPropositionalRelation})   = PropositionalTrait()
# Point trait.
OntologicalTrait(::Type{<:AbstractPointRelation})           = PointTrait()
# Interval trait.
OntologicalTrait(::Type{<:AbstractIntervalRelation})        = IntervalTrait()

ispropositional(x::T) where {T}             = ispropositional(OntologicalTrait(T), x)
ispropositional(::PropositionalTrait, x)    = true
ispropositional(::OntologicalTrait, x)      = false

ispoint(x::T) where {T}     = ispoint(OntologicalTrait(T), x)
ispoint(::PointTrait, x)    = true
ispoint(::OntologicalTrait) = false

isinterval(x::T) where {T}          = isinterval(OntologicalTrait(T), x)
isinterval(::IntervalTrait, x)      = true
isinterval(::OntologicalTrait, x)   = false

#########################
# @@@ End of TRAITS @@@ #
#########################

Base.:(==)(x::Proposition, y::Proposition) = x.A == y.A && x.rel == y.rel && x.a == y.a
Base.:(==)(x::AbstractRelation, y::AbstractRelation) = typeof(x) == typeof(y)
Base.:(==)(x::FormulaNode, y::FormulaNode) = _isequal(x, y)
Base.:(==)(ρ₁::ClassificationRule, ρ₂::ClassificationRule) = _isequal(_antecedent(ρ₁), _antecedent(ρ₂)) && _consequent(ρ₁) == _consequent(ρ₂)
function _isequal(x::FormulaNode, y::FormulaNode)
    if isdefined(x, :left) && isdefined(x, :right) && isdefined(y, :left) && isdefined(y, :right)
        return x.data == y.data && _isequal(x.left, y.left) && _isequal(x.right, y.right)
    elseif !isdefined(x, :left) && isdefined(x, :right) && !isdefined(y, :left) && isdefined(y, :right)
        return x.data == y.data && _isequal(x.right, y.right)
    elseif !isdefined(x, :left) && !isdefined(x, :right) && !isdefined(y, :left) && !isdefined(y, :right)
        return x.data == y.data
    end
    return false
end

# TODO: remove?
function Base.:(==)(Γ₁::ClassificationRules, Γ₂::ClassificationRules)
    if length(_rules(Γ₁)) == length(_rules(Γ₂))
        res = true
        # SS = Set(_rules(S))
        for i ∈ 1:length(_rules(Γ₂))
            (!(Γ₁[i] ∈ Γ₂)) && (res = false)
        end
        # (Set(rules(R)) != Set(rules(S))) && (res = false)
        (classes(Γ₁) != classes(Γ₂)) && (res = false)
        return res
    else
        return false
    end
end

hash(p::Proposition) = hash(p.A) + hash(p.rel) + hash(p.a)
function hash(x::FormulaNode)
    if isdefined(x, :left) && isdefined(x, :right)
        return hash(x.data) + hash(x.left) + hash(x.right)
    elseif !isdefined(x, :left) && isdefined(x, :right)
        return hash(x.data) + hash(x.right)
    elseif !isdefined(x, :left) && !isdefined(x, :right)
        return hash(x.data)
    end
end
hash(ρ::ClassificationRule) = hash(_antecedent(ρ)) + hash(_consequent(ρ))
hash(Γ::ClassificationRules) = reduce(+, [hash(ρ) for ρ in Γ])

hash(x::Tuple{FormulaNode, Tuple{Int64, Int64}}) = hash(x[1]) + hash(x[2][1]) + hash(x[2][2]) # TODO: needed?