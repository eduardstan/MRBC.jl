function rules_crossover!(Γ₁::ClassificationRules, Γ₂::ClassificationRules, Γ₁′::ClassificationRules, Γ₂′::ClassificationRules)
    i = rand(1:length(Γ₁))
    j = rand(1:length(Γ₂))
    Γ₁′ = deepcopy(Γ₁)
    Γ₂′ = deepcopy(Γ₂)
    _rules(Γ₁′)[i] = _rules(Γ₂)[j]
    _rules(Γ₂′)[j] = _rules(Γ₁)[i]
    return
end

# levels: 50% 30% 10% 10%
#   1) rules level:
#       a) add rule, and
#       b) remove rule
#   2) rule level that replaces a subformula with another subformula
#   3) class level that changes the class
#   4) horizon level
function rules_mutation!(Γ::ClassificationRules)
    level = rand(1:100)

    # rules level
    if level ≤ 50
        # add rule
        if rand(1:2) == 1
            if length(Γ) < Γ.maxnumrules
                push!(_rules(Γ), _rand_rule(Γ.init_relations,Γ.relations,Γ.algebra,Γ.ds,Γ.orders;minmd=Γ.minmd,maxmd=Γ.maxmd,maxdepth=Γ.maxdepth,isroot=true))
            end
            return
        # remove rule
        else
            if length(Γ) > Γ.minnumrules
                deleteat!(_rules(Γ), rand(1:length(Γ)))
                return
            end
        end
    # rule level
    elseif 50 < level && level ≤ 80

        i = rand(1:length(Γ))
        rule = _rules(Γ)[i]

        nodes = _collect_nodes(rule)
        s = reduce(+,[_size(ν) for ν in nodes])
        w = [1 - _size(ν)/s for ν in nodes]./(length(nodes)-1)
        sf = StatsBase.sample(nodes, Weights(w))
        # sf = nodes[rand(1:length(nodes))]

        # upwards modal depth of the selected subformula
        umd = _upwards_md(sf)
        # # upwards modal depth is the previously computed one if the subformula is a leaf;
        # # otherwise, it is minus 1 because we need to replace an internal node which has a modality by definition
        # umd = (_is_leaf(sf) ? umd : umd-1)
        
        udepth = _upwards_depth(sf)

        # new subtree
        subtree = isdefined(sf, :parent) ?
            _rand_antecedent(Γ.init_relations,Γ.relations,Γ.algebra,Γ.ds,Γ.orders;minmd=Γ.minmd-umd,maxmd=Γ.maxmd-umd,maxdepth=Γ.maxdepth-udepth,isroot=false) :
            _rand_antecedent(Γ.init_relations,Γ.relations,Γ.algebra,Γ.ds,Γ.orders;minmd=Γ.minmd-umd,maxmd=Γ.maxmd-umd,maxdepth=Γ.maxdepth-udepth,isroot=true)
        # set parent node, if exists
        if isdefined(sf, :parent)
            if isdefined(sf.parent, :left) && sf.parent.left == sf
                sf.parent.left = subtree
            elseif isdefined(sf.parent, :right) && sf.parent.right == sf
                sf.parent.right = subtree
            end
            _parent!(subtree, sf.parent)
        else
            _antecedent!(Γ[i],subtree)
        end
    # class level
    elseif 80 < level && level ≤ 90
        i = rand(1:length(Γ))
        C = Γ.ds.unique_classes
        c = C[rand(1:length(C))]

        # TODO: farebbe loop infinito..
        while _consequent(_rules(Γ)[i]) == c
            c = C[rand(1:length(C))]
        end

        _rules(Γ)[i].consequent = c
        return
    # horizon level
    else
        # reduce horizon value by 1, if possible
        if rand(1:2) == 1
            if _horizon(Γ) > Γ.minh
                _horizon!(Γ, _horizon(Γ) - 1)
            end
        # increase horizon value by 1, if possible
        else
            if _horizon(Γ) < Γ.maxh
                _horizon!(Γ, _horizon(Γ) + 1)
            end
        end
    end
end