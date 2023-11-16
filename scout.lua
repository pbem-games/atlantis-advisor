local function flag_bool(name, set)
    if set then
        print(name,' 1')
    else
        print(name, ' 0')
    end
end

function avoid(set)
    flag_bool('avoid', set)
end

function behind(set)
    flag_bool('behind', set)
end

function noaid(set)
    flag_bool('noaid', set)
end

function needs(req)
    need = '@;needs ' .. req.amount

    if req.unneeded then
        need = need .. ' unneeded'
    end

    item = find_item(req.item)
    if not item then
        return
    end

    print(need .. ' ' .. item.code)
end

function skill(code, level)
    return { code = code, level = level }
end

function order(auto, name, ...)
    return { auto = auto, name = name, args = args }
end

function study_queue(unit, skills)
    for _, skill in ipairs(skills) do
        current = unit.get_skill(skill.code)

        if current.level < skill.level and can_learn(skill, unit)  then
            return {
                code = skill.code,
                level = skill.level,
                cost = skill.cost * unit.men,
            }
        end
    end
end

----------------------------------------------------------------------------------------

name = 'Scout'

-- will run only once after turn is loaded
function startup(args, unit, region, mem)
end

-- will run at the beginning of the order processing every time orders need to be reprocessed
function initial(args, unit, region, mem)
    -- set flags
    order_avoid(true)
    order_behind(true)
    order_noaid(true)

    -- set needs
    needs({ item = 'silver', amount = 'enough' })

    study = study_queue(unit, {
        skill('ente', 1),
        skill('obse', 1),
        skill('ente', 2),
        skill('obse', 2),
        skill('ridi', 2),
    })

    if study then
        mem.order = study
        needs({ item = 'silver', amount = study.cost })
    end
end

-- do actions here that are not monthlong
function action(args, unit, region, mem)
end

-- do monthlong actions here
function monthlong(args, unit, region, mem)
    if mem.order and get_shared(unit, region, 'silver') > mem.order.cost then
        order_study(mem.order.code, mem.order.level)
    else
        if unit.get_skill('ente').level >= 2 then
            order_entertain()
        else
            order_work()
        end
    end
end

-- final stage, suitable for different messages and checks
function final(args, unit, region, mem)
end
