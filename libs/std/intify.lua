require "SltRuntime"

function isNumLua(n) return tonumber(n().value) == nil end

isNumBase = SltThunk.create(
    function() return
        SltFunc.create(
            function(n) return SltBool.create(tonumber(tostring(n())) ~= nil, n().loc) end
        )
    end
)

stringToInt = SltThunk.create(
    function() return
        SltFunc.create(
            function(i)
                if isNumLua(i) then 
                    return SltError.create("TypeError: ", "First argument is not a string", i())
                else
                    return SltNum.create(tonumber(i().value), {6, 1, "intify.lua"})
                end
            end
            ),
            {7, 1, "intify.lua"}
    end
)