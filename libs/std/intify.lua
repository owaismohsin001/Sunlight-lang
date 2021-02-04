require "SltRuntime"

function isNum(n) return tonumber(n().value) == nil end

stringToInt = SltThunk.create(
    function() return
        SltFunc.create(
            function(i)
                if isNum(i) then 
                    return SltError.create("TypeError: ", "First argument is not a string", i())
                else
                    return SltNum.create(tonumber(i().value), {6, 1, "intify.lua"})
                end
            end
            ),
            {7, 1, "intify.lua"}
    end
)