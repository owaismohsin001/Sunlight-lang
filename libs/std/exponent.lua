require 'SltRuntime'

unsafeExponent = SltThunk.create(
    function() return
        SltFunc.create(
            function(a) return
                SltFunc.create(
                    function(b) return
                        SltNum.create(math.pow(a().value, b().value), a().pos)
                    end
                )
            end
            ),
            {7, 1, "intify.lua"}
    end
)