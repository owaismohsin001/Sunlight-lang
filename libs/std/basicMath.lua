require 'SltRuntime'

function round(x)
    return x>=0 and math.floor(x+0.5) or math.ceil(x-0.5)
end

unsafeRound = SltThunk.create(
    function() return
        SltFunc.create(
            function(a) return
                    SltNum.create(round(a().value), a().pos)
            end,
            {7, 1, "intify.lua"}
        )
    end
)

unsafeCeil = SltThunk.create(
    function() return 
        SltFunc.create(
            function(a) return SltNum.create(math.ceil(a().value), a().pos) end
        )
    end
)

unsafeFloor = SltThunk.create(
    function() return 
        SltFunc.create(
            function(a) return SltNum.create(math.floor(a().value), a().pos) end
        )
    end
)

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

unsafeMod = 
  SltThunk.create(
    function() return
      SltFunc.create(
        function(a)
          return SltFunc.create(
            function(b)
              return SltNum.create(math.fmod(a().value, b().value), b().loc)
            end
          )
        end
      )
    end
  )