require "SltRuntime"

unsafeRead = SltThunk.create(
  function() return 
    SltFunc.create(
      function(st)
        io.write(tostring(st()))
        inp = io.read()
        io.write("\n")
        return SltString.create(inp)
      end     
    )
  end,
  Mutates
)