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

unsafeReadFile = SltThunk.create(
  function() return
    SltFunc.create(
      function(st)
        local content = ""
        for line in io.lines(tostring(st())) do
          content = content == "" and line or content .. "\n" .. line
        end
        return SltString.create(content, st().loc)
      end
    )
  end,
  Mutates
)

unsafeExistsFile = SltThunk.create(
  function() return 
    SltFunc.create(
      function(sltFileName)
        local fn = tostring(sltFileName())
        local file = io.open(fn, "r")
        return SltBool.create(file ~= nil, sltFileName().loc)
      end     
    )
  end,
  Mutates
)

unsafeWriteFile = SltThunk.create(
  function() return
    SltFunc.create(
      function(filenameSlt) return 
        SltFunc.create(
          function(contentSlt) return
            SltFunc.create(
              function(modeSlt)
                file = io.open(tostring(filenameSlt()), tostring(modeSlt()))
                file:write(tostring(contentSlt()))
                file:close()
                return SltString.create(tostring(contentSlt()), filenameSlt().loc)
              end
            )
          end
        )
      end
    )
  end,
  Mutates
)