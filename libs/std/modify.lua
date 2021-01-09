baseModify = SltThunk.create(
  function() return 
    SltFunc.create(
      function(struct)
        return SltFunc.create(
          function(string)
            return SltFunc.create(
              function(func)
                local a = copy(struct())
                if not a.is_struct then SltError.crate("first argument TypeError", "Must be a struct rather than being " .. t.type_, t) end
                local val = a.table[string().value]
                a.table[string().value] = SltThunk.create(function() return func()(val) end)
                return a
              end
            )
          end
        )
      end     
    )
  end,
  Mutates
)
