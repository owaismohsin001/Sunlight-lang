function copy(obj, seen)
  -- Handle non-tables and previously-seen tables.
  if type(obj) ~= 'table' then return obj end
  if seen and seen[obj] then return seen[obj] end

  -- New table; mark it as seen and copy recursively.
  local s = seen or {}
  local res = {}
  s[obj] = res
  for k, v in pairs(obj) do res[copy(k, s)] = copy(v, s) end
  return setmetatable(res, getmetatable(obj))
end

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
