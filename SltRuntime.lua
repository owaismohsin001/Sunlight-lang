function SltValueCall(this, ...)
    return error(SltError.create("Type Error", "Can't call a value of type " .. this.type_, this))
end

function outErr(f)
  print(f())
end

function checkType(expected, val)
  if expected ~= val.type_ then error(SltError.create("TypeError", "expected " .. expected .. " got " .. val.type_, val)) end
  return val
end

function tryOrError(fun)
  local status, err = pcall(fun)
  if not status then
    print(debug.traceback())
    print(err)
    os.exit()
    return
  end
  return
end

function try(f, catch_f)
  local status, exception = pcall(f)
  if not status then
    return catch_f(exception)
  end
  return f()
end

SltValue = {}
SltValue = {
    metatable = {
          __index = SltValue;
          __call = SltValueCall;
      };
      type_ = "Value";
      add = function(this, other)
        error(SltError.create("TypeError", "Cannot add " .. this.type_ .. " to " .. other.type_, this))
       end;

      tail = function(this) 
        error(SltError.create("TypeError", "Cannot get the tail of " .. this.type_ , this)) 
      end;

      head = function(this) 
        error(SltError.create("TypeError", "Cannot get the head of " .. this.type_ , this)) 
      end;

      sub = function(this, other)
        error(SltError.create("TypeError", "Cannot substract " .. this.type_ .. " from " .. other.type_, this))
      end;

      mul = function(this, other)
        error(SltError.create("TypeError", "Cannot multiply " .. this.type_ .. " with " .. other.type_, this))
      end;

      div = function(this, other)
        error(SltError.create("TypeError", "Cannot divide " .. this.type_ .. " by " .. other.type_, this))
      end;

      isType = function(this, str)
        return SltBool.create(this.type_ == str, this.loc)
      end;

      neg = function(this)
        error(SltError.create("TypeError", "Can't negate " .. this.type_, this))
      end;

      notted = function(this)
        error(SltError.create("TypeError", "Can't not " .. this.type_, this))
      end;

      is_true = function(this)
        error(SltError.create("TypeError", "Can't use " .. this.type_ .. " in as conditions", this))
      end;

      eq = function(this, other)
        error(SltError.create("TypeError", "Can't check equality of  " .. this.type_ .. " and " .. other.type_, this))
      end;

      lt = function(this, other)
        error(SltError.create("TypeError", "Cannot quantify " .. this.type_ .. " and " .. other.type_ .. " with appropriate quantities for '<'", this))
      end;

      lte = function(this, other)
        error(SltError.create("TypeError", "Cannot quantify " .. this.type_ .. " and " .. other.type_ .. " with appropriate quantities for '<='", this))
      end;

      gt = function(this, other)
        error(SltError.create("TypeError", "Cannot quantify " .. this.type_ .. " and " .. other.type_ .. " with appropriate quantities for '>'", this))
      end;

      gte = function(this, other)
        error(SltError.create("TypeError", "Cannot quantify " .. this.type_ .. " and " .. other.type_ .. " with appropriate quantities for '>='", this))
      end;

      neq = function(this, other)
        error(SltError.create("TypeError", "Can't check equality of  " .. this.type_ .. " and " .. other.type_, this))
      end;

      anded = function(this, other)
        error(SltError.create("TypeError", "Can't make 'and' of " .. this.type_ .. " and " .. other.type_, this))
      end;

      isEmpty = true;

      ored = function(this, other)
        error(SltError.create("TypeError", "Can't make 'or' of " .. this.type_ .. " and " .. other.type_, this))
      end;

      inside = function(this, other)
        error(SltError.create("TypeError", "Can't check if " .. this.type_ .. "is in other " .. other.type_, this))
      end;

      getProperty = function(this, other)
        error(SltError.create("KeyError", "Cannot get the property " .. tostring(other) .. " of " .. this.type_, this))
      end;

      getIndex = function(this, other)
        error(SltError.create("KeyError", "Cannot get the index " .. other.value .. " from " .. this.type_, this))
      end;

      concat = function(this, other)
        error(SltError.create("TypeError", "Cannot concatenate " .. this.type_ .. " to " .. other.type_, this))
      end;

      unwrap = function(this, args)
        tryOrError(function() return checkType("SltTuple", this) end)
        tryOrError(function() return this.checkLength(this, args) end)
        return unpack(this.value)
      end;

      destructure = function(this, args)
        if not this.is_struct then error(SltError.create("TypeError", "Cannot destructure " .. this.type_, this)) end
        tryOrError(function() return this.checkLength(this, args) end)
        local newTable = {}
        for k, v in pairs(this.table) do 
          table.insert(newTable, v)
        end
        return unpack(newTable)
      end;

      toString = function(this)
        error(SltError.create("TypeError", "Can't print Value as a string " .. this.type_, this))
      end;

      getOutput = function(this)
        print("TypeError: " .. this.type_ .. " cannot be used as output, use a list instead")
        os.exit()
      end;

      locate = function(this, loc)
        this.loc = loc
        return this
      end;

       sameTypes = function (this, other, fun)
            other = other or this
            if other.type_ == this.type_ then
                return true
            end
            return fun(this, other)
        end;
}

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

function map(tbl, f)
  local t = {}
  for k,v in pairs(tbl) do
      t[k] = f(v)
  end
  return t
end

function listToString(this)
  local tbs = {}
  local output = this
  while not output:tail().isEmpty do
    table.insert(tbs, tostring(output.head()))
    output = output.tail()
  end
  return "[" .. table.concat(tbs, ", ") .. "]"
end

function reversed(iterable)
  local i = len(iterable)-1
  local reversed_iterable = {}
  local counter = 0
  while i >= 0 do
    reversed_iterable[counter] = iterable[i]
    i = i-1
    counter = counter + 1
  end
  return reversed_iterable
end

function make_iterable(set)
  local iterable = {}
  local counter = 0
  for k, v in pairs(set) do
    iterable[counter] = v
    counter = counter + 1
  end
  return iterable
end

SltList = {}
SltList.__index = SltValue

SltList.fromValues = function(values, loc)
  local result = SltNum.create(0)
  local list = reversed(make_iterable((values)))
  local length = len(list)
  local i = 0
  while i<length do
    local headThunk = list[i]
    local tailThunk = result
    result = SltList.create(headThunk, tailThunk, loc)
    i = i+1
  end
  return result
end

SltList.direct = function(head, tail, loc)
  local list = SltList.create("nil", "nil")
  list.head = head
  list.tail = tail
  list.loc = loc
  return list
end

SltList.create = function(head, tail, loc)
  local this = {}
  this.head = function(this) return head end;
  this.tail = function(this) return tail end;
  this.isEmpty = false;
  this.loc = loc;
  this.type_ = "SltList";

  this.toList = function(this)
    local tailIter = this.tail(this)
    local result = {this.head(this)}
    while not tailIter.isEmpty do
      table.insert(result, tailIter.head(tailIter))
      tailIter = tailIter.tail(tailIter)
    end
    return result
  end;

  this.concat = function(this, other)
    local tail = this.tail(this)
    if tail.isEmpty then
      local tailThunk = other
      local x = SltList.direct(SltThunk.create(function() return this.head(this) end), tailThunk, this.loc)
      return x
    end
    local thunk = SltThunk.create(function() return tail.concat(tail, other) end)
    return SltList.direct(SltThunk.create(function() return this.head(this) end), thunk, this.loc)
  end;

  this.eq = function(this, other)
    if other.type_ ~= this.type_ then return SltBool.create(false, this.loc) end
    local this_seq = make_iterable(this.toList(this))
    local other_seq = make_iterable(other.toList(other))
    for k, v in pairs(this_seq) do
      if not contains(other_seq, k) then return SltBool.create(false, this.loc) end
      if not v.equaled(v, other_seq[k]).value then return SltBool.create(false, his.loc) end
    end
    return SltBool.create(true, this.loc)
  end;

  this.neq = function(this, other)
    return SltBool.create(not (this.equaled(this, other).value), this.loc)
  end;

  this.locate = function(this, location) 
    this.loc = location 
    return this
  end

  this.getOutput = function(this)
    local output = this
    while output.type_ == "SltList" do
      print(output:head():toString())
      output = output:tail()
    end
    return
  end;

  toString = function(this)
    return "[" .. table.concat(map(this:toList(), function(x) return tostring(x) end), ", ") .. "]"
  end;

  setmetatable(this, {
      __index = SltValue;
      __call = SltValueCall;
      __unum = SltValue.neg;
      __add = SltValue.add;
      __sub = SltValue.sub;
      __mul = SltValue.mul;
      __div = SltValue.div;  
      __tostring = toString;
  })
  return this
end

SltEmptyList = {}
SltEmptyList.__index = SltList
function SltEmptyList.create(pos)
  this = SltList.create(
    function(this) return error(SltError.create("IndexError", "Head of an empty list is impossible", this)) end, 
    function(this) return SltEmptyList.create(pos) end, 
    pos
  ) 
  this.type_ = "SltList"
  this.isEmpty = true
  this.loc = pos

  this.concat = function(this, other)
    return copy(other())
  end

  this.eq = function(this, other) return SltBool.create(other.isEmpty == nil) end
  this.neq = function(this, other) return SltBool.create(other.isEmpty ~= nil) end

  setmetatable(this, {
    __index = SltValue;
    __call = SltValueCall;
    __unum = SltValue.neg;
    __add = SltValue.add;
    __sub = SltValue.sub;
    __mul = SltValue.mul;
    __div = SltValue.div;
    __tostring = function(_) return "[]" end
  })
  return this
end

SltTuple = {}
SltTuple.__index = SltValue
function SltTuple.create(tp, loc)
  local this = {}
  this.type_ = "SltTuple"
  this.value = tp
  this.loc = loc

  this.checkLength = function(this, length)
    if length ~= len(this.value) then
      error(
        SltError.create(
          "TypeError", "Cannot destructure length " .. tostring(len(this.value)) .. " tuple to " .. length .. " names", this
        )
      )
    end
    return this
  end;

  this.eq = function(this, other)
    if this.type_ ~= other.type_ then return SltBool.create(false, this.loc) end
    bool_false = SltBool.create(false, this.loc)
    if #this.value ~= #other.value then return bool_false end
    local main_bool = SltBool.create(true, this.loc)
    for i, v in ipairs(this.value) do
      if other.value[i] == nil then return bool_false end
      if v:neq(other.value[i]):is_true() then return bool_false end
    end
    return main_bool
  end

  this.neq = function(this, other)
    SltValue.sameTypes(this, other, SltValue.neq)
    return (this:eq(other)):notted()
  end

  toString = function(this)
    return "(" .. table.concat(map(this.value, function(v) return tostring(v()) end), ", ") .. ",)"
  end

  setmetatable(this, {
    __index = SltValue;
    __call = SltValueCall;
    __unum = SltValue.neg;
    __add = SltValue.add;
    __sub = SltValue.sub;
    __mul = SltValue.mul;
    __div = SltValue.div;
    __tostring = toString
  })

  return this
end;

function hasKey(key, table)
  for k, _ in pairs(table) do
    if k == key then return true end
  end
  return false
end

function len(table)
  local i = 0
  for _, _ in pairs(table) do
    i = i + 1
  end
  return i
end

function make_table(this, keys, table)
  if len(keys) ~= len(table) then error(SltError.create("TypeError", "Insufficient keys were supplied", this)) end
  for k, _ in pairs(keys) do
    if not hasKey(k, table) then error(SltError.create("TypeError", "Key " .. k .." not found", this)) end
  end
  return table
end

function get_keys(t)
  keys = {}
  for k, _ in pairs(t) do
    table.insert(keys, k)
  end
  return keys
end

SltStruct = {}
SltStruct.__index = SltValue
function SltStruct.create(name, overarch, keys, tb, loc)
  local this = {}
  this.type_ = name
  this.is_struct = true
  this.overarch = overarch
  this.loc = loc
  this.table = make_table(this, keys, tb)

  this.isType = function(this, str)
    return SltBool.create(this.type_ == str or this.overarch == str, this.pos)
  end;

  this.checkLength = function(this, length)
    if length ~= len(this.table) then
      error(
        SltError.create(
          "TypeError", "Cannot destructure length " .. tostring(len(this.value)) .. " struct to " .. tostring(length) .. " names", this
        )
      )
    end
    return this
  end

  this.eq = function(this, other)
    function eq_struct(ta, tb)
      if len(ta) ~= len(tb) then return SltBool.create(false) end
      for k, v in pairs(ta) do
        if not hasKey(k, tb) then return SltBool.create(false) end
        if not (tb[k]():eq(v())):is_true() then return SltBool.create(false) end
      end
      return SltBool.create(true)
    end

    if other.is_struct then
      return SltBool.create(this.type_ == other.type_ and this.overarch == other.overarch and eq_struct(this.table, other.table))
    else
      return SltBool.create(false)
    end
  end

  this.neq = function(this, other) return (this:eq(other)):notted() end

  toString = function(this)
    local init = this.type_
    local ls = init .. "{"
    local slen = len(this.table)
    if slen == 0 then return init end
    local i = 0
    for k, v in pairs(this.table) do
      ls = ls .. k .. " :: " .. tostring(v())
      ls = i == slen-1 and ls or ls .. ", "
      i = i+1
    end
    return ls .. "}"
  end

  tableString = function (t) return "{" .. table.concat(t, ", ") .. "}" end

  this.getProperty = function (this, property)
    tryOrError(function() return checkType("SltString", property) end)
    p = this.table[property.value]
    if p == nil then 
      error(SltError.create("IndexError", tostring(property) .. " not found in " .. tableString(get_keys(this.table)), this)) 
    end
    return p
  end

  setmetatable(this, {
    __index = SltValue;
    __call = SltValueCall;
    __unum = SltValue.neg;
    __add = SltValue.add;
    __sub = SltValue.sub;
    __mul = SltValue.mul;
    __div = SltValue.div;
    __tostring = toString
  })

  return this
end;

SltError = {}
SltError.__index = SltValue
function SltError.create(errorType, message, value)
  local this = setmetatable({
    err = errorType;
    message = message;
    value = value;
  }, SltError)

  fn, ln, cn = unpack(this.value.loc)
  ln = tostring(ln)
  cn = tostring(cn)

  loc = "In file: \"" .. fn .. "\", line no: " .. ln .. ", col no: " .. cn
  print(loc)
  print("Uncaught Error: " .. message)
end

SltThunk = {}
SltThunk.__index = SltValue
function SltThunk.create(fun)
  local this = {}
  this.type_ = "SltThunk";
  this.fun = fun;
  this.value = nil;

  setmetatable(this, {
    __index = SltValue;
    __call = function(this)
      if this.value == nil then
        this.value = this.fun()
      end
      return this.value
    end;
    __unum = SltValue.neg;
    __add = SltValue.add;
    __mul = SltValue.mul;
    __eq = SltValue.eq;
    __lt = SltValue.lt;
    __le = SltValue.lte;
    __tostring = toString
  })
  return this
end;

SltBool = {}
SltBool.__index = SltValue
function SltBool.create(num, location)
    local this = {}
    this.type_ = "SltBool";
    this.loc = location;
    this.value = num;

    this.ored = function(this, other)
      if this.value then
        return SltBool.create(true, this.loc)
      end
      SltValue.sameTypes(this, other(), SltValue.ored)
      if other().value then 
        return SltBool.create(true, this.loc)
      end
      return SltBool.create(false, this.loc)
    end

    this.anded = function(this, other)
      if this.value then
        SltValue.sameTypes(this, other(), SltValue.anded)
        if other().value then 
          return SltBool.create(true, this.loc)
        end
      end
      return SltBool.create(false, this.loc)
    end

    this.notted = function(this)
      return SltBool.create(not this.value, this.loc)
    end

    this.eq = function(this)
      if this.type_ ~= other.type_ then return SltBool.create(false, this.loc) end
      return SltBool.crate(this.value == other.value, this.loc)
    end

    this.neq = function(this)
      return this:eq(other):notted()
    end

    this.is_true = function(this) return this.value end

    toString = function(this)
        return tostring(this.value)
    end

    setmetatable(this, {
      __index = SltValue;
      __call = SltValueCall;
      __unum = SltValue.neg;
      __add = SltValue.add;
      __sub = SltValue.sub;
      __mul = SltValue.mul;
      __div = SltValue.div;
      __tostring = toString
      })
      return this
end

SltNum = {}
SltNum.__index = SltValue
function SltNum.create(num, location)
  local this = {}
  this.type_ = "SltNum";
  this.loc = location;
  this.value = num;

  add = function(this, other)
    SltValue.sameTypes(this, other, SltValue.add)
    return SltNum.create(this.value + other.value):locate(this.loc)
  end;

  sub = function(this, other)
    SltValue.sameTypes(this, other, SltValue.sub)
    return SltNum.create(this.value - other.value):locate(this.loc)
  end;

  mul = function(this, other)
    SltValue.sameTypes(this, other, SltValue.mul)
    return SltNum.create(this.value * other.value):locate(this.loc)
  end;

  div = function(this, other)
    SltValue.sameTypes(this, other, SltValue.div)
    if other.value == 0 then
      error(tostring(SltError.create("DivisionByZero", "Can't divide by zero", this)))
    end
    return SltNum.create(this.value / other.value):locate(this.loc)
  end;

  this.neg = function(this)
    return SltNum.create(this.value * -1):locate(this.loc)
  end;

  this.eq = function(this, other)
    if this.type_ ~= other.type_ then return SltBool.create(false, this.loc) end
    return SltBool.create(this.value == other.value):locate(this.loc)
  end;

  this.neq = function(this, other)
    if this.type_ ~= other.type_ then return SltBool.create(true) end
    return SltBool.create(this.value ~= other.value):locate(this.loc)
  end;

  this.lt = function(this, other)
    SltValue.sameTypes(this, other, SltValue.lt)
    return SltBool.create(this.value < other.value):locate(this.loc)
  end;

  this.lte = function(this, other)
    SltValue.sameTypes(this, other, SltValue.lte)
    return SltBool.create(this.value <= other.value):locate(this.loc)
  end;

  this.gte = function(this, other)
    SltValue.sameTypes(this, other, SltValue.lte)
    return SltBool.create(this.value >= other.value):locate(this.loc)
  end;

  this.gt = function(this, other)
    SltValue.sameTypes(this, other, SltValue.lte)
    return SltBool.create(this.value > other.value):locate(this.loc)
  end;

  toString = function(this)
    return tostring(this.value)
  end;

  mt = {
    __index = SltValue;
    __call = SltValueCall;
    __add = add;
    __sub = sub;
    __mul = mul;
    __div = div;
    __tostring = toString
    }
  setmetatable(this, mt)
  return this
end

SltString = {}
SltString.__index = SltValue
function SltString.create(str, loc)
  local this = {}
  this.type_ = "SltString"
  this.value = str
  this.loc = loc

  this.concat = function(this, other)
    SltValue.sameTypes(this, other, SltValue.concat)
    return SltString.create(this.value .. other.value)
  end

  this.eq = function(this, other)
    return SltBool.create(this.value == other.value, this.loc)
  end

  this.neq = function(this, other) return this:eq(other):notted() end

  toString = function(this) return this.value end

  setmetatable(this, {
    __index = SltValue;
    __call = SltValueCall;
    __add = SltValue.add;
    __sub = SltValue.sub;
    __mul = SltValue.mul;
    __div = SltValue.div;
    __tostring = toString
  })

  return this
end;

SltFunc = {}
SltFunc.__index = SltValue
function  SltFunc.create(fun, loc, isPure)
  local this = {}
  this.fun = fun;
  this.isPure = isPure and true or false
  this.values = {};
  this.type_ = "SltFunc";
  this.loc = loc

  this.locate = function(this, location) return locate(this, location) end;

  toString = function(this)
    return "<function>"
  end

  setmetatable(this, {
    __index = SltValue;
    __call = function(this, ...)
      return this.fun(...)
    end;
    __add = SltValue.add;
    __sub = SltValue.sub;
    __mul = SltValue.mul;
    __div = SltValue.div;
    __tostring = toString
  })
  return this
end;


---------------------------------
head = 
  SltThunk.create(
    function() return
      SltFunc.create(
        function(ls)
          return ls():head()
        end
      )
    end
  )

tail = SltThunk.create(
  function() return
    SltFunc.create(
      function(ls)
        return ls():tail()
      end
    )
  end
)