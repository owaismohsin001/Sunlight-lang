'use strict'

import { sha1 } from "./compiler_libs/hashLib.js"

let uCount = 0

const Mutates = -1

function error(errType: string, message: string, value: SltValue): SltValue{
  if (value.loc != null) {
    const [fn, ln, cn] = value.loc
    const lnString = ln.toString()
    const cnString = cn.toString()
    console.log("In file: \"" + fn + "\", line no: " + lnString + ", col no: " + cnString)
  }
  throw (`Uncaught Error: ${errType} ${message}`)
}

function checkType(expected: string, val: SltValue){
    if (expected != val.type_) error("TypeError", "expected " + expected + " got " + val.type_, val)
    return val
}

function sameTypes(t: SltValue, other: SltValue, fun: (a: SltValue, b: SltValue) => SltValue) {
    other = other || t
    if (other.type_ == t.type_){
        return true
    }
    return fun(t, other)
}

async function write(text: string){
  const te = new TextEncoder();
  await Deno.writeAll(Deno.stdout, te.encode(text));  
}

class SltValue extends Function {
    type_: string = ""
    loc: [number, number, string] = [0, 0, ""]
    isEmpty: boolean = true
    hashAble: boolean = false
    value: any = null
    mutates: number | null = null
    overarch: string = ""
    tail: Function | (() => SltValue)
    head: Function | (() => SltValue)
    constructor(...args: any[]) {
        super()
        this.tail = () => error("TypeError", "Cannot get the tail of " + this.type_ , this)
        this.head = () => error("TypeError", "Cannot get the head of " + this.type_ , this)
        return new Proxy(this, {
          apply: (target, thisArg, args) => target._call(...args)
        })
    }

    _call(..._: SltValue[]) {
        return error("Type Error", "Can't call a value of type " + this.type_, this)
    }

    add(other: SltValue): SltValue { return error("TypeError", "Cannot add " + this.type_ + " to " + other.type_, this) }

    sub(other: SltValue): SltValue { return error("TypeError", "Cannot substract " + this.type_ + " from " + other.type_, this) }

    mul(other: SltValue): SltValue { return error("TypeError", "Cannot multiply " + this.type_ + " from " + other.type_, this) }

    div(other: SltValue): SltValue { return error("TypeError", "Cannot divide " + this.type_ + " from " + other.type_, this) }

    isType(other: string | SltValue): SltValue {
        if (typeof other == "string") {return new SltBool(this.type_ == other, this.loc)}
        return new SltBool(this.type_ == other.type_, this.loc)
    }

  neg(): SltValue { return error("TypeError", "Can't negate " + this.type_, this) }

  notted(): SltValue { return error("TypeError", "Can't not " + this.type_, this) }

  is_true(): boolean { error("TypeError", "Can't use " + this.type_ + " in as conditions", this); return false }

  eq(_: SltValue): SltValue { return new SltBool(false, this.loc) }

  lt(other: SltValue): SltValue {return error("TypeError", "Cannot quantify " + this.type_ + " and " + other.type_ + " with appropriate quantities for '<'", this)}

  lte(other: SltValue): SltValue {return error("TypeError", "Cannot quantify " + this.type_ + " and " + other.type_ + " with appropriate quantities for '<='", this)}

  gt(other: SltValue): SltValue {return error("TypeError", "Cannot quantify " + this.type_ + " and " + other.type_ + " with appropriate quantities for '>'", this)}

  gte(other: SltValue): SltValue {return error("TypeError", "Cannot quantify " + this.type_ + " and " + other.type_ + " with appropriate quantities for '>='", this)}

  async getOutput(_: boolean = false) { 
    await write(this.toString())
  }

  neq(_: SltValue): SltValue { return new SltBool(true, this.loc) }

  anded(other: SltValue): SltValue { return error("TypeError", "Can't make 'and' of " + this.type_ + " and " + other.type_, this) }

  getHash() {
    uCount = uCount + 1
    return "Unhasable" + this.type_ + uCount.toString()
  }

  ored(other: SltValue): SltValue { return error("TypeError", "Can't make 'or' of " + this.type_ + " and " + other.type_, this) }

  getProperty(other: SltValue): SltValue { return error("KeyError", "Cannot get the property " + other.toString() + " of " + this.type_, this) }

  locate(loc: [number, number, string]): SltValue {
    this.loc = loc
    return this
  }

  concat(other: SltValue): SltValue { return error("TypeError", "Cannot concatenate " + this.type_ + " to " + other.type_, this); }

}

function destructure(struct: SltValue, args: number): SltValue[] {
  if (struct instanceof SltStruct) {
    struct.checkLength(args)
    let arr: SltValue[] = []
    for (const k in struct.value){
      arr.push(struct.value[k])
    }
    return arr
  }
  return [error("TypeError", "Cannot destructure " + struct.type_, struct)]
}

function unwrap(tup: SltValue, args: number){
  if (tup instanceof SltTuple){
    tup.checkLength(args)
    return tup.value
  }
  error("TypeError", "Cannot destructure " + tup.type_, tup)
}

class SltThunk extends SltValue {
  fun: () => SltValue
  constructor(func: () => SltValue, mutates: number | null = null){
    super()
    this.type_ = "SltThunk";
    this.mutates = mutates
    this.fun = func;
    this.value = null;
  }

  toString() { return "SltThunk" }

  _call() {
    if (this.value == null || this.mutates == Mutates) this.value = this.fun()
    return this.value
  }
}


class SltBool extends SltValue {
  value: boolean

  constructor(bool: boolean, loc: [number, number, string]) {
      super()
      this.type_ = "SltBool"
      this.value = bool
      this.loc = loc
      return new Proxy(this, {
        apply: (target, thisArg, args) => target._call(...args)
      })
  }

  ored(other: SltValue): SltValue {
      if (this.value) return new SltBool(true, this.loc)
      sameTypes(this, other(), (a, b) => a.ored(b))
      if (other().value) return new SltBool(true, this.loc)
      return new SltBool(false, this.loc)
  }

  anded(other: SltValue): SltValue {
      if (this.value) {
          if (other().value) return new SltBool(true, this.loc)
      } 
      return new SltBool(false, this.loc)
  }

  notted() { return new SltBool(!this.value, this.loc) }

  eq(other: SltValue) {
    if (this.type_ != other.type_) return new SltBool(false, this.loc)
    return new SltBool(this.value == other.value, this.loc)
  }

  neq(other: SltValue) { return this.eq(other).notted() }

  is_true() { return this.value }

  toString() { return this.value.toString() }
}


class SltNum extends SltValue {
  constructor(num: number, location: [number, number, string]){
    super()
    this.type_ = "SltNum";
    this.hashAble = false;
    this.loc = location;
    this.value = num;
  }

  add(other: SltValue): SltValue {
    sameTypes(this, other, (a, b) => a.add(b))
    return new SltNum(this.value + other.value, this.loc)
  }

  sub(other: SltValue): SltValue {
    sameTypes(this, other, (a, b) => a.sub(b))
    return new SltNum(this.value - other.value, this.loc)
  }

  mul(other: SltValue): SltValue {
    sameTypes(this, other, (a, b) => a.mul(b))
    return new SltNum(this.value * other.value, this.loc)
  }

  div(other: SltValue): SltValue {
    sameTypes(this, other, (a, b) => a.div(b))
    if (other.value == 0) error("DivisionByZero", "Can't divide by zero", this)
    return new SltNum(this.value + other.value, this.loc)
  }

  getHash() { return sha1(this.value.toString()) + "IsANumber" }

  neg() { return new SltNum(-this.value, this.loc) }

  concat(other: SltValue){
    if (this.value != 0) return super.concat(other)
    return other()
  }

  eq(other: SltValue) {
    if (this.type_ != other.type_) return new SltBool(false, this.loc)
    return new SltBool(this.value == other.value, this.loc)
  }    

  neq(other: SltValue) {
    if (this.type_ != other.type_) return new SltBool(true, this.loc)
    return new SltBool(this.value != other.value, this.loc)
  }

  lt(other: SltValue) {
    sameTypes(this, other, (_, b) => super.lt(b))
    return new SltBool(this.value < other.value, this.loc)
  }

  lte(other: SltValue) {
    sameTypes(this, other, (_, b) => super.lte(b))
    return new SltBool(this.value <= other.value, this.loc)
  }

  gte(other: SltValue) {
    sameTypes(this, other, (_, b) => super.gte(b))
    return new SltBool(this.value >= other.value, this.loc)
  }

  gt(other: SltValue) {
    sameTypes(this, other, (_, b) => super.gt(b))
    return new SltBool(this.value > other.value, this.loc)
  }

  toString() { return this.value.toString() }

}

interface HashTable<T> {
  [key: string]: T;
}

class SltFunc extends SltValue {
  fun: (a: SltValue) => SltValue
  values: HashTable<SltValue>
  constructor(fun: (a: SltValue) => SltValue, loc: [number, number, string]){
    super()
    this.fun = fun;
    this.type_ = "SltFunc"
    this.values = {}
    this.hashAble = false
    this.loc = loc
  }

  toString() { return "<function>" }

  eq(other: SltValue) { return new SltBool(false, this.loc) }
  neq(other: SltValue) { return new SltBool(true, this.loc) }

  getValue(arg: SltValue) {
    if (arg().hashAble) return this.values[arg().getHash()]
    return null
  }

  _call(a: SltValue, ..._: SltValue[]): SltValue {    
    const val = this.getValue(a)
    if (val != null) return val
    const res = this.fun(a)
    if (a().hashAble) this.values[a().getHash()] = res
    return res
  }
}

class SltList extends SltValue {
  constructor(head: SltValue, tail: SltValue, loc: [number, number, string]){
    super()
    this.head = new SltThunk(() => head);
    this.tail = new SltThunk(() => tail);
    this.isEmpty = false;
    this.loc = loc;
    this.type_ = "SltList";    
  }

  static fromValues(vals: SltValue[], loc: [number, number, string]){
    function reversed<T>(array: T[]){
      return array.map((item,idx) => array[array.length-1-idx])
    }

    let result = new SltNum(0, loc)
    const list = reversed(vals)
    const length = list.length
    let i = 0
    while (i<length){
      let headThunk = list[i]
      let tailThunk = result
      result = new SltList(headThunk, tailThunk, loc)
      i = i+1
    }
    return result
  }

  static direct(head: SltValue, tail: SltValue, loc: [number, number, string]){
    const list = new SltList(new SltValue(), new SltValue(), loc)
    list.head = head
    list.tail = tail
    list.loc = loc
    return list
  }

  toList(){
    let tailIter = this.tail()
    const result: SltValue[] = [this.head()]
    while (!tailIter.isEmpty){      
      result.push(tailIter.head(tailIter))
      tailIter = tailIter.tail(tailIter)
    }
    return result
  }

  concat(other: SltValue) {
    let tail: SltValue = this.tail()  
      
    if (tail.isEmpty) {
      const tailThunk = other
      const x = SltList.direct(new SltThunk(() => this.head()), tailThunk, this.loc)      
      return x
    }
    const thunk = new SltThunk(() => tail.concat(other))
    return SltList.direct(new SltThunk(() => this.head()), thunk, this.loc)
  }

  eq(other: SltValue){
    if (other.type_ != this.type_) return new SltBool(false, this.loc)
    if (!(other instanceof SltList)) return new SltBool(false, this.loc)
    const this_seq = this.toList()
    const other_seq = other.toList()
    for (const k in this_seq) {
      const v = this_seq[k]
      if (other_seq[k] != undefined) return new SltBool(false, this.loc)
      if (!v.eq(other_seq[k]).value) return new SltBool(false, this.loc)
    }
    return new SltBool(true, this.loc)
  }

  neq(other: SltValue){
    return new SltBool(!(this.eq(other).value), this.loc)
  }

  locate(location: [number, number, string]) {
    this.loc = location 
    return this
  }

  async getOutput(whole: boolean){    
    let output = this
    if (whole) { 
      await write("[")
      while (output.type_ == "SltList"){
        await output.head().getOutput(true)
        await write(", ")
        output = output.tail()
      }
      await write("]")
      return
    }
    while (output.type_ == "SltList") {
      await output.head().getOutput(true)
      await write("\n\n")
      output = output.tail()
    }
    return
  }

  toString(){
    return "[" + this.toList().map(x => x.toString()).join(", ") + "]"
  }
}


class SltTuple extends SltValue {
  constructor(tp: SltValue[], loc: [number, number, string]){
    super()
    this.type_ = "SltTuple"
    this.value = tp
    this.loc = loc
  }

  checkLength(length: number){
    if (length != this.value.length) {
      error(
        "TypeError", "Cannot destructure length " + this.value.length.toString() + " tuple to " + length + " names", this
      )
    }
    return this
  }

  eq(other: SltValue) {
    if (this.type_ != other.type_) return new SltBool(false, this.loc)
    const bool_false = new SltBool(false, this.loc)
    if (this.value.length != other.value.length) return bool_false
    let main_bool = new SltBool(true, this.loc)
    for (const i in this.value){         
      const v = this.value[i]      
      if (other.value[i] == undefined || other.value[i] == null) return bool_false
      if (v().neq(other.value[i]()).is_true()) return bool_false
    }
    return main_bool
  }

  getHash() { return this.value.map((v: SltValue) => v().getHash()).join("NexT") }

  neq(other: SltValue){
    sameTypes(this, other, super.neq)
    return (this.eq(other)).notted()
  }

  toString() { return "(" + this.value.map((v: SltValue) => v().toString()).join(", ") + ",)" }

}

class SltType extends SltValue {
  constructor(t: string, loc: [number, number, string]){
    super()
    this.type_ = t;
    this.loc = loc;
  }

  toString(){ return "<" + this.type_ + ">" }

  getHash(){ return sha1(this.type_) + "IsATypeRepr" }

}

class SltString extends SltValue {
  constructor(str: string, loc: [number, number, string]){
    super()
    this.type_ = "SltString"
    this.hashAble = false
    this.value = str
    this.loc = loc
  }

  concat(other: SltValue){
    sameTypes(this, other(), super.concat)
    return new SltString(this.value + other().value, this.loc)
  }

  getHash() { return sha1(this.value.toString()) + "IsAString" }

  eq(other: SltValue) { return new SltBool(this.value == other.value, this.loc) }

  neq(other: SltValue) { return this.eq(other).notted() }

  toString() { return this.value }

}

class SltStruct extends SltValue {
  constructor(name: string, overarch: string, canHash: boolean, tb: HashTable<SltValue>, loc: [number, number, string]){
    super()
    this.type_ = name
    this.overarch = overarch
    this.hashAble = canHash
    this.loc = loc
    this.value = tb
  }

  isType(other: SltValue | string) {
    if (other instanceof SltValue) return new SltBool(this.type_ == other.type_ || this.overarch == other.type_, this.loc) 
    return new SltBool(this.type_ == other || this.overarch == other, this.loc)
  }

  checkLength(length: number){    
    if (length != Object.keys(this.value).length) {
      error(
        "TypeError", "Cannot destructure length " + Object.keys(this.value).length.toString() + " struct to " + length.toString() + " names", this
      )
    }
    return this
  }

  getHash(){
    const init = sha1(this.type_)
    let ls = init
    for (const k in this.value) {
      ls = ls + sha1(k) + "-to-" + this.value[k]().getHash()
    }
    return ls + "From" + sha1(this.overarch || "NoOverArch")
  }

  eq(other: SltValue): SltValue {
    const eq_struct = (ta: HashTable<SltValue>, tb: HashTable<SltValue>) => {
      if (ta.length != tb.length) return new SltBool(false, this.loc)
      for (const k in ta){
        const v = ta[k]
        if (!(k in tb.hasKey)) return new SltBool(false, this.loc)
        if (!tb[k]().eq(v())) return new SltBool(false, this.loc)
      }
      return new SltBool(true, this.loc)
    }

    if (other instanceof SltStruct) return new SltBool(this.type_ == other.type_ && this.overarch == other.overarch && eq_struct(this.value, other.value).value, this.loc)
    else return new SltBool(false, this.loc)
  }

  neq(other: SltValue) { return this.eq(other).notted() }

  toString(){
    const init = this.type_
    let ls = init + "{"
    const slen = this.value.toString()
    if (slen == 0) return init
    let i = 0
    for (const k in this.value){
      const v = this.value[k]
      ls = ls + k + " :: " + v().toString()
      ls = i == slen-1 && ls || ls + ", "
      i = i+1
    }
    return ls + "}"
  }

  tableString() { return "{" + this.value.join(", ") + "}" }

  getProperty(property: SltValue){
    if (!(property instanceof SltString)) error("TypeError", "Expected string found " + property.toString(), this)    
    if (property.value in this.value) return this.value[property.value]
    error("IndexError", property.toString() + " not found in " + this.tableString(), this)
  }
}


/////////////////////////////
// Base functions

const listHead = 
  new SltThunk(
    () => new SltFunc((ls) => ls().head(), [0, 0, "core"])
  )

const listTail = 
  new SltThunk(
    () => new SltFunc((ls) => ls().tail(), [0, 0, "core"])
  )

const baseStringify = 
  new SltThunk(
    () => new SltFunc(t => new SltString(t().toString(), t().loc), [0, 0, "core"])
  )

const unsafeMod = 
  new SltThunk(
    () => {
      return new SltFunc(
        a =>
          new SltFunc(
            b => new SltNum(a().value % b().value, b().loc),
            a().loc
          ),
          [0, 0, "core"]
        )
      }
    )

const evaluate = new SltThunk(
  () =>
    new SltFunc(
      t => {
        if (t.type_ != "SltThunk") return error("TypeError", "Cannot evaluate a " + t.type_, t)
        t().toString()
        return t()
      },
      [0, 0, "core"]
    )
)

const typeOf = new SltThunk(
  () =>
    new SltFunc(
      a => new SltType(a().type_, a().loc),
      [0, 0, "core"]
    )
)

const ovTypeOf = new SltThunk(
  () =>
    new SltFunc(
      a => new SltType(a().overarch ? a().overarch : a().type_, a().loc),
      [0, 0, "core"]
    )
)

const unsafeWrite = new SltThunk(
  () => new SltFunc(
    exp => new SltFunc(
      st => {        
        exp().getOutput(true)
        return exp()
      },
      exp().loc
    ),
    [0, 0, "core"]
  ),
  Mutates
)

export {
  SltValue, SltNum, SltFunc, SltString, 
  SltStruct, SltList, SltThunk, SltTuple, 
  SltType, destructure, unwrap, listHead,
  listTail, baseStringify, unsafeMod, evaluate,
  typeOf, ovTypeOf, unsafeWrite, SltBool,
  error, Mutates, write
}