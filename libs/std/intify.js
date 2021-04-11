import {SltThunk, SltFunc, SltNum, SltBool, error} from "../../SltRuntime.ts"

const isNum = n => !isNaN(n().toString()) && !isNaN(parseFloat(n().toString()))

const isNumBase = new SltThunk(
    () => new SltFunc(n => new SltBool(isNum(n), n().loc), [7, 2, "intify.js"])
)

const stringToInt = new SltThunk(
    () =>
        new SltFunc(
            i => {
                    if (isNum(i)) error("TypeError: ", "First argument is not a string", i())
                    else return new SltNum(i().value.parseFloat(), [6, 1, "intify.js"])
                }, 
                [2, 5, "intify.js"]
            )
)

export {stringToInt, isNumBase}