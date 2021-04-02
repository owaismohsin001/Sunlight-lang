import {SltThunk, SltFunc, SltNum, SltValue, error} from "../../SltRuntime.ts"

const isNum = n => !isNaN(parseFloat(n().value))

const stringToInt = new SltThunk(
    () =>
        new SltFunc(
            i => {
                    if (isNum(i)) error("TypeError: ", "First argument is not a string", i())
                    else return new SltNum(i().value.parseFloat(), [6, 1, "intify.lua"])
                }, 
                [2, 5, "intify.lua"]
            )
)

export {stringToInt, isNum}