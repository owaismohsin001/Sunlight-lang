import {SltFunc, SltThunk, SltNum} from '../../SltRuntime.ts'

const unsafeExponent = new SltThunk(
    () =>
        new SltFunc(
            a =>
                new SltFunc(
                    b => new SltNum(a().value ** b().value, a().pos),
                    [7, 1, "intify.lua"]
                ),
                [7, 1, "intify.lua"]
            ),
)

const unsafeCeil = new SltThunk(
    () => 
        new SltFunc(
            a => new SltNum(Math.ceil(a().value), a().pos),
            [7, 1, "intify.lua"]
        )
)

const unsafeFloor = new SltThunk(
    () => 
        new SltFunc(
            a => new SltNum(Math.floor(a().value), a().pos),
            [7, 1, "intify.lua"]
        )
)

const unsafeRound = new SltThunk(
    () => 
        new SltFunc(
            a => new SltNum(Math.round(a().value), a().pos),
            [7, 1, "intify.lua"]
        )
)

export {unsafeExponent, unsafeRound, unsafeCeil, unsafeFloor}