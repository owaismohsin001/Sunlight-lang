import { SltFunc, SltThunk, SltStruct, error } from "../../SltRuntime.ts"

const cloneFun = self => {
    var that = self;
    var temp = function temporary() { return that.apply(self, arguments); };
    for(var key in self) {
        if (self.hasOwnProperty(key)) {
            temp[key] = self[key];
        }
    }
    return temp;
};

function clone(obj){
    if (!(obj instanceof Object)) return obj
    const new_obj = new obj.constructor()
    for (const k in obj){
        new_obj[k] = null
    }
    for (const k in obj){
        const v = obj[k]
        if (v instanceof Function) new_obj[k] = cloneFun(v)
        else new_obj[k] = clone(v)
    }
    return new_obj
}


const baseModify = new SltThunk( 
    () => new SltFunc(
        (struct) => new SltFunc(
            (str) => new SltFunc(
                (fun) => {
                    const a = clone(struct())
                    console.log(a);
                    if (!(a instanceof SltStruct)) error("first argument TypeError", "Must be a struct rather than being " + a.type_, a)
                    if (str().value in a.value) {
                        const val = a.value[str().value]
                        console.log(val());
                        a.value[str().value] = new SltThunk(() => fun()(val))
                        return a
                    }
                    error("first argument KeyError",  "Key " + str().toString() + " not found in " + struct().toString(), a)
                },
                [1, 1, "modify.slt"]
            ),
            [1, 1, "modify.slt"]
        ), 
        [1, 1, "modify.js"]
    )
)

export {baseModify}