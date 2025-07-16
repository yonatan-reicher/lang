use crate::context::{Context, Module};
use crate::value::{BuiltinDefinition, Value};


impl Module {
    pub fn insert(&mut self, builtin: BuiltinDefinition) {
        self.values.insert(builtin.name.clone(), builtin.into());
    }
}


fn stdlib() -> Module {
    let mut ret = Module {
        values: [].into(),
    };
    ret.insert(BuiltinDefinition {
        name: "abs".into(),
        arity: 1,
        func: |values| {
            let [Value::Int(i)] = values else {
                panic!("AARG");
            };
            i.abs().into()
        },
    });
    ret.insert(BuiltinDefinition {
        name: "neg".into(),
        arity: 1,
        func: |values| {
            let [Value::Int(i)] = values else { panic!("AARG"); };
            (-i).into()
        },
    });
    ret
}


impl Context {
    pub fn add_stdlib(&mut self) {
        self.modules.insert("stdlib".to_string(), stdlib());
    }
}
