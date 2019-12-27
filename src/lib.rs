use core::convert::{TryFrom, TryInto};
use core::fmt;
use core::iter::{self, FromIterator};

use parity_wasm::elements as pwasm;

pub use parity_wasm::elements::{
    BlockType, BrTableData, CustomSection, ExportEntry, External, GlobalType, ImportEntry,
    Instruction, Internal, MemoryType, ResizableLimits, TableElementType, TableType, ValueType,
};
pub use parity_wasm::SerializationError;
pub use wasmi_validation::Error as ValidationError;

pub const PAGE_SIZE: u32 = 64 * 1024; // 64 KiB

#[derive(Debug)]
pub enum LoadError {
    SerializationError(SerializationError),
    ValidationError(ValidationError),
}

impl fmt::Display for LoadError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::SerializationError(error) => write!(f, "Error while serializing file: {}", error),
            Self::ValidationError(error) => write!(f, "Error while validating file: {}", error),
        }
    }
}

impl From<SerializationError> for LoadError {
    fn from(error: SerializationError) -> Self {
        Self::SerializationError(error)
    }
}

impl From<ValidationError> for LoadError {
    fn from(error: ValidationError) -> Self {
        Self::ValidationError(error)
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct FunctionType {
    type_ref: u32,
    params: Vec<ValueType>,
    return_type: Option<ValueType>,
}

impl FunctionType {
    fn new(type_ref: u32, func_type: &mut pwasm::FunctionType) -> Self {
        FunctionType {
            type_ref,
            params: take(func_type.params_mut()),
            return_type: take(func_type.return_type_mut()),
        }
    }
    pub const fn type_ref(&self) -> u32 {
        self.type_ref
    }
    pub fn params(&self) -> &[ValueType] {
        &self.params
    }
    pub fn param_count(&self) -> u32 {
        self.params.len() as u32
    }
    pub const fn return_type(&self) -> Option<ValueType> {
        self.return_type
    }
}

impl fmt::Display for FunctionType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let params = self
            .params
            .iter()
            .map(|t| t.to_string())
            .collect::<Vec<String>>()
            .join(", ");
        let return_type = match self.return_type {
            Some(return_type) => return_type.to_string(),
            None => String::from("()"),
        };
        write!(f, "fn ({}) -> {}", params, return_type)
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Function {
    name: String,
    func_type: FunctionType,
    is_imported: bool,
    locals: Vec<ValueType>,
    instructions: Vec<Instruction>,
}

impl Function {
    const fn new(
        name: String,
        func_type: FunctionType,
        locals: Vec<ValueType>,
        instructions: Vec<Instruction>,
    ) -> Self {
        Function {
            name,
            func_type,
            is_imported: false,
            locals,
            instructions,
        }
    }

    fn new_imported(name: String, func_type: FunctionType) -> Self {
        Function {
            name,
            func_type,
            is_imported: true,
            locals: Vec::new(),
            instructions: Vec::new(),
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }
    pub const fn func_type(&self) -> &FunctionType {
        &self.func_type
    }
    pub const fn type_ref(&self) -> u32 {
        self.func_type.type_ref()
    }
    pub fn params(&self) -> &[ValueType] {
        self.func_type.params()
    }
    pub fn param_count(&self) -> u32 {
        self.func_type.param_count()
    }
    pub const fn return_type(&self) -> Option<ValueType> {
        self.func_type().return_type()
    }
    pub const fn is_imported(&self) -> bool {
        self.is_imported
    }
    pub fn locals(&self) -> &[ValueType] {
        &self.locals
    }
    pub fn instructions(&self) -> &[Instruction] {
        &self.instructions
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "fn {}{}", self.name, &self.func_type.to_string()[3..])
    }
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum InitExpr {
    I32Const(i32),
    I64Const(i64),
    F32Const(u32),
    F64Const(u64),
    Global(u32),
}

impl TryFrom<&pwasm::InitExpr> for InitExpr {
    type Error = String;

    fn try_from(init_expr: &pwasm::InitExpr) -> Result<Self, Self::Error> {
        let instrs = init_expr.code();
        if instrs.len() != 2 {
            return Err(format!("Init expr has invalid length: {}", instrs.len()));
        }
        if instrs[1] != Instruction::End {
            return Err("Init expr has multiple instructions".to_string());
        }
        match &instrs[0] {
            Instruction::I32Const(val) => Ok(InitExpr::I32Const(*val)),
            Instruction::I64Const(val) => Ok(InitExpr::I64Const(*val)),
            Instruction::F32Const(val) => Ok(InitExpr::F32Const(*val)),
            Instruction::F64Const(val) => Ok(InitExpr::F64Const(*val)),
            Instruction::GetGlobal(index) => Ok(InitExpr::Global(*index)),
            other => Err(format!("Invalid instruction in init expr: {}", other)),
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct Global {
    name: String,
    is_imported: bool,
    is_mutable: bool,
    value_type: ValueType,
    init_expr: InitExpr,
}

impl Global {
    fn from_parity(name: String, global: &pwasm::GlobalEntry) -> Self {
        let global_type = global.global_type();
        Global {
            name,
            is_imported: false,
            is_mutable: global_type.is_mutable(),
            value_type: global_type.content_type(),
            init_expr: global.init_expr().try_into().unwrap(),
        }
    }
    fn from_import(name: String, index: u32, global_type: pwasm::GlobalType) -> Self {
        Global {
            name,
            is_imported: true,
            is_mutable: global_type.is_mutable(),
            value_type: global_type.content_type(),
            init_expr: InitExpr::Global(index),
        }
    }
    pub fn name(&self) -> &str {
        &self.name
    }
    pub const fn is_imported(&self) -> bool {
        self.is_imported
    }
    pub const fn is_mutable(&self) -> bool {
        self.is_mutable
    }
    pub const fn value_type(&self) -> ValueType {
        self.value_type
    }
    pub const fn init_expr(&self) -> &InitExpr {
        &self.init_expr
    }
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub struct Table {
    is_imported: bool,
    elem_type: TableElementType,
    limits: ResizableLimits,
}

impl Table {
    pub const fn is_imported(&self) -> bool {
        self.is_imported
    }
    pub const fn elem_type(&self) -> TableElementType {
        self.elem_type
    }
    pub const fn limits(&self) -> &ResizableLimits {
        &self.limits
    }
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub struct Memory {
    is_imported: bool,
    limits: ResizableLimits,
}

impl Memory {
    pub const fn is_imported(&self) -> bool {
        self.is_imported
    }
    pub const fn limits(&self) -> &ResizableLimits {
        &self.limits
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct TableInit {
    index: u32,
    offset: InitExpr,
    entries: Vec<u32>,
}

impl TableInit {
    pub const fn index(&self) -> u32 {
        self.index
    }
    pub const fn offset(&self) -> &InitExpr {
        &self.offset
    }
    pub fn entries(&self) -> &[u32] {
        &self.entries
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct MemoryInit {
    index: u32,
    offset: InitExpr,
    data: Vec<u8>,
}

impl MemoryInit {
    pub const fn index(&self) -> u32 {
        self.index
    }
    pub const fn offset(&self) -> &InitExpr {
        &self.offset
    }
    pub fn data(&self) -> &[u8] {
        &self.data
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct Module {
    types: Vec<FunctionType>,
    functions: Vec<Function>,
    globals: Vec<Global>,
    tables: Vec<Table>,
    memories: Vec<Memory>,
    table_inits: Vec<TableInit>,
    memory_inits: Vec<MemoryInit>,
    imports: Vec<ImportEntry>,
    exports: Vec<ExportEntry>,
    start_func: Option<u32>,
    custom_sections: Vec<CustomSection>,
}

impl Module {
    pub fn from_file<P: AsRef<::std::path::Path>>(path: P) -> Result<Self, LoadError> {
        let module = parity_wasm::deserialize_file(path)?;
        wasmi_validation::validate_module::<wasmi_validation::PlainValidator>(&module)?;
        Ok(Module::from_parity_module(module))
    }

    fn from_parity_module(module: pwasm::Module) -> Self {
        // TODO: What happens when multiple functions have the same name?
        let mut module = match module.parse_names() {
            Ok(module) => module,
            Err((_, module)) => module,
        };

        let types = get_types(&mut module);

        let mut globals = Vec::new();
        let mut functions = Vec::new();
        let mut tables = Vec::new();
        let mut memories = Vec::new();
        let mut imports = Vec::new();
        let mut exports = Vec::new();

        if let Some(import_sec) = module.import_section_mut() {
            for entry in import_sec.entries() {
                let name = format!("{}.{}", entry.module(), entry.field());
                match entry.external() {
                    External::Function(type_ref) => {
                        let func_type = types[*type_ref as usize].clone();
                        functions.push(Function::new_imported(name, func_type))
                    }
                    External::Global(global_type) => globals.push(Global::from_import(
                        name,
                        globals.len() as u32,
                        *global_type,
                    )),
                    External::Table(table_type) => tables.push(Table {
                        is_imported: true,
                        elem_type: table_type.elem_type(),
                        limits: *table_type.limits(),
                    }),
                    External::Memory(memory_type) => memories.push(Memory {
                        is_imported: true,
                        limits: *memory_type.limits(),
                    }),
                }
            }
            imports = take(import_sec.entries_mut());
        }

        handle_global_section(&mut globals, &module);
        handle_function_section(&mut functions, &module, &types);
        handle_table_section(&mut tables, &mut module);
        handle_memory_section(&mut memories, &mut module);

        if let Some(export_sec) = module.export_section_mut() {
            for export in export_sec.entries() {
                match export.internal() {
                    Internal::Function(index) => {
                        functions[*index as usize].name = export.field().to_string()
                    }
                    Internal::Global(index) => {
                        globals[*index as usize].name = export.field().to_string()
                    }
                    _ => (),
                }
            }
            exports = take(export_sec.entries_mut());
        }

        if let Some(name_sec) = module.names_section() {
            if let Some(func_names) = name_sec.functions() {
                for (i, name) in func_names.names() {
                    functions[i as usize].name = name.clone();
                }
            }
        }

        Module {
            types,
            functions,
            globals,
            tables,
            memories,
            table_inits: get_table_inits(&mut module),
            memory_inits: get_memory_inits(&mut module),
            imports,
            exports,
            start_func: module.start_section(),
            custom_sections: Vec::from_iter(module.custom_sections().cloned()),
        }
    }

    pub fn types(&self) -> &[FunctionType] {
        &self.types
    }
    pub fn functions(&self) -> &[Function] {
        &self.functions
    }
    pub fn func(&self, index: u32) -> &Function {
        &self.functions[index as usize]
    }
    pub fn get_func(&self, index: u32) -> Option<&Function> {
        self.functions.get(index as usize)
    }
    pub fn globals(&self) -> &[Global] {
        &self.globals
    }
    pub fn tables(&self) -> &[Table] {
        &self.tables
    }
    pub fn memories(&self) -> &[Memory] {
        &self.memories
    }
    pub fn table_inits(&self) -> &[TableInit] {
        &self.table_inits
    }
    pub fn memory_inits(&self) -> &[MemoryInit] {
        &self.memory_inits
    }
    pub fn imports(&self) -> &[ImportEntry] {
        &self.imports
    }
    pub fn exports(&self) -> &[ExportEntry] {
        &self.exports
    }
    pub const fn start_func(&self) -> Option<u32> {
        self.start_func
    }
    pub fn custom_sections(&self) -> &[CustomSection] {
        &self.custom_sections
    }
}

fn get_types(module: &mut pwasm::Module) -> Vec<FunctionType> {
    match module.type_section_mut() {
        Some(type_sec) => type_sec
            .types_mut()
            .iter_mut()
            .enumerate()
            .map(|(i, t)| {
                let pwasm::Type::Function(func_type) = t;
                FunctionType::new(i as u32, func_type)
            })
            .collect(),
        None => Vec::new(),
    }
}

fn handle_global_section(globals: &mut Vec<Global>, module: &pwasm::Module) {
    if let Some(global_sec) = module.global_section() {
        for global in global_sec.entries() {
            let name = format!("global_{}", globals.len());
            globals.push(Global::from_parity(name, global));
        }
    }
}

fn handle_function_section(
    functions: &mut Vec<Function>,
    module: &pwasm::Module,
    types: &[FunctionType],
) {
    if let Some(func_sec) = module.function_section() {
        let func_bodies = module.code_section().map(|sec| sec.bodies()).unwrap_or(&[]);
        for (type_ref, body) in func_sec.entries().iter().zip(func_bodies.iter()) {
            let type_ref = type_ref.type_ref();
            let name = format!("func_{}", functions.len());
            let func_type = types[type_ref as usize].clone();
            let locals = body
                .locals()
                .iter()
                .flat_map(|locals| iter::repeat(locals.value_type()).take(locals.count() as usize))
                .collect();
            let instructions = body.code().elements().to_vec();
            functions.push(Function::new(name, func_type, locals, instructions));
        }
    }
}

fn handle_table_section(tables: &mut Vec<Table>, module: &mut pwasm::Module) {
    if let Some(table_sec) = module.table_section_mut() {
        tables.extend(table_sec.entries_mut().drain(..).map(|table_type| Table {
            is_imported: false,
            elem_type: table_type.elem_type(),
            limits: *table_type.limits(),
        }));
    }
}

fn handle_memory_section(memories: &mut Vec<Memory>, module: &mut pwasm::Module) {
    if let Some(memory_sec) = module.memory_section_mut() {
        memories.extend(
            memory_sec
                .entries_mut()
                .drain(..)
                .map(|memory_type| Memory {
                    is_imported: false,
                    limits: *memory_type.limits(),
                }),
        );
    }
}

fn get_table_inits(module: &mut pwasm::Module) -> Vec<TableInit> {
    let mut inits = Vec::new();
    if let Some(elements_sec) = module.elements_section_mut() {
        for init in elements_sec.entries_mut() {
            inits.push(TableInit {
                index: init.index(),
                offset: init.offset().as_ref().unwrap().try_into().unwrap(),
                entries: take(init.members_mut()),
            });
        }
    }
    inits
}

fn get_memory_inits(module: &mut pwasm::Module) -> Vec<MemoryInit> {
    let mut inits = Vec::new();
    if let Some(data_sec) = module.data_section_mut() {
        for init in data_sec.entries_mut() {
            inits.push(MemoryInit {
                index: init.index(),
                offset: init.offset().as_ref().unwrap().try_into().unwrap(),
                data: take(init.value_mut()),
            });
        }
    }
    inits
}

fn take<T: Default>(t: &mut T) -> T {
    std::mem::replace(t, T::default())
}
