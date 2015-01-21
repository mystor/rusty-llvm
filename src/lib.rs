// CURRENT PROGRESS: LLVMConstNull
#![feature(unsafe_destructor)]
#![allow(unstable)]
#![experimental]

extern crate libc;

extern crate "rustc_llvm" as ll;

use std::mem;
use std::str::from_c_str;
use std::ffi::CString;
use libc::*;

// A bunch of enums which we need from the bindings. We re-export them
pub use ll::debuginfo;
pub use ll::{
    FileType,
    InlineAsmDiagHandler,
    DiagnosticHandler,
    DiagnosticKind,
    DiagnosticSeverity,
    AtomicOrdering,
    AtomicBinOp,
    TypeKind,
    Opcode,
    CodeGenModel,
    CodeGenOptLevel,
    RelocMode,
};
use debuginfo::{DIBuilderRef, DIDescriptor,
                DIFile, DILexicalBlock, DISubprogram, DIType,
                DIBasicType, DIDerivedType, DICompositeType,
                DIVariable, DIGlobalVariable, DIArray, DISubrange};


// Data objects in rusty llvm are intended to mirror the reference types
// which are used in the llvm-c api. They are always passed around by reference,
// and are cast into the correct type in order to pass them into the C function.
//
// The code here is pretty awful (mem::transmute sucks), but it allows for the
// "more safe" from_ll and as_ll functions to be used in the implementation of methods
// throughout the file.
trait LLRef<T> {
    unsafe fn as_ll(&self) -> T;
}
macro_rules! llref_ty{
    ($name:ident , $llname:ty) => {
        // An llref_ty is a reference to a c-type. It has an llname
        // (which is the name used in librustc_llvm), and can be converted
        // between that and itself
        #[allow(missing_copy_implementations)]
        pub enum $name {}
        impl $name {
            #[allow(dead_code)]
            unsafe fn from_ll<'a>(ll: $llname) -> Option<&'a $name> {
                mem::transmute(ll)
            }
        }
        impl LLRef<$llname> for $name {
            #[allow(dead_code)]
            unsafe fn as_ll(&self) -> $llname {
                mem::transmute(self)
            }
        }
        impl <'a> LLRef<$llname> for Option<&'a $name> {
            #[allow(dead_code)]
            unsafe fn as_ll(&self) -> $llname {
                mem::transmute(self)
            }
        }
    };
    ($name:ident <'a> , $llname:ty) => {
        // This is for if a created object has a limited lifetime.
        // For example, if you create a module in a context, it only lives as long
        // as that context. There are no instances of it currently in the file
        pub enum $name<'a> {}
        impl <'a> $name <'a> {
            #[allow(dead_code)]
            unsafe fn from_ll<'b>(ll: $llname) -> Option<&'b $name<'a>> {
                mem::transmute(ll)
            }
        }
        impl <'a> LLRef<$llname> for $name <'a> {
            #[allow(dead_code)]
            unsafe fn as_ll(&self) -> $llname {
                mem::transmute(self)
            }
        }
        impl <'a, 'b> LLRef<$llname> for Option<&'b $name <'a>> {
            #[allow(dead_code)]
            unsafe fn as_ll(&self) -> $llname {
                mem::transmute(self)
            }
        }
    }
}
// Ownable Types have Drop implemented on them, which will call the llvm destructor for the type
// when the type is dropped. In addition, as they are destructable, they can be cast from their
// raw pointer form into a Box<$name>, which will call the drop method when they are destroyed.
macro_rules! llref_ownable_ty{
    ($name:ident , $llname:ty , $dispose:expr) => {
        llref_ty!($name, $llname);

        impl Drop for $name {
            fn drop(&mut self) {
                unsafe { $dispose(self.as_ll()) }
            }
        }
        impl $name {
            unsafe fn box_from_ll(ll: $llname) -> Box<$name> {
                mem::transmute(ll)
            }
        }
    }
}

fn to_c_bool(b: bool) -> ll::Bool {
    if b { ll::True } else { ll::False }
}

fn from_c_bool(b: ll::Bool) -> bool {
    b != ll::False
}

fn str_to_c_str(s: &str) -> CString {
    CString::from_slice(s.as_bytes())
}

fn c_str_to_string(s: *const c_char) -> String {
    format!("{}", unsafe {
        from_c_str(s)
    })
}

impl <'a, T: LLRef<U>, U> LLRef<*const U> for [&'a T] {
    unsafe fn as_ll(&self) -> *const U {
        let ptr: *const &'a T = self.as_ptr();
        mem::transmute(ptr)
    }
}

llref_ty!(Type, ll::TypeRef);
impl Type {
    pub fn get_type_kind(&self) -> TypeKind {
        unsafe {
            ll::LLVMGetTypeKind(self.as_ll())
        }
    }
    pub fn get_type_context(&self) -> Option<&Context> {
        unsafe {
            Context::from_ll(ll::LLVMGetTypeContext(self.as_ll()))
        }
    }
    pub fn get_int_type_width(&self) -> c_uint {
        unsafe {
            ll::LLVMGetIntTypeWidth(self.as_ll())
        }
    }
    pub fn function_type(&self, param_types: &[&Type], is_var_arg: bool) -> Option<&Type> {
        unsafe {
            Type::from_ll(ll::LLVMFunctionType(self.as_ll(),
                                               param_types.as_ll(),
                                               param_types.len() as u32,
                                               to_c_bool(is_var_arg)))
        }
    }
    pub fn is_function_var_arg(&self) -> bool {
        unsafe {
            from_c_bool(ll::LLVMIsFunctionVarArg(self.as_ll()))
        }
    }
    pub fn get_return_type(&self) -> Option<&Type> {
        unsafe {
            Type::from_ll(ll::LLVMGetReturnType(self.as_ll()))
        }
    }
    pub fn count_param_types(&self) -> c_uint {
        unsafe {
            ll::LLVMCountParamTypes(self.as_ll())
        }
    }
    pub fn get_param_types(&self) -> Vec<Option<&Type>> {
        // TODO(michael): What would the best way be to implement this
        unimplemented!()
    }
    pub fn count_struct_element_types(&self) -> c_uint {
        unsafe {
            ll::LLVMCountStructElementTypes(self.as_ll())
        }
    }
    pub fn get_struct_element_types(&self) -> Vec<Option<&Type>> {
        // TODO(michael): What would be the best way to implement this
        unimplemented!()
    }
    pub fn is_packed_struct(&self) -> bool {
        unsafe {
            from_c_bool(ll::LLVMIsPackedStruct(self.as_ll()))
        }
    }
    pub fn rust_array_type(&self, element_count: u64) -> Option<&Type> {
        unsafe {
            Type::from_ll(ll::LLVMRustArrayType(self.as_ll(), element_count))
        }
    }
    pub fn pointer_type(&self, address_space: c_uint) -> Option<&Type> {
        unsafe {
            Type::from_ll(ll::LLVMPointerType(self.as_ll(), address_space))
        }
    }
    pub fn vector_type(&self, element_count: c_uint) -> Option<&Type> {
        unsafe {
            Type::from_ll(ll::LLVMVectorType(self.as_ll(), element_count))
        }
    }
    pub fn get_element_type(&self) -> Option<&Type> {
        unsafe {
            Type::from_ll(ll::LLVMGetElementType(self.as_ll()))
        }
    }
    pub fn get_array_length(&self) -> c_uint {
        unsafe {
            ll::LLVMGetArrayLength(self.as_ll())
        }
    }
    pub fn get_pointer_address_space(&self) -> c_uint {
        unsafe {
            ll::LLVMGetPointerAddressSpace(self.as_ll())
        }
    }
    pub fn get_vector_size(&self) -> c_uint {
        unsafe {
            ll::LLVMGetVectorSize(self.as_ll())
        }
    }
    pub fn const_null(&self) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMConstNull(self.as_ll()))
        }
    }
    pub fn const_all_ones(&self) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMConstAllOnes(self.as_ll()))
        }
    }
    pub fn get_undef(&self) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMGetUndef(self.as_ll()))
        }
    }
    pub fn const_pointer_null(&self) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMConstPointerNull(self.as_ll()))
        }
    }
    pub fn const_int(&self, n: c_ulonglong, sign_extend: bool) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMConstInt(self.as_ll(), n, to_c_bool(sign_extend)))
        }
    }
    pub fn const_int_of_string(&self, text: &str, radix: u8) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMConstIntOfString(self.as_ll(), str_to_c_str(text).as_ptr(), radix))
        }
    }
    pub fn const_int_of_string_and_size(&self, text: &str, s_len: c_uint, radix: u8) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMConstIntOfStringAndSize(self.as_ll(), str_to_c_str(text).as_ptr(), s_len, radix))
        }
    }
    pub fn const_real(&self, n: f64) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMConstReal(self.as_ll(), n))
        }
    }
    pub fn const_real_of_string(&self, text: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMConstRealOfString(self.as_ll(), str_to_c_str(text).as_ptr()))
        }
    }
    pub fn const_real_of_string_and_size(&self, text: &str, s_len: c_uint) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMConstRealOfStringAndSize(self.as_ll(), str_to_c_str(text).as_ptr(), s_len))
        }
    }
    pub fn const_array(&self, constant_vals: &[&Value]) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMConstArray(self.as_ll(),
                                              constant_vals.as_ll(),
                                              constant_vals.len() as u32))
        }
    }
    pub fn align_of(&self) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMAlignOf(self.as_ll()))
        }
    }
    pub fn size_of(&self) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMSizeOf(self.as_ll()))
        }
    }
    pub fn const_inline_asm(&self, asm_string: &str, constraints: &str, has_side_effects: bool, is_align_stack: bool) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMConstInlineAsm(self.as_ll(), str_to_c_str(asm_string).as_ptr(), str_to_c_str(constraints).as_ptr(), to_c_bool(has_side_effects), to_c_bool(is_align_stack)))
        }
    }
    pub fn struct_set_body(&self, element_types: &[&Type], packed: bool) {
        unsafe {
            ll::LLVMStructSetBody(self.as_ll(),
                                  element_types.as_ll(),
                                  element_types.len() as u32,
                                  to_c_bool(packed))
        }
    }
    pub fn const_named_struct(&self, constant_vals: &[&Value]) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMConstNamedStruct(self.as_ll(),
                                                    constant_vals.as_ll(),
                                                    constant_vals.len() as u32))
        }
    }
    pub fn inline_asm(&self, asm_string: &str, constraints: &str, side_effects: bool, align_stack: bool, dialect: c_uint) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMInlineAsm(self.as_ll(), str_to_c_str(asm_string).as_ptr(), str_to_c_str(constraints).as_ptr(), to_c_bool(side_effects), to_c_bool(align_stack), dialect))
        }
    }
    pub fn di_builder_create_op_deref(&self) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMDIBuilderCreateOpDeref(self.as_ll()))
        }
    }
    pub fn di_builder_create_op_plus(&self) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMDIBuilderCreateOpPlus(self.as_ll()))
        }
    }
    pub fn write_type_to_string(&self, s: &RustString) {
        unsafe {
            ll::LLVMWriteTypeToString(self.as_ll(), s.as_ll())
        }
    }
}
// TODO(michael): You can't create an ExecutionEngine... Weird.
llref_ownable_ty!(ExecutionEngine, ll::ExecutionEngineRef, ll::LLVMDisposeExecutionEngine);
impl ExecutionEngine {
    pub fn get_pointer_to_global(&self, v: &Value) -> *const ( ) {
        unsafe {
            ll::LLVMGetPointerToGlobal(self.as_ll(), v.as_ll())
        }
    }
}
llref_ty!(Use, ll::UseRef);
impl Use {
    pub fn get_next_use(&self) -> Option<&Use> {
        unsafe {
            Use::from_ll(ll::LLVMGetNextUse(self.as_ll()))
        }
    }
    pub fn get_user(&self) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMGetUser(self.as_ll()))
        }
    }
    pub fn get_used_value(&self) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMGetUsedValue(self.as_ll()))
        }
    }
}
llref_ty!(SMDiagnostic, ll::SMDiagnosticRef);
impl SMDiagnostic {
    pub fn write_sm_diagnostic_to_string(&self, s: &RustString) {
        unsafe {
            ll::LLVMWriteSMDiagnosticToString(self.as_ll(), s.as_ll())
        }
    }
}
llref_ownable_ty!(MemoryBuffer, ll::MemoryBufferRef, ll::LLVMDisposeMemoryBuffer);
impl MemoryBuffer {
    pub fn create_object_file(&self) -> Box<ObjectFile> {
        unsafe {
            ObjectFile::box_from_ll(ll::LLVMCreateObjectFile(self.as_ll()))
        }
    }
}
llref_ownable_ty!(TargetData, ll::TargetDataRef, ll::LLVMDisposeTargetData);
impl TargetData {
    pub fn add_target_data(&self, pm: &PassManager) {
        unsafe {
            ll::LLVMAddTargetData(self.as_ll(), pm.as_ll())
        }
    }
    pub fn store_size_of_type(&self, ty: &Type) -> c_ulonglong {
        unsafe {
            ll::LLVMStoreSizeOfType(self.as_ll(), ty.as_ll())
        }
    }
    pub fn size_of_type_in_bits(&self, ty: &Type) -> c_ulonglong {
        unsafe {
            ll::LLVMSizeOfTypeInBits(self.as_ll(), ty.as_ll())
        }
    }
    pub fn abi_size_of_type(&self, ty: &Type) -> c_ulonglong {
        unsafe {
            ll::LLVMABISizeOfType(self.as_ll(), ty.as_ll())
        }
    }
    pub fn preferred_alignment_of_type(&self, ty: &Type) -> c_uint {
        unsafe {
            ll::LLVMPreferredAlignmentOfType(self.as_ll(), ty.as_ll())
        }
    }
    pub fn abi_alignment_of_type(&self, ty: &Type) -> c_uint {
        unsafe {
            ll::LLVMABIAlignmentOfType(self.as_ll(), ty.as_ll())
        }
    }
    pub fn offset_of_element(&self, struct_ty: &Type, element: c_uint) -> c_ulonglong {
        unsafe {
            ll::LLVMOffsetOfElement(self.as_ll(), struct_ty.as_ll(), element)
        }
    }
    pub fn call_frame_alignment_of_type(&self, ty: &Type) -> c_uint {
        unsafe {
            ll::LLVMCallFrameAlignmentOfType(self.as_ll(), ty.as_ll())
        }
    }
}
llref_ownable_ty!(PassManager, ll::PassManagerRef, ll::LLVMDisposePassManager);
impl PassManager {
    pub fn run_pass_manager(&self, m: &Module) -> bool {
        unsafe {
            from_c_bool(ll::LLVMRunPassManager(self.as_ll(), m.as_ll()))
        }
    }
    pub fn run_function_pass_manager(&self, f: &Value) -> bool {
        unsafe {
            from_c_bool(ll::LLVMRunFunctionPassManager(self.as_ll(), f.as_ll()))
        }
    }
    pub fn initialize_function_pass_manager(&self) -> bool {
        unsafe {
            from_c_bool(ll::LLVMInitializeFunctionPassManager(self.as_ll()))
        }
    }
    pub fn finalize_function_pass_manager(&self) -> bool {
        unsafe {
            from_c_bool(ll::LLVMFinalizeFunctionPassManager(self.as_ll()))
        }
    }
    pub fn add_verifier_pass(&self) {
        unsafe {
            ll::LLVMAddVerifierPass(self.as_ll())
        }
    }
    pub fn add_global_optimizer_pass(&self) {
        unsafe {
            ll::LLVMAddGlobalOptimizerPass(self.as_ll())
        }
    }
    pub fn add_ipsccp_pass(&self) {
        unsafe {
            ll::LLVMAddIPSCCPPass(self.as_ll())
        }
    }
    pub fn add_dead_arg_elimination_pass(&self) {
        unsafe {
            ll::LLVMAddDeadArgEliminationPass(self.as_ll())
        }
    }
    pub fn add_instruction_combining_pass(&self) {
        unsafe {
            ll::LLVMAddInstructionCombiningPass(self.as_ll())
        }
    }
    pub fn add_cfg_simplification_pass(&self) {
        unsafe {
            ll::LLVMAddCFGSimplificationPass(self.as_ll())
        }
    }
    pub fn add_function_inlining_pass(&self) {
        unsafe {
            ll::LLVMAddFunctionInliningPass(self.as_ll())
        }
    }
    pub fn add_function_attrs_pass(&self) {
        unsafe {
            ll::LLVMAddFunctionAttrsPass(self.as_ll())
        }
    }
    pub fn add_scalar_repl_aggregates_pass(&self) {
        unsafe {
            ll::LLVMAddScalarReplAggregatesPass(self.as_ll())
        }
    }
    pub fn add_scalar_repl_aggregates_pass_ssa(&self) {
        unsafe {
            ll::LLVMAddScalarReplAggregatesPassSSA(self.as_ll())
        }
    }
    pub fn add_jump_threading_pass(&self) {
        unsafe {
            ll::LLVMAddJumpThreadingPass(self.as_ll())
        }
    }
    pub fn add_constant_propagation_pass(&self) {
        unsafe {
            ll::LLVMAddConstantPropagationPass(self.as_ll())
        }
    }
    pub fn add_reassociate_pass(&self) {
        unsafe {
            ll::LLVMAddReassociatePass(self.as_ll())
        }
    }
    pub fn add_loop_rotate_pass(&self) {
        unsafe {
            ll::LLVMAddLoopRotatePass(self.as_ll())
        }
    }
    pub fn add_licm_pass(&self) {
        unsafe {
            ll::LLVMAddLICMPass(self.as_ll())
        }
    }
    pub fn add_loop_unswitch_pass(&self) {
        unsafe {
            ll::LLVMAddLoopUnswitchPass(self.as_ll())
        }
    }
    pub fn add_loop_deletion_pass(&self) {
        unsafe {
            ll::LLVMAddLoopDeletionPass(self.as_ll())
        }
    }
    pub fn add_loop_unroll_pass(&self) {
        unsafe {
            ll::LLVMAddLoopUnrollPass(self.as_ll())
        }
    }
    pub fn add_gvn_pass(&self) {
        unsafe {
            ll::LLVMAddGVNPass(self.as_ll())
        }
    }
    pub fn add_mem_cpy_opt_pass(&self) {
        unsafe {
            ll::LLVMAddMemCpyOptPass(self.as_ll())
        }
    }
    pub fn add_sccp_pass(&self) {
        unsafe {
            ll::LLVMAddSCCPPass(self.as_ll())
        }
    }
    pub fn add_dead_store_elimination_pass(&self) {
        unsafe {
            ll::LLVMAddDeadStoreEliminationPass(self.as_ll())
        }
    }
    pub fn add_strip_dead_prototypes_pass(&self) {
        unsafe {
            ll::LLVMAddStripDeadPrototypesPass(self.as_ll())
        }
    }
    pub fn add_constant_merge_pass(&self) {
        unsafe {
            ll::LLVMAddConstantMergePass(self.as_ll())
        }
    }
    pub fn add_argument_promotion_pass(&self) {
        unsafe {
            ll::LLVMAddArgumentPromotionPass(self.as_ll())
        }
    }
    pub fn add_tail_call_elimination_pass(&self) {
        unsafe {
            ll::LLVMAddTailCallEliminationPass(self.as_ll())
        }
    }
    pub fn add_ind_var_simplify_pass(&self) {
        unsafe {
            ll::LLVMAddIndVarSimplifyPass(self.as_ll())
        }
    }
    pub fn add_aggressive_dce_pass(&self) {
        unsafe {
            ll::LLVMAddAggressiveDCEPass(self.as_ll())
        }
    }
    pub fn add_global_dce_pass(&self) {
        unsafe {
            ll::LLVMAddGlobalDCEPass(self.as_ll())
        }
    }
    pub fn add_correlated_value_propagation_pass(&self) {
        unsafe {
            ll::LLVMAddCorrelatedValuePropagationPass(self.as_ll())
        }
    }
    pub fn add_prune_eh_pass(&self) {
        unsafe {
            ll::LLVMAddPruneEHPass(self.as_ll())
        }
    }
    pub fn add_simplify_lib_calls_pass(&self) {
        unsafe {
            ll::LLVMAddSimplifyLibCallsPass(self.as_ll())
        }
    }
    pub fn add_loop_idiom_pass(&self) {
        unsafe {
            ll::LLVMAddLoopIdiomPass(self.as_ll())
        }
    }
    pub fn add_early_cse_pass(&self) {
        unsafe {
            ll::LLVMAddEarlyCSEPass(self.as_ll())
        }
    }
    pub fn add_type_based_alias_analysis_pass(&self) {
        unsafe {
            ll::LLVMAddTypeBasedAliasAnalysisPass(self.as_ll())
        }
    }
    pub fn add_basic_alias_analysis_pass(&self) {
        unsafe {
            ll::LLVMAddBasicAliasAnalysisPass(self.as_ll())
        }
    }
    pub fn rust_add_pass(&self, pass: &str) -> bool {
        unsafe {
            ll::LLVMRustAddPass(self.as_ll(), str_to_c_str(pass).as_ptr())
        }
    }
    pub fn rust_add_library_info(&self, m: &Module, disable_simplify_lib_calls: bool) {
        unsafe {
            ll::LLVMRustAddLibraryInfo(self.as_ll(), m.as_ll(), disable_simplify_lib_calls)
        }
    }
    pub fn rust_run_function_pass_manager(&self, m: &Module) {
        unsafe {
            ll::LLVMRustRunFunctionPassManager(self.as_ll(), m.as_ll())
        }
    }
    pub fn rust_print_module(&self, m: &Module, output: &str) {
        unsafe {
            ll::LLVMRustPrintModule(self.as_ll(), m.as_ll(), str_to_c_str(output).as_ptr())
        }
    }
}
llref_ownable_ty!(ObjectFile, ll::ObjectFileRef, ll::LLVMDisposeObjectFile);
impl ObjectFile {
    pub fn get_sections(&self) -> Box<SectionIterator> {
        unsafe {
            SectionIterator::box_from_ll(ll::LLVMGetSections(self.as_ll()))
        }
    }
    pub fn is_section_iterator_at_end(&self, si: &SectionIterator) -> bool {
        unsafe {
            from_c_bool(ll::LLVMIsSectionIteratorAtEnd(self.as_ll(), si.as_ll()))
        }
    }
}
llref_ownable_ty!(Builder, ll::BuilderRef, ll::LLVMDisposeBuilder);
impl Builder {
    pub fn position_builder(&self, block: &BasicBlock, instr: &Value) {
        unsafe {
            ll::LLVMPositionBuilder(self.as_ll(), block.as_ll(), instr.as_ll())
        }
    }
    pub fn position_builder_before(&self, instr: &Value) {
        unsafe {
            ll::LLVMPositionBuilderBefore(self.as_ll(), instr.as_ll())
        }
    }
    pub fn position_builder_at_end(&self, block: &BasicBlock) {
        unsafe {
            ll::LLVMPositionBuilderAtEnd(self.as_ll(), block.as_ll())
        }
    }
    pub fn get_insert_block(&self) -> Option<&BasicBlock> {
        unsafe {
            BasicBlock::from_ll(ll::LLVMGetInsertBlock(self.as_ll()))
        }
    }
    pub fn clear_insertion_position(&self) {
        unsafe {
            ll::LLVMClearInsertionPosition(self.as_ll())
        }
    }
    pub fn insert_into_builder(&self, instr: &Value) {
        unsafe {
            ll::LLVMInsertIntoBuilder(self.as_ll(), instr.as_ll())
        }
    }
    pub fn insert_into_builder_with_name(&self, instr: &Value, name: &str) {
        unsafe {
            ll::LLVMInsertIntoBuilderWithName(self.as_ll(), instr.as_ll(), str_to_c_str(name).as_ptr())
        }
    }
    pub fn set_current_debug_location(&self, l: &Value) {
        unsafe {
            ll::LLVMSetCurrentDebugLocation(self.as_ll(), l.as_ll())
        }
    }
    pub fn get_current_debug_location(&self) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMGetCurrentDebugLocation(self.as_ll()))
        }
    }
    pub fn set_inst_debug_location(&self, inst: &Value) {
        unsafe {
            ll::LLVMSetInstDebugLocation(self.as_ll(), inst.as_ll())
        }
    }
    pub fn build_ret_void(&self) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildRetVoid(self.as_ll()))
        }
    }
    pub fn build_ret(&self, v: &Value) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildRet(self.as_ll(), v.as_ll()))
        }
    }
    pub fn build_aggregate_ret(&self, ret_vals: &[&Value]) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildAggregateRet(self.as_ll(),
                                                     ret_vals.as_ll(),
                                                     ret_vals.len() as u32))
        }
    }
    pub fn build_br(&self, dest: &BasicBlock) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildBr(self.as_ll(), dest.as_ll()))
        }
    }
    pub fn build_cond_br(&self, cond: &Value, then: &BasicBlock, alt: &BasicBlock) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildCondBr(self.as_ll(), cond.as_ll(), then.as_ll(), alt.as_ll()))
        }
    }
    pub fn build_switch(&self, v: &Value, alt: &BasicBlock, num_cases: c_uint) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildSwitch(self.as_ll(), v.as_ll(), alt.as_ll(), num_cases))
        }
    }
    pub fn build_indirect_br(&self, addr: &Value, num_dests: c_uint) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildIndirectBr(self.as_ll(), addr.as_ll(), num_dests))
        }
    }
    pub fn build_invoke(&self,
                        func: &Value,
                        args: &[&Value],
                        then: &BasicBlock,
                        catch: &BasicBlock,
                        name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildInvoke(self.as_ll(),
                                               func.as_ll(),
                                               args.as_ll(),
                                               args.len() as u32,
                                               then.as_ll(),
                                               catch.as_ll(),
                                               str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_landing_pad(&self, ty: &Type, pers_fn: &Value, num_clauses: c_uint, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildLandingPad(self.as_ll(), ty.as_ll(), pers_fn.as_ll(), num_clauses, str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_resume(&self, exn: &Value) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildResume(self.as_ll(), exn.as_ll()))
        }
    }
    pub fn build_unreachable(&self) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildUnreachable(self.as_ll()))
        }
    }
    pub fn build_add(&self, lhs: &Value, rhs: &Value, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildAdd(self.as_ll(), lhs.as_ll(), rhs.as_ll(), str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_nsw_add(&self, lhs: &Value, rhs: &Value, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildNSWAdd(self.as_ll(), lhs.as_ll(), rhs.as_ll(), str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_nuw_add(&self, lhs: &Value, rhs: &Value, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildNUWAdd(self.as_ll(), lhs.as_ll(), rhs.as_ll(), str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_f_add(&self, lhs: &Value, rhs: &Value, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildFAdd(self.as_ll(), lhs.as_ll(), rhs.as_ll(), str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_sub(&self, lhs: &Value, rhs: &Value, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildSub(self.as_ll(), lhs.as_ll(), rhs.as_ll(), str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_nsw_sub(&self, lhs: &Value, rhs: &Value, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildNSWSub(self.as_ll(), lhs.as_ll(), rhs.as_ll(), str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_nuw_sub(&self, lhs: &Value, rhs: &Value, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildNUWSub(self.as_ll(), lhs.as_ll(), rhs.as_ll(), str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_f_sub(&self, lhs: &Value, rhs: &Value, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildFSub(self.as_ll(), lhs.as_ll(), rhs.as_ll(), str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_mul(&self, lhs: &Value, rhs: &Value, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildMul(self.as_ll(), lhs.as_ll(), rhs.as_ll(), str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_nsw_mul(&self, lhs: &Value, rhs: &Value, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildNSWMul(self.as_ll(), lhs.as_ll(), rhs.as_ll(), str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_nuw_mul(&self, lhs: &Value, rhs: &Value, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildNUWMul(self.as_ll(), lhs.as_ll(), rhs.as_ll(), str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_f_mul(&self, lhs: &Value, rhs: &Value, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildFMul(self.as_ll(), lhs.as_ll(), rhs.as_ll(), str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_u_div(&self, lhs: &Value, rhs: &Value, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildUDiv(self.as_ll(), lhs.as_ll(), rhs.as_ll(), str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_s_div(&self, lhs: &Value, rhs: &Value, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildSDiv(self.as_ll(), lhs.as_ll(), rhs.as_ll(), str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_exact_s_div(&self, lhs: &Value, rhs: &Value, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildExactSDiv(self.as_ll(), lhs.as_ll(), rhs.as_ll(), str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_f_div(&self, lhs: &Value, rhs: &Value, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildFDiv(self.as_ll(), lhs.as_ll(), rhs.as_ll(), str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_u_rem(&self, lhs: &Value, rhs: &Value, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildURem(self.as_ll(), lhs.as_ll(), rhs.as_ll(), str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_s_rem(&self, lhs: &Value, rhs: &Value, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildSRem(self.as_ll(), lhs.as_ll(), rhs.as_ll(), str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_f_rem(&self, lhs: &Value, rhs: &Value, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildFRem(self.as_ll(), lhs.as_ll(), rhs.as_ll(), str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_shl(&self, lhs: &Value, rhs: &Value, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildShl(self.as_ll(), lhs.as_ll(), rhs.as_ll(), str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_l_shr(&self, lhs: &Value, rhs: &Value, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildLShr(self.as_ll(), lhs.as_ll(), rhs.as_ll(), str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_a_shr(&self, lhs: &Value, rhs: &Value, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildAShr(self.as_ll(), lhs.as_ll(), rhs.as_ll(), str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_and(&self, lhs: &Value, rhs: &Value, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildAnd(self.as_ll(), lhs.as_ll(), rhs.as_ll(), str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_or(&self, lhs: &Value, rhs: &Value, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildOr(self.as_ll(), lhs.as_ll(), rhs.as_ll(), str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_xor(&self, lhs: &Value, rhs: &Value, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildXor(self.as_ll(), lhs.as_ll(), rhs.as_ll(), str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_bin_op(&self, op: Opcode, lhs: &Value, rhs: &Value, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildBinOp(self.as_ll(), op, lhs.as_ll(), rhs.as_ll(), str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_neg(&self, v: &Value, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildNeg(self.as_ll(), v.as_ll(), str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_nsw_neg(&self, v: &Value, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildNSWNeg(self.as_ll(), v.as_ll(), str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_nuw_neg(&self, v: &Value, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildNUWNeg(self.as_ll(), v.as_ll(), str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_f_neg(&self, v: &Value, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildFNeg(self.as_ll(), v.as_ll(), str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_not(&self, v: &Value, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildNot(self.as_ll(), v.as_ll(), str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_malloc(&self, ty: &Type, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildMalloc(self.as_ll(), ty.as_ll(), str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_array_malloc(&self, ty: &Type, val: &Value, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildArrayMalloc(self.as_ll(), ty.as_ll(), val.as_ll(), str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_alloca(&self, ty: &Type, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildAlloca(self.as_ll(), ty.as_ll(), str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_array_alloca(&self, ty: &Type, val: &Value, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildArrayAlloca(self.as_ll(), ty.as_ll(), val.as_ll(), str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_free(&self, pointer_val: &Value) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildFree(self.as_ll(), pointer_val.as_ll()))
        }
    }
    pub fn build_load(&self, pointer_val: &Value, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildLoad(self.as_ll(), pointer_val.as_ll(), str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_store(&self, val: &Value, ptr: &Value) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildStore(self.as_ll(), val.as_ll(), ptr.as_ll()))
        }
    }
    pub fn build_gep(&self,
                     pointer: &Value,
                     indices: &[&Value],
                     name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildGEP(self.as_ll(),
                                            pointer.as_ll(),
                                            indices.as_ll(),
                                            indices.len() as u32,
                                            str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_in_bounds_gep(&self,
                               pointer: &Value,
                               indices: &[&Value],
                               name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildInBoundsGEP(self.as_ll(),
                                                    pointer.as_ll(),
                                                    indices.as_ll(),
                                                    indices.len() as u32,
                                                    str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_struct_gep(&self, pointer: &Value, idx: c_uint, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildStructGEP(self.as_ll(), pointer.as_ll(), idx, str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_global_string(&self, str: &str, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildGlobalString(self.as_ll(), str_to_c_str(str).as_ptr(), str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_global_string_ptr(&self, str: &str, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildGlobalStringPtr(self.as_ll(), str_to_c_str(str).as_ptr(), str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_trunc(&self, val: &Value, dest_ty: &Type, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildTrunc(self.as_ll(), val.as_ll(), dest_ty.as_ll(), str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_z_ext(&self, val: &Value, dest_ty: &Type, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildZExt(self.as_ll(), val.as_ll(), dest_ty.as_ll(), str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_s_ext(&self, val: &Value, dest_ty: &Type, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildSExt(self.as_ll(), val.as_ll(), dest_ty.as_ll(), str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_fp_to_ui(&self, val: &Value, dest_ty: &Type, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildFPToUI(self.as_ll(), val.as_ll(), dest_ty.as_ll(), str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_fp_to_si(&self, val: &Value, dest_ty: &Type, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildFPToSI(self.as_ll(), val.as_ll(), dest_ty.as_ll(), str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_ui_to_fp(&self, val: &Value, dest_ty: &Type, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildUIToFP(self.as_ll(), val.as_ll(), dest_ty.as_ll(), str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_si_to_fp(&self, val: &Value, dest_ty: &Type, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildSIToFP(self.as_ll(), val.as_ll(), dest_ty.as_ll(), str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_fp_trunc(&self, val: &Value, dest_ty: &Type, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildFPTrunc(self.as_ll(), val.as_ll(), dest_ty.as_ll(), str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_fp_ext(&self, val: &Value, dest_ty: &Type, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildFPExt(self.as_ll(), val.as_ll(), dest_ty.as_ll(), str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_ptr_to_int(&self, val: &Value, dest_ty: &Type, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildPtrToInt(self.as_ll(), val.as_ll(), dest_ty.as_ll(), str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_int_to_ptr(&self, val: &Value, dest_ty: &Type, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildIntToPtr(self.as_ll(), val.as_ll(), dest_ty.as_ll(), str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_bit_cast(&self, val: &Value, dest_ty: &Type, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildBitCast(self.as_ll(), val.as_ll(), dest_ty.as_ll(), str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_z_ext_or_bit_cast(&self, val: &Value, dest_ty: &Type, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildZExtOrBitCast(self.as_ll(), val.as_ll(), dest_ty.as_ll(), str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_s_ext_or_bit_cast(&self, val: &Value, dest_ty: &Type, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildSExtOrBitCast(self.as_ll(), val.as_ll(), dest_ty.as_ll(), str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_trunc_or_bit_cast(&self, val: &Value, dest_ty: &Type, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildTruncOrBitCast(self.as_ll(), val.as_ll(), dest_ty.as_ll(), str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_cast(&self, op: Opcode, val: &Value, dest_ty: &Type, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildCast(self.as_ll(), op, val.as_ll(), dest_ty.as_ll(), str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_pointer_cast(&self, val: &Value, dest_ty: &Type, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildPointerCast(self.as_ll(), val.as_ll(), dest_ty.as_ll(), str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_int_cast(&self, val: &Value, dest_ty: &Type, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildIntCast(self.as_ll(), val.as_ll(), dest_ty.as_ll(), str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_fp_cast(&self, val: &Value, dest_ty: &Type, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildFPCast(self.as_ll(), val.as_ll(), dest_ty.as_ll(), str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_i_cmp(&self, op: c_uint, lhs: &Value, rhs: &Value, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildICmp(self.as_ll(), op, lhs.as_ll(), rhs.as_ll(), str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_f_cmp(&self, op: c_uint, lhs: &Value, rhs: &Value, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildFCmp(self.as_ll(), op, lhs.as_ll(), rhs.as_ll(), str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_phi(&self, ty: &Type, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildPhi(self.as_ll(), ty.as_ll(), str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_call(&self, func: &Value, args: &[&Value], name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildCall(self.as_ll(),
                                             func.as_ll(),
                                             args.as_ll(),
                                             args.len() as u32,
                                             str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_select(&self, cond: &Value, then: &Value, alt: &Value, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildSelect(self.as_ll(), cond.as_ll(), then.as_ll(), alt.as_ll(), str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_va_arg(&self, list: &Value, ty: &Type, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildVAArg(self.as_ll(), list.as_ll(), ty.as_ll(), str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_extract_element(&self, vec_val: &Value, index: &Value, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildExtractElement(self.as_ll(), vec_val.as_ll(), index.as_ll(), str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_insert_element(&self, vec_val: &Value, elt_val: &Value, index: &Value, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildInsertElement(self.as_ll(), vec_val.as_ll(), elt_val.as_ll(), index.as_ll(), str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_shuffle_vector(&self, v1: &Value, v2: &Value, mask: &Value, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildShuffleVector(self.as_ll(), v1.as_ll(), v2.as_ll(), mask.as_ll(), str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_extract_value(&self, agg_val: &Value, index: c_uint, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildExtractValue(self.as_ll(), agg_val.as_ll(), index, str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_insert_value(&self, agg_val: &Value, elt_val: &Value, index: c_uint, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildInsertValue(self.as_ll(), agg_val.as_ll(), elt_val.as_ll(), index, str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_is_null(&self, val: &Value, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildIsNull(self.as_ll(), val.as_ll(), str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_is_not_null(&self, val: &Value, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildIsNotNull(self.as_ll(), val.as_ll(), str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_ptr_diff(&self, lhs: &Value, rhs: &Value, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildPtrDiff(self.as_ll(), lhs.as_ll(), rhs.as_ll(), str_to_c_str(name).as_ptr()))
        }
    }
    pub fn build_atomic_load(&self, pointer_val: &Value, name: &str, order: AtomicOrdering, alignment: c_uint) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildAtomicLoad(self.as_ll(), pointer_val.as_ll(), str_to_c_str(name).as_ptr(), order, alignment))
        }
    }
    pub fn build_atomic_store(&self, val: &Value, ptr: &Value, order: AtomicOrdering, alignment: c_uint) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildAtomicStore(self.as_ll(), val.as_ll(), ptr.as_ll(), order, alignment))
        }
    }
    pub fn build_atomic_cmp_xchg(&self, lhs: &Value, cmp: &Value, rhs: &Value, order: AtomicOrdering, failure_order: AtomicOrdering) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildAtomicCmpXchg(self.as_ll(), lhs.as_ll(), cmp.as_ll(), rhs.as_ll(), order, failure_order))
        }
    }
    pub fn build_atomic_rmw(&self, op: AtomicBinOp, lhs: &Value, rhs: &Value, order: AtomicOrdering, single_threaded: bool) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBuildAtomicRMW(self.as_ll(), op, lhs.as_ll(), rhs.as_ll(), order, to_c_bool(single_threaded)))
        }
    }
    pub fn build_atomic_fence(&self, order: AtomicOrdering) {
        unsafe {
            ll::LLVMBuildAtomicFence(self.as_ll(), order)
        }
    }
}
llref_ty!(DiagnosticInfo, ll::DiagnosticInfoRef);
impl DiagnosticInfo {
    pub fn unpack_optimization_diagnostic(&self,
                                          pass_name_out: *mut *const c_char,
                                          function_out: *mut ll::ValueRef,
                                          debugloc_out: *mut ll::DebugLocRef,
                                          message_out: *mut ll::TwineRef) {
        // TODO(michael): I'm not sure what this function does, so I'm not sure how to implement it
        unimplemented!()
    }
    pub fn write_diagnostic_info_to_string(&self, s: &RustString) {
        unsafe {
            ll::LLVMWriteDiagnosticInfoToString(self.as_ll(), s.as_ll())
        }
    }
    pub fn get_diag_info_severity(&self) -> DiagnosticSeverity {
        unsafe {
            ll::LLVMGetDiagInfoSeverity(self.as_ll())
        }
    }
    pub fn get_diag_info_kind(&self) -> DiagnosticKind {
        unsafe {
            ll::LLVMGetDiagInfoKind(self.as_ll())
        }
    }
}
llref_ownable_ty!(DIBuilder, DIBuilderRef, ll::LLVMDIBuilderDispose);
impl DIBuilder {
    pub fn di_builder_finalize(&self) {
        unsafe {
            ll::LLVMDIBuilderFinalize(self.as_ll())
        }
    }
    pub fn di_builder_create_compile_unit(&self,
                                          lang: c_uint,
                                          file: &str,
                                          dir: &str,
                                          producer: &str,
                                          is_optimized: bool,
                                          flags: &str,
                                          runtime_ver: c_uint,
                                          split_name: &str) -> DIDescriptor {
        unsafe {
            ll::LLVMDIBuilderCreateCompileUnit(self.as_ll(),
                                               lang,
                                               str_to_c_str(file).as_ptr(),
                                               str_to_c_str(dir).as_ptr(),
                                               str_to_c_str(producer).as_ptr(),
                                               is_optimized,
                                               str_to_c_str(flags).as_ptr(),
                                               runtime_ver,
                                               str_to_c_str(split_name).as_ptr())
        }
    }
    pub fn di_builder_create_file(&self, filename: &str, directory: &str) -> DIFile {
        unsafe {
            ll::LLVMDIBuilderCreateFile(self.as_ll(), str_to_c_str(filename).as_ptr(), str_to_c_str(directory).as_ptr())
        }
    }
    pub fn di_builder_create_subroutine_type(&self, file: DIFile, parameter_types: DIArray) -> DICompositeType {
        unsafe {
            ll::LLVMDIBuilderCreateSubroutineType(self.as_ll(), file, parameter_types)
        }
    }
    pub fn di_builder_create_function(&self,
                                      scope: DIDescriptor,
                                      name: &str,
                                      linkage_name: &str,
                                      file: DIFile,
                                      line_no: c_uint,
                                      ty: DIType,
                                      is_local_to_unit: bool,
                                      is_definition: bool,
                                      scope_line: c_uint,
                                      flags: c_uint,
                                      is_optimized: bool,
                                      func: &Value,
                                      t_param: &Value,
                                      decl: &Value) -> DISubprogram {
        unsafe {
            ll::LLVMDIBuilderCreateFunction(self.as_ll(),
                                        scope,
                                        str_to_c_str(name).as_ptr(),
                                        str_to_c_str(linkage_name).as_ptr(),
                                        file,
                                        line_no,
                                        ty,
                                        is_local_to_unit,
                                        is_definition,
                                        scope_line,
                                        flags,
                                        is_optimized,
                                        func.as_ll(),
                                        t_param.as_ll(),
                                        decl.as_ll())
        }
    }
    pub fn di_builder_create_basic_type(&self, name: &str, size_in_bits: c_ulonglong, align_in_bits: c_ulonglong, encoding: c_uint) -> DIBasicType {
        unsafe {
            ll::LLVMDIBuilderCreateBasicType(self.as_ll(), str_to_c_str(name).as_ptr(), size_in_bits, align_in_bits, encoding)
        }
    }
    pub fn di_builder_create_pointer_type(&self, pointee_ty: DIType, size_in_bits: c_ulonglong, align_in_bits: c_ulonglong, name: &str) -> DIDerivedType {
        unsafe {
            ll::LLVMDIBuilderCreatePointerType(self.as_ll(), pointee_ty, size_in_bits, align_in_bits, str_to_c_str(name).as_ptr())
        }
    }
    pub fn di_builder_create_struct_type(&self, scope: DIDescriptor, name: &str, file: DIFile, line_number: c_uint, size_in_bits: c_ulonglong, align_in_bits: c_ulonglong, flags: c_uint, derived_from: DIType, elements: DIArray, run_time_lang: c_uint, v_table_holder: &Value, unique_id: &str) -> DICompositeType {
        unsafe {
            ll::LLVMDIBuilderCreateStructType(self.as_ll(), scope, str_to_c_str(name).as_ptr(), file, line_number, size_in_bits, align_in_bits, flags, derived_from, elements, run_time_lang, v_table_holder.as_ll(), str_to_c_str(unique_id).as_ptr())
        }
    }
    pub fn di_builder_create_member_type(&self, scope: DIDescriptor, name: &str, file: DIFile, line_no: c_uint, size_in_bits: c_ulonglong, align_in_bits: c_ulonglong, offset_in_bits: c_ulonglong, flags: c_uint, ty: DIType) -> DIDerivedType {
        unsafe {
            ll::LLVMDIBuilderCreateMemberType(self.as_ll(), scope, str_to_c_str(name).as_ptr(), file, line_no, size_in_bits, align_in_bits, offset_in_bits, flags, ty)
        }
    }
    pub fn di_builder_create_lexical_block(&self, scope: DIDescriptor, file: DIFile, line: c_uint, col: c_uint) -> DILexicalBlock {
        unsafe {
            ll::LLVMDIBuilderCreateLexicalBlock(self.as_ll(), scope, file, line, col)
        }
    }
    pub fn di_builder_create_static_variable(&self, context: DIDescriptor, name: &str, linkage_name: &str, file: DIFile, line_no: c_uint, ty: DIType, is_local_to_unit: bool, val: &Value, decl: &Value) -> DIGlobalVariable {
        unsafe {
            ll::LLVMDIBuilderCreateStaticVariable(self.as_ll(), context, str_to_c_str(name).as_ptr(), str_to_c_str(linkage_name).as_ptr(), file, line_no, ty, is_local_to_unit, val.as_ll(), decl.as_ll())
        }
    }
    pub fn di_builder_create_local_variable(&self, tag: c_uint, scope: DIDescriptor, name: &str, file: DIFile, line_no: c_uint, ty: DIType, always_preserve: bool, flags: c_uint, arg_no: c_uint) -> DIVariable {
        unsafe {
            ll::LLVMDIBuilderCreateLocalVariable(self.as_ll(), tag, scope, str_to_c_str(name).as_ptr(), file, line_no, ty, always_preserve, flags, arg_no)
        }
    }
    pub fn di_builder_create_array_type(&self, size: c_ulonglong, align_in_bits: c_ulonglong, ty: DIType, subscripts: DIArray) -> DIType {
        unsafe {
            ll::LLVMDIBuilderCreateArrayType(self.as_ll(), size, align_in_bits, ty, subscripts)
        }
    }
    pub fn di_builder_create_vector_type(&self, size: c_ulonglong, align_in_bits: c_ulonglong, ty: DIType, subscripts: DIArray) -> DIType {
        unsafe {
            ll::LLVMDIBuilderCreateVectorType(self.as_ll(), size, align_in_bits, ty, subscripts)
        }
    }
    pub fn di_builder_get_or_create_subrange(&self, lo: c_longlong, count: c_longlong) -> DISubrange {
        unsafe {
            ll::LLVMDIBuilderGetOrCreateSubrange(self.as_ll(), lo, count)
        }
    }
    pub fn di_builder_get_or_create_array(&self, ptr: *const DIDescriptor, count: c_uint) -> DIArray {
        unsafe {
            ll::LLVMDIBuilderGetOrCreateArray(self.as_ll(), ptr, count)
        }
    }
    pub fn di_builder_insert_declare_at_end(&self, val: &Value, var_info: DIVariable, insert_at_end: &BasicBlock) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMDIBuilderInsertDeclareAtEnd(self.as_ll(), val.as_ll(), var_info, insert_at_end.as_ll()))
        }
    }
    pub fn di_builder_insert_declare_before(&self, val: &Value, var_info: DIVariable, insert_before: &Value) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMDIBuilderInsertDeclareBefore(self.as_ll(), val.as_ll(), var_info, insert_before.as_ll()))
        }
    }
    pub fn di_builder_create_enumerator(&self, name: &str, val: c_ulonglong) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMDIBuilderCreateEnumerator(self.as_ll(), str_to_c_str(name).as_ptr(), val))
        }
    }
    pub fn di_builder_create_enumeration_type(&self, scope: &Value, name: &str, file: &Value, line_number: c_uint, size_in_bits: c_ulonglong, align_in_bits: c_ulonglong, elements: &Value, class_type: &Value) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMDIBuilderCreateEnumerationType(self.as_ll(), scope.as_ll(), str_to_c_str(name).as_ptr(), file.as_ll(), line_number, size_in_bits, align_in_bits, elements.as_ll(), class_type.as_ll()))
        }
    }
    pub fn di_builder_create_union_type(&self, scope: &Value, name: &str, file: &Value, line_number: c_uint, size_in_bits: c_ulonglong, align_in_bits: c_ulonglong, flags: c_uint, elements: &Value, run_time_lang: c_uint, unique_id: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMDIBuilderCreateUnionType(self.as_ll(), scope.as_ll(), str_to_c_str(name).as_ptr(), file.as_ll(), line_number, size_in_bits, align_in_bits, flags, elements.as_ll(), run_time_lang, str_to_c_str(unique_id).as_ptr()))
        }
    }
    pub fn di_builder_create_template_type_parameter(&self, scope: &Value, name: &str, ty: &Value, file: &Value, line_no: c_uint, column_no: c_uint) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMDIBuilderCreateTemplateTypeParameter(self.as_ll(), scope.as_ll(), str_to_c_str(name).as_ptr(), ty.as_ll(), file.as_ll(), line_no, column_no))
        }
    }
    pub fn di_builder_create_complex_variable(&self,
                                              tag: c_uint,
                                              scope: &Value,
                                              name: &str,
                                              file: &Value,
                                              line_no: c_uint,
                                              ty: &Value,
                                              addr_ops: &[&Value],
                                              arg_no: c_uint) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMDIBuilderCreateComplexVariable(self.as_ll(),
                                                                  tag,
                                                                  scope.as_ll(),
                                                                  str_to_c_str(name).as_ptr(),
                                                                  file.as_ll(),
                                                                  line_no,
                                                                  ty.as_ll(),
                                                                  addr_ops.as_ll(),
                                                                  addr_ops.len() as u32,
                                                                  arg_no))
        }
    }
    pub fn di_builder_create_name_space(&self, scope: &Value, name: &str, file: &Value, line_no: c_uint) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMDIBuilderCreateNameSpace(self.as_ll(), scope.as_ll(), str_to_c_str(name).as_ptr(), file.as_ll(), line_no))
        }
    }
}
llref_ownable_ty!(Context, ll::ContextRef, ll::LLVMContextDispose);
impl Context {
    pub fn get_md_kind_id_in_context(&self, name: &str, s_len: c_uint) -> c_uint {
        unsafe {
            ll::LLVMGetMDKindIDInContext(self.as_ll(), str_to_c_str(name).as_ptr(), s_len)
        }
    }
    pub fn int1_type_in_context(&self) -> Option<&Type> {
        unsafe {
            Type::from_ll(ll::LLVMInt1TypeInContext(self.as_ll()))
        }
    }
    pub fn int8_type_in_context(&self) -> Option<&Type> {
        unsafe {
            Type::from_ll(ll::LLVMInt8TypeInContext(self.as_ll()))
        }
    }
    pub fn int16_type_in_context(&self) -> Option<&Type> {
        unsafe {
            Type::from_ll(ll::LLVMInt16TypeInContext(self.as_ll()))
        }
    }
    pub fn int32_type_in_context(&self) -> Option<&Type> {
        unsafe {
            Type::from_ll(ll::LLVMInt32TypeInContext(self.as_ll()))
        }
    }
    pub fn int64_type_in_context(&self) -> Option<&Type> {
        unsafe {
            Type::from_ll(ll::LLVMInt64TypeInContext(self.as_ll()))
        }
    }
    pub fn int_type_in_context(&self, num_bits: c_uint) -> Option<&Type> {
        unsafe {
            Type::from_ll(ll::LLVMIntTypeInContext(self.as_ll(), num_bits))
        }
    }
    pub fn float_type_in_context(&self) -> Option<&Type> {
        unsafe {
            Type::from_ll(ll::LLVMFloatTypeInContext(self.as_ll()))
        }
    }
    pub fn double_type_in_context(&self) -> Option<&Type> {
        unsafe {
            Type::from_ll(ll::LLVMDoubleTypeInContext(self.as_ll()))
        }
    }
    pub fn x86_fp80_type_in_context(&self) -> Option<&Type> {
        unsafe {
            Type::from_ll(ll::LLVMX86FP80TypeInContext(self.as_ll()))
        }
    }
    pub fn fp128_type_in_context(&self) -> Option<&Type> {
        unsafe {
            Type::from_ll(ll::LLVMFP128TypeInContext(self.as_ll()))
        }
    }
    pub fn ppcfp128_type_in_context(&self) -> Option<&Type> {
        unsafe {
            Type::from_ll(ll::LLVMPPCFP128TypeInContext(self.as_ll()))
        }
    }
    pub fn struct_type_in_context(&self,
                                  element_types: &[&Type],
                                  packed: bool) -> Option<&Type> {
        unsafe {
            Type::from_ll(ll::LLVMStructTypeInContext(self.as_ll(),
                                                      element_types.as_ll(),
                                                      element_types.len() as u32,
                                                      to_c_bool(packed)))
        }
    }
    pub fn void_type_in_context(&self) -> Option<&Type> {
        unsafe {
            Type::from_ll(ll::LLVMVoidTypeInContext(self.as_ll()))
        }
    }
    pub fn label_type_in_context(&self) -> Option<&Type> {
        unsafe {
            Type::from_ll(ll::LLVMLabelTypeInContext(self.as_ll()))
        }
    }
    pub fn metadata_type_in_context(&self) -> Option<&Type> {
        unsafe {
            Type::from_ll(ll::LLVMMetadataTypeInContext(self.as_ll()))
        }
    }
    pub fn md_string_in_context(&self, s: &str) -> Option<&Value> {
        unsafe {
            // TODO(michael): What's the correct way to do strings in this situation
            Value::from_ll(ll::LLVMMDStringInContext(self.as_ll(),
                                                     s.as_ptr() as *const i8,
                                                     s.len() as u32))
        }
    }
    pub fn md_node_in_context(&self, vals: &[&Value]) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMMDNodeInContext(self.as_ll(),
                                                   vals.as_ll(),
                                                   vals.len() as u32))
        }
    }
    pub fn const_string_in_context(&self, s: &str, dont_null_terminate: bool) -> Option<&Value> {
        unsafe {
            // TODO(michael): What's the correct way to do strings in this situation
            Value::from_ll(ll::LLVMConstStringInContext(self.as_ll(),
                                                        s.as_ptr() as *const i8,
                                                        s.len() as u32,
                                                        to_c_bool(dont_null_terminate)))
        }
    }
    pub fn const_struct_in_context(&self, constant_vals: &[&Value], packed: bool) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMConstStructInContext(self.as_ll(),
                                                        constant_vals.as_ll(),
                                                        constant_vals.len() as u32,
                                                        to_c_bool(packed)))
        }
    }
    pub fn append_basic_block_in_context(&self, func: &Value, name: &str) -> Option<&BasicBlock> {
        unsafe {
            BasicBlock::from_ll(ll::LLVMAppendBasicBlockInContext(self.as_ll(), func.as_ll(), str_to_c_str(name).as_ptr()))
        }
    }
    pub fn insert_basic_block_in_context(&self, bb: &BasicBlock, name: &str) -> Option<&BasicBlock> {
        unsafe {
            BasicBlock::from_ll(ll::LLVMInsertBasicBlockInContext(self.as_ll(), bb.as_ll(), str_to_c_str(name).as_ptr()))
        }
    }
    pub fn create_builder_in_context(&self) -> Box<Builder> {
        unsafe {
            Builder::box_from_ll(ll::LLVMCreateBuilderInContext(self.as_ll()))
        }
    }
    pub fn struct_create_named(&self, name: &str) -> Option<&Type> {
        unsafe {
            Type::from_ll(ll::LLVMStructCreateNamed(self.as_ll(), str_to_c_str(name).as_ptr()))
        }
    }
    pub fn context_set_diagnostic_handler(&self, handler: DiagnosticHandler, diagnostic_context: *mut c_void) {
        unsafe {
            ll::LLVMContextSetDiagnosticHandler(self.as_ll(), handler, diagnostic_context)
        }
    }
    pub fn write_debug_loc_to_string(&self, dl: &DebugLoc, s: &RustString) {
        unsafe {
            ll::LLVMWriteDebugLocToString(self.as_ll(), dl.as_ll(), s.as_ll())
        }
    }
    pub fn set_inline_asm_diagnostic_handler(&self, h: InlineAsmDiagHandler, cx: *mut c_void) {
        unsafe {
            ll::LLVMSetInlineAsmDiagnosticHandler(self.as_ll(), h, cx)
        }
    }
}
llref_ty!(Twine, ll::TwineRef);
impl Twine {
    pub fn write_twine_to_string(&self, s: &RustString) {
        unsafe {
            ll::LLVMWriteTwineToString(self.as_ll(), s.as_ll())
        }
    }
}
llref_ownable_ty!(TargetMachine, ll::TargetMachineRef, ll::LLVMRustDisposeTargetMachine);
impl TargetMachine {
    pub fn rust_add_analysis_passes(&self, pm: &PassManager, m: &Module) {
        unsafe {
            ll::LLVMRustAddAnalysisPasses(self.as_ll(), pm.as_ll(), m.as_ll())
        }
    }
    pub fn rust_write_output_file(&self, pm: &PassManager, m: &Module, output: &str, file_type: FileType) -> bool {
        unsafe {
            ll::LLVMRustWriteOutputFile(self.as_ll(), pm.as_ll(), m.as_ll(), str_to_c_str(output).as_ptr(), file_type)
        }
    }
}
llref_ty!(BasicBlock, ll::BasicBlockRef);
impl BasicBlock {
    pub fn basic_block_as_value(&self) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBasicBlockAsValue(self.as_ll()))
        }
    }
    pub fn get_basic_block_parent(&self) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMGetBasicBlockParent(self.as_ll()))
        }
    }
    pub fn get_next_basic_block(&self) -> Option<&BasicBlock> {
        unsafe {
            BasicBlock::from_ll(ll::LLVMGetNextBasicBlock(self.as_ll()))
        }
    }
    pub fn get_previous_basic_block(&self) -> Option<&BasicBlock> {
        unsafe {
            BasicBlock::from_ll(ll::LLVMGetPreviousBasicBlock(self.as_ll()))
        }
    }
    // TODO(michael): Is this a dispose method? How should I deal with it...
    pub fn delete_basic_block(&self) {
        unsafe {
            ll::LLVMDeleteBasicBlock(self.as_ll())
        }
    }
    pub fn move_basic_block_after(&self, move_after: &BasicBlock) {
        unsafe {
            ll::LLVMMoveBasicBlockAfter(self.as_ll(), move_after.as_ll())
        }
    }
    pub fn move_basic_block_before(&self, move_before: &BasicBlock) {
        unsafe {
            ll::LLVMMoveBasicBlockBefore(self.as_ll(), move_before.as_ll())
        }
    }
    pub fn get_first_instruction(&self) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMGetFirstInstruction(self.as_ll()))
        }
    }
    pub fn get_last_instruction(&self) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMGetLastInstruction(self.as_ll()))
        }
    }
}
llref_ty!(Value, ll::ValueRef);
impl Value {
    pub fn type_of(&self) -> Option<&Type> {
        unsafe {
            Type::from_ll(ll::LLVMTypeOf(self.as_ll()))
        }
    }
    pub fn get_value_name(&self) -> String {
        unsafe {
            c_str_to_string(ll::LLVMGetValueName(self.as_ll()))
        }
    }
    pub fn set_value_name(&self, name: &str) {
        unsafe {
            ll::LLVMSetValueName(self.as_ll(), str_to_c_str(name).as_ptr())
        }
    }
    pub fn dump_value(&self) {
        unsafe {
            ll::LLVMDumpValue(self.as_ll())
        }
    }
    pub fn replace_all_uses_with(&self, new_val: &Value) {
        unsafe {
            ll::LLVMReplaceAllUsesWith(self.as_ll(), new_val.as_ll())
        }
    }
    pub fn has_metadata(&self) -> c_int {
        unsafe {
            ll::LLVMHasMetadata(self.as_ll())
        }
    }
    pub fn get_metadata(&self, kind_id: c_uint) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMGetMetadata(self.as_ll(), kind_id))
        }
    }
    pub fn set_metadata(&self, kind_id: c_uint, node: &Value) {
        unsafe {
            ll::LLVMSetMetadata(self.as_ll(), kind_id, node.as_ll())
        }
    }
    pub fn get_first_use(&self) -> Option<&Use> {
        unsafe {
            Use::from_ll(ll::LLVMGetFirstUse(self.as_ll()))
        }
    }
    pub fn get_num_operands(&self) -> c_int {
        unsafe {
            ll::LLVMGetNumOperands(self.as_ll())
        }
    }
    pub fn get_operand(&self, index: c_uint) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMGetOperand(self.as_ll(), index))
        }
    }
    pub fn set_operand(&self, index: c_uint, op: &Value) {
        unsafe {
            ll::LLVMSetOperand(self.as_ll(), index, op.as_ll())
        }
    }
    pub fn is_constant(&self) -> bool {
        unsafe {
            from_c_bool(ll::LLVMIsConstant(self.as_ll()))
        }
    }
    pub fn is_null(&self) -> bool {
        unsafe {
            from_c_bool(ll::LLVMIsNull(self.as_ll()))
        }
    }
    pub fn is_undef(&self) -> bool {
        unsafe {
            from_c_bool(ll::LLVMIsUndef(self.as_ll()))
        }
    }
    pub fn const_int_get_z_ext_value(&self) -> c_ulonglong {
        unsafe {
            ll::LLVMConstIntGetZExtValue(self.as_ll())
        }
    }
    pub fn const_int_get_s_ext_value(&self) -> c_longlong {
        unsafe {
            ll::LLVMConstIntGetSExtValue(self.as_ll())
        }
    }
    pub fn const_neg(&self) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMConstNeg(self.as_ll()))
        }
    }
    pub fn const_nsw_neg(&self) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMConstNSWNeg(self.as_ll()))
        }
    }
    pub fn const_nuw_neg(&self) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMConstNUWNeg(self.as_ll()))
        }
    }
    pub fn const_f_neg(&self) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMConstFNeg(self.as_ll()))
        }
    }
    pub fn const_not(&self) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMConstNot(self.as_ll()))
        }
    }
    pub fn const_add(&self, rhs_constant: &Value) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMConstAdd(self.as_ll(), rhs_constant.as_ll()))
        }
    }
    pub fn const_nsw_add(&self, rhs_constant: &Value) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMConstNSWAdd(self.as_ll(), rhs_constant.as_ll()))
        }
    }
    pub fn const_nuw_add(&self, rhs_constant: &Value) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMConstNUWAdd(self.as_ll(), rhs_constant.as_ll()))
        }
    }
    pub fn const_f_add(&self, rhs_constant: &Value) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMConstFAdd(self.as_ll(), rhs_constant.as_ll()))
        }
    }
    pub fn const_sub(&self, rhs_constant: &Value) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMConstSub(self.as_ll(), rhs_constant.as_ll()))
        }
    }
    pub fn const_nsw_sub(&self, rhs_constant: &Value) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMConstNSWSub(self.as_ll(), rhs_constant.as_ll()))
        }
    }
    pub fn const_nuw_sub(&self, rhs_constant: &Value) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMConstNUWSub(self.as_ll(), rhs_constant.as_ll()))
        }
    }
    pub fn const_f_sub(&self, rhs_constant: &Value) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMConstFSub(self.as_ll(), rhs_constant.as_ll()))
        }
    }
    pub fn const_mul(&self, rhs_constant: &Value) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMConstMul(self.as_ll(), rhs_constant.as_ll()))
        }
    }
    pub fn const_nsw_mul(&self, rhs_constant: &Value) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMConstNSWMul(self.as_ll(), rhs_constant.as_ll()))
        }
    }
    pub fn const_nuw_mul(&self, rhs_constant: &Value) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMConstNUWMul(self.as_ll(), rhs_constant.as_ll()))
        }
    }
    pub fn const_f_mul(&self, rhs_constant: &Value) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMConstFMul(self.as_ll(), rhs_constant.as_ll()))
        }
    }
    pub fn const_u_div(&self, rhs_constant: &Value) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMConstUDiv(self.as_ll(), rhs_constant.as_ll()))
        }
    }
    pub fn const_s_div(&self, rhs_constant: &Value) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMConstSDiv(self.as_ll(), rhs_constant.as_ll()))
        }
    }
    pub fn const_exact_s_div(&self, rhs_constant: &Value) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMConstExactSDiv(self.as_ll(), rhs_constant.as_ll()))
        }
    }
    pub fn const_f_div(&self, rhs_constant: &Value) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMConstFDiv(self.as_ll(), rhs_constant.as_ll()))
        }
    }
    pub fn const_u_rem(&self, rhs_constant: &Value) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMConstURem(self.as_ll(), rhs_constant.as_ll()))
        }
    }
    pub fn const_s_rem(&self, rhs_constant: &Value) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMConstSRem(self.as_ll(), rhs_constant.as_ll()))
        }
    }
    pub fn const_f_rem(&self, rhs_constant: &Value) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMConstFRem(self.as_ll(), rhs_constant.as_ll()))
        }
    }
    pub fn const_and(&self, rhs_constant: &Value) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMConstAnd(self.as_ll(), rhs_constant.as_ll()))
        }
    }
    pub fn const_or(&self, rhs_constant: &Value) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMConstOr(self.as_ll(), rhs_constant.as_ll()))
        }
    }
    pub fn const_xor(&self, rhs_constant: &Value) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMConstXor(self.as_ll(), rhs_constant.as_ll()))
        }
    }
    pub fn const_shl(&self, rhs_constant: &Value) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMConstShl(self.as_ll(), rhs_constant.as_ll()))
        }
    }
    pub fn const_l_shr(&self, rhs_constant: &Value) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMConstLShr(self.as_ll(), rhs_constant.as_ll()))
        }
    }
    pub fn const_a_shr(&self, rhs_constant: &Value) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMConstAShr(self.as_ll(), rhs_constant.as_ll()))
        }
    }
    pub fn const_gep(&self, constant_indices: &[&Value]) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMConstGEP(self.as_ll(),
                                            constant_indices.as_ll(),
                                            constant_indices.len() as u32))
        }
    }
    pub fn const_in_bounds_gep(&self, constant_indices: &[&Value]) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMConstInBoundsGEP(self.as_ll(),
                                                    constant_indices.as_ll(),
                                                    constant_indices.len() as u32))
        }
    }
    pub fn const_trunc(&self, to_type: &Type) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMConstTrunc(self.as_ll(), to_type.as_ll()))
        }
    }
    pub fn const_s_ext(&self, to_type: &Type) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMConstSExt(self.as_ll(), to_type.as_ll()))
        }
    }
    pub fn const_z_ext(&self, to_type: &Type) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMConstZExt(self.as_ll(), to_type.as_ll()))
        }
    }
    pub fn const_fp_trunc(&self, to_type: &Type) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMConstFPTrunc(self.as_ll(), to_type.as_ll()))
        }
    }
    pub fn const_fp_ext(&self, to_type: &Type) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMConstFPExt(self.as_ll(), to_type.as_ll()))
        }
    }
    pub fn const_ui_to_fp(&self, to_type: &Type) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMConstUIToFP(self.as_ll(), to_type.as_ll()))
        }
    }
    pub fn const_si_to_fp(&self, to_type: &Type) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMConstSIToFP(self.as_ll(), to_type.as_ll()))
        }
    }
    pub fn const_fp_to_ui(&self, to_type: &Type) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMConstFPToUI(self.as_ll(), to_type.as_ll()))
        }
    }
    pub fn const_fp_to_si(&self, to_type: &Type) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMConstFPToSI(self.as_ll(), to_type.as_ll()))
        }
    }
    pub fn const_ptr_to_int(&self, to_type: &Type) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMConstPtrToInt(self.as_ll(), to_type.as_ll()))
        }
    }
    pub fn const_int_to_ptr(&self, to_type: &Type) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMConstIntToPtr(self.as_ll(), to_type.as_ll()))
        }
    }
    pub fn const_bit_cast(&self, to_type: &Type) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMConstBitCast(self.as_ll(), to_type.as_ll()))
        }
    }
    pub fn const_z_ext_or_bit_cast(&self, to_type: &Type) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMConstZExtOrBitCast(self.as_ll(), to_type.as_ll()))
        }
    }
    pub fn const_s_ext_or_bit_cast(&self, to_type: &Type) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMConstSExtOrBitCast(self.as_ll(), to_type.as_ll()))
        }
    }
    pub fn const_trunc_or_bit_cast(&self, to_type: &Type) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMConstTruncOrBitCast(self.as_ll(), to_type.as_ll()))
        }
    }
    pub fn const_pointer_cast(&self, to_type: &Type) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMConstPointerCast(self.as_ll(), to_type.as_ll()))
        }
    }
    pub fn const_int_cast(&self, to_type: &Type, is_signed: bool) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMConstIntCast(self.as_ll(), to_type.as_ll(), to_c_bool(is_signed)))
        }
    }
    pub fn const_fp_cast(&self, to_type: &Type) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMConstFPCast(self.as_ll(), to_type.as_ll()))
        }
    }
    pub fn const_select(&self, constant_if_true: &Value, constant_if_false: &Value) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMConstSelect(self.as_ll(), constant_if_true.as_ll(), constant_if_false.as_ll()))
        }
    }
    pub fn const_extract_element(&self, index_constant: &Value) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMConstExtractElement(self.as_ll(), index_constant.as_ll()))
        }
    }
    pub fn const_insert_element(&self, element_value_constant: &Value, index_constant: &Value) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMConstInsertElement(self.as_ll(), element_value_constant.as_ll(), index_constant.as_ll()))
        }
    }
    pub fn const_shuffle_vector(&self, vector_b_constant: &Value, mask_constant: &Value) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMConstShuffleVector(self.as_ll(), vector_b_constant.as_ll(), mask_constant.as_ll()))
        }
    }
    pub fn const_extract_value(&self, idx_list: *const c_uint, num_idx: c_uint) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMConstExtractValue(self.as_ll(), idx_list, num_idx))
        }
    }
    pub fn const_insert_value(&self, element_value_constant: &Value, idx_list: *const c_uint, num_idx: c_uint) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMConstInsertValue(self.as_ll(), element_value_constant.as_ll(), idx_list, num_idx))
        }
    }
    pub fn block_address(&self, bb: &BasicBlock) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMBlockAddress(self.as_ll(), bb.as_ll()))
        }
    }
    pub fn get_global_parent(&self) -> Option<&Module> {
        unsafe {
            Module::from_ll(ll::LLVMGetGlobalParent(self.as_ll()))
        }
    }
    pub fn is_declaration(&self) -> bool {
        unsafe {
            from_c_bool(ll::LLVMIsDeclaration(self.as_ll()))
        }
    }
    pub fn get_linkage(&self) -> c_uint {
        unsafe {
            ll::LLVMGetLinkage(self.as_ll())
        }
    }
    pub fn set_linkage(&self, link: c_uint) {
        unsafe {
            ll::LLVMSetLinkage(self.as_ll(), link)
        }
    }
    pub fn get_section(&self) -> String {
        unsafe {
            c_str_to_string(ll::LLVMGetSection(self.as_ll()))
        }
    }
    pub fn set_section(&self, section: &str) {
        unsafe {
            ll::LLVMSetSection(self.as_ll(), str_to_c_str(section).as_ptr())
        }
    }
    pub fn get_visibility(&self) -> c_uint {
        unsafe {
            ll::LLVMGetVisibility(self.as_ll())
        }
    }
    pub fn set_visibility(&self, viz: c_uint) {
        unsafe {
            ll::LLVMSetVisibility(self.as_ll(), viz)
        }
    }
    pub fn get_alignment(&self) -> c_uint {
        unsafe {
            ll::LLVMGetAlignment(self.as_ll())
        }
    }
    pub fn set_alignment(&self, bytes: c_uint) {
        unsafe {
            ll::LLVMSetAlignment(self.as_ll(), bytes)
        }
    }
    pub fn get_next_global(&self) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMGetNextGlobal(self.as_ll()))
        }
    }
    pub fn get_previous_global(&self) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMGetPreviousGlobal(self.as_ll()))
        }
    }
    pub fn delete_global(&self) {
        unsafe {
            ll::LLVMDeleteGlobal(self.as_ll())
        }
    }
    pub fn get_initializer(&self) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMGetInitializer(self.as_ll()))
        }
    }
    pub fn set_initializer(&self, constant_val: &Value) {
        unsafe {
            ll::LLVMSetInitializer(self.as_ll(), constant_val.as_ll())
        }
    }
    pub fn is_thread_local(&self) -> bool {
        unsafe {
            from_c_bool(ll::LLVMIsThreadLocal(self.as_ll()))
        }
    }
    pub fn set_thread_local(&self, is_thread_local: bool) {
        unsafe {
            ll::LLVMSetThreadLocal(self.as_ll(), to_c_bool(is_thread_local))
        }
    }
    pub fn is_global_constant(&self) -> bool {
        unsafe {
            from_c_bool(ll::LLVMIsGlobalConstant(self.as_ll()))
        }
    }
    pub fn set_global_constant(&self, is_constant: bool) {
        unsafe {
            ll::LLVMSetGlobalConstant(self.as_ll(), to_c_bool(is_constant))
        }
    }
    pub fn get_next_function(&self) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMGetNextFunction(self.as_ll()))
        }
    }
    pub fn get_previous_function(&self) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMGetPreviousFunction(self.as_ll()))
        }
    }
    pub fn delete_function(&self) {
        unsafe {
            ll::LLVMDeleteFunction(self.as_ll())
        }
    }
    pub fn get_intrinsic_id(&self) -> c_uint {
        unsafe {
            ll::LLVMGetIntrinsicID(self.as_ll())
        }
    }
    pub fn get_function_call_conv(&self) -> c_uint {
        unsafe {
            ll::LLVMGetFunctionCallConv(self.as_ll())
        }
    }
    pub fn set_function_call_conv(&self, cc: c_uint) {
        unsafe {
            ll::LLVMSetFunctionCallConv(self.as_ll(), cc)
        }
    }
    pub fn get_gc(&self) -> String {
        unsafe {
            c_str_to_string(ll::LLVMGetGC(self.as_ll()))
        }
    }
    pub fn set_gc(&self, name: &str) {
        unsafe {
            ll::LLVMSetGC(self.as_ll(), str_to_c_str(name).as_ptr())
        }
    }
    pub fn add_dereferenceable_attr(&self, index: c_uint, bytes: uint64_t) {
        unsafe {
            ll::LLVMAddDereferenceableAttr(self.as_ll(), index, bytes)
        }
    }
    pub fn add_function_attribute(&self, index: c_uint, pa: uint64_t) {
        unsafe {
            ll::LLVMAddFunctionAttribute(self.as_ll(), index, pa)
        }
    }
    pub fn add_function_attr_string(&self, index: c_uint, name: &str) {
        unsafe {
            ll::LLVMAddFunctionAttrString(self.as_ll(), index, str_to_c_str(name).as_ptr())
        }
    }
    pub fn remove_function_attr_string(&self, index: c_uint, name: &str) {
        unsafe {
            ll::LLVMRemoveFunctionAttrString(self.as_ll(), index, str_to_c_str(name).as_ptr())
        }
    }
    pub fn get_function_attr(&self) -> c_ulonglong {
        unsafe {
            ll::LLVMGetFunctionAttr(self.as_ll())
        }
    }
    pub fn count_params(&self) -> c_uint {
        unsafe {
            ll::LLVMCountParams(self.as_ll())
        }
    }
    pub fn get_params(&self) -> Vec<Option<&Value>> {
        // TODO(michael): What would the best way be to implement this
        unimplemented!()
    }
    pub fn get_param(&self, index: c_uint) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMGetParam(self.as_ll(), index))
        }
    }
    pub fn get_param_parent(&self) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMGetParamParent(self.as_ll()))
        }
    }
    pub fn get_first_param(&self) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMGetFirstParam(self.as_ll()))
        }
    }
    pub fn get_last_param(&self) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMGetLastParam(self.as_ll()))
        }
    }
    pub fn get_next_param(&self) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMGetNextParam(self.as_ll()))
        }
    }
    pub fn get_previous_param(&self) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMGetPreviousParam(self.as_ll()))
        }
    }
    pub fn add_attribute(&self, pa: c_uint) {
        unsafe {
            ll::LLVMAddAttribute(self.as_ll(), pa)
        }
    }
    pub fn remove_attribute(&self, pa: c_uint) {
        unsafe {
            ll::LLVMRemoveAttribute(self.as_ll(), pa)
        }
    }
    pub fn get_attribute(&self) -> c_uint {
        unsafe {
            ll::LLVMGetAttribute(self.as_ll())
        }
    }
    pub fn set_param_alignment(&self, align: c_uint) {
        unsafe {
            ll::LLVMSetParamAlignment(self.as_ll(), align)
        }
    }
    pub fn value_is_basic_block(&self) -> bool {
        unsafe {
            from_c_bool(ll::LLVMValueIsBasicBlock(self.as_ll()))
        }
    }
    pub fn value_as_basic_block(&self) -> Option<&BasicBlock> {
        unsafe {
            BasicBlock::from_ll(ll::LLVMValueAsBasicBlock(self.as_ll()))
        }
    }
    pub fn count_basic_blocks(&self) -> c_uint {
        unsafe {
            ll::LLVMCountBasicBlocks(self.as_ll())
        }
    }
    pub fn get_basic_blocks(&self) -> Vec<Option<&Value>> {
        // TODO(michael): What would the best way be to implement this
        unimplemented!()
    }
    pub fn get_first_basic_block(&self) -> Option<&BasicBlock> {
        unsafe {
            BasicBlock::from_ll(ll::LLVMGetFirstBasicBlock(self.as_ll()))
        }
    }
    pub fn get_last_basic_block(&self) -> Option<&BasicBlock> {
        unsafe {
            BasicBlock::from_ll(ll::LLVMGetLastBasicBlock(self.as_ll()))
        }
    }
    pub fn get_entry_basic_block(&self) -> Option<&BasicBlock> {
        unsafe {
            BasicBlock::from_ll(ll::LLVMGetEntryBasicBlock(self.as_ll()))
        }
    }
    pub fn get_instruction_parent(&self) -> Option<&BasicBlock> {
        unsafe {
            BasicBlock::from_ll(ll::LLVMGetInstructionParent(self.as_ll()))
        }
    }
    pub fn get_next_instruction(&self) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMGetNextInstruction(self.as_ll()))
        }
    }
    pub fn get_previous_instruction(&self) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMGetPreviousInstruction(self.as_ll()))
        }
    }
    pub fn instruction_erase_from_parent(&self) {
        unsafe {
            ll::LLVMInstructionEraseFromParent(self.as_ll())
        }
    }
    pub fn set_instruction_call_conv(&self, cc: c_uint) {
        unsafe {
            ll::LLVMSetInstructionCallConv(self.as_ll(), cc)
        }
    }
    pub fn get_instruction_call_conv(&self) -> c_uint {
        unsafe {
            ll::LLVMGetInstructionCallConv(self.as_ll())
        }
    }
    pub fn add_instr_attribute(&self, index: c_uint, ia: c_uint) {
        unsafe {
            ll::LLVMAddInstrAttribute(self.as_ll(), index, ia)
        }
    }
    pub fn remove_instr_attribute(&self, index: c_uint, ia: c_uint) {
        unsafe {
            ll::LLVMRemoveInstrAttribute(self.as_ll(), index, ia)
        }
    }
    pub fn set_instr_param_alignment(&self, index: c_uint, align: c_uint) {
        unsafe {
            ll::LLVMSetInstrParamAlignment(self.as_ll(), index, align)
        }
    }
    pub fn add_call_site_attribute(&self, index: c_uint, val: uint64_t) {
        unsafe {
            ll::LLVMAddCallSiteAttribute(self.as_ll(), index, val)
        }
    }
    pub fn add_dereferenceable_call_site_attr(&self, index: c_uint, bytes: uint64_t) {
        unsafe {
            ll::LLVMAddDereferenceableCallSiteAttr(self.as_ll(), index, bytes)
        }
    }
    pub fn is_tail_call(&self) -> bool {
        unsafe {
            from_c_bool(ll::LLVMIsTailCall(self.as_ll()))
        }
    }
    pub fn set_tail_call(&self, is_tail_call: bool) {
        unsafe {
            ll::LLVMSetTailCall(self.as_ll(), to_c_bool(is_tail_call))
        }
    }
    pub fn get_volatile(&self) -> bool {
        unsafe {
            from_c_bool(ll::LLVMGetVolatile(self.as_ll()))
        }
    }
    pub fn set_volatile(&self, volatile: bool) {
        unsafe {
            ll::LLVMSetVolatile(self.as_ll(), to_c_bool(volatile))
        }
    }
    pub fn add_incoming(&self, incoming: &[(&Value, &BasicBlock)]) {
        unsafe {
            let incoming_values: Vec<_> =
                incoming.iter().map(|&(x, _)| x.as_ll()).collect();
            let incoming_blocks: Vec<_> =
                incoming.iter().map(|&(_, x)| x.as_ll()).collect();

            ll::LLVMAddIncoming(self.as_ll(),
                                incoming_values.as_ptr(),
                                incoming_blocks.as_ptr(),
                                incoming.len() as u32)
        }
    }
    pub fn count_incoming(&self) -> c_uint {
        unsafe {
            ll::LLVMCountIncoming(self.as_ll())
        }
    }
    pub fn get_incoming_value(&self, index: c_uint) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMGetIncomingValue(self.as_ll(), index))
        }
    }
    pub fn get_incoming_block(&self, index: c_uint) -> Option<&BasicBlock> {
        unsafe {
            BasicBlock::from_ll(ll::LLVMGetIncomingBlock(self.as_ll(), index))
        }
    }
    pub fn get_incoming(&self, index: c_uint) -> Option<(&Value, &BasicBlock)> {
        let value = self.get_incoming_value(index);
        let block = self.get_incoming_block(index);
        if let (Some(value), Some(block)) = (value, block) {
            Some((value, block))
        } else {
            None
        }
    }
    pub fn add_case(&self, on_val: &Value, dest: &BasicBlock) {
        unsafe {
            ll::LLVMAddCase(self.as_ll(), on_val.as_ll(), dest.as_ll())
        }
    }
    pub fn add_destination(&self, dest: &BasicBlock) {
        unsafe {
            ll::LLVMAddDestination(self.as_ll(), dest.as_ll())
        }
    }
    pub fn add_clause(&self, clause_val: &Value) {
        unsafe {
            ll::LLVMAddClause(self.as_ll(), clause_val.as_ll())
        }
    }
    pub fn set_cleanup(&self, val: bool) {
        unsafe {
            ll::LLVMSetCleanup(self.as_ll(), to_c_bool(val))
        }
    }
    pub fn is_a_terminator_inst(&self) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMIsATerminatorInst(self.as_ll()))
        }
    }
    pub fn is_a_store_inst(&self) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMIsAStoreInst(self.as_ll()))
        }
    }
    pub fn set_unnamed_addr(&self, unnamed_addr: bool) {
        unsafe {
            ll::LLVMSetUnnamedAddr(self.as_ll(), to_c_bool(unnamed_addr))
        }
    }
    pub fn di_composite_type_set_type_array(&self, type_array: &Value) {
        unsafe {
            ll::LLVMDICompositeTypeSetTypeArray(self.as_ll(), type_array.as_ll())
        }
    }
    pub fn write_value_to_string(&self, s: &RustString) {
        unsafe {
            ll::LLVMWriteValueToString(self.as_ll(), s.as_ll())
        }
    }
    pub fn is_a_argument(&self) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMIsAArgument(self.as_ll()))
        }
    }
    pub fn is_a_alloca_inst(&self) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMIsAAllocaInst(self.as_ll()))
        }
    }
    pub fn rust_set_dll_export_storage_class(&self) {
        unsafe {
            ll::LLVMRustSetDLLExportStorageClass(self.as_ll())
        }
    }
}
llref_ty!(Archive, ll::ArchiveRef);
impl Archive {
    pub fn rust_archive_read_section(&self, name: &str, out_len: *mut size_t) -> String {
        unsafe {
            c_str_to_string(ll::LLVMRustArchiveReadSection(self.as_ll(), str_to_c_str(name).as_ptr(), out_len))
        }
    }
    pub fn rust_destroy_archive(&self) {
        unsafe {
            ll::LLVMRustDestroyArchive(self.as_ll())
        }
    }
}
llref_ownable_ty!(SectionIterator, ll::SectionIteratorRef, ll::LLVMDisposeSectionIterator);
impl SectionIterator {
    pub fn move_to_next_section(&self) {
        unsafe {
            ll::LLVMMoveToNextSection(self.as_ll())
        }
    }
    pub fn get_section_size(&self) -> c_ulonglong {
        unsafe {
            ll::LLVMGetSectionSize(self.as_ll())
        }
    }
    pub fn get_section_contents(&self) -> String {
        unsafe {
            c_str_to_string(ll::LLVMGetSectionContents(self.as_ll()))
        }
    }
    pub fn rust_get_section_name(&self, data: *mut *const c_char) -> c_int {
        unsafe {
            ll::LLVMRustGetSectionName(self.as_ll(), data)
        }
    }
}
llref_ownable_ty!(PassManagerBuilder, ll::PassManagerBuilderRef, ll::LLVMPassManagerBuilderDispose);
impl PassManagerBuilder {
    pub fn pass_manager_builder_set_opt_level(&self, optimization_level: c_uint) {
        unsafe {
            ll::LLVMPassManagerBuilderSetOptLevel(self.as_ll(), optimization_level)
        }
    }
    pub fn pass_manager_builder_set_size_level(&self, value: bool) {
        unsafe {
            ll::LLVMPassManagerBuilderSetSizeLevel(self.as_ll(), to_c_bool(value))
        }
    }
    pub fn pass_manager_builder_set_disable_unit_at_a_time(&self, value: bool) {
        unsafe {
            ll::LLVMPassManagerBuilderSetDisableUnitAtATime(self.as_ll(), to_c_bool(value))
        }
    }
    pub fn pass_manager_builder_set_disable_unroll_loops(&self, value: bool) {
        unsafe {
            ll::LLVMPassManagerBuilderSetDisableUnrollLoops(self.as_ll(), to_c_bool(value))
        }
    }
    pub fn pass_manager_builder_set_disable_simplify_lib_calls(&self, value: bool) {
        unsafe {
            ll::LLVMPassManagerBuilderSetDisableSimplifyLibCalls(self.as_ll(), to_c_bool(value))
        }
    }
    pub fn pass_manager_builder_use_inliner_with_threshold(&self, threshold: c_uint) {
        unsafe {
            ll::LLVMPassManagerBuilderUseInlinerWithThreshold(self.as_ll(), threshold)
        }
    }
    pub fn pass_manager_builder_populate_module_pass_manager(&self, pm: &PassManager) {
        unsafe {
            ll::LLVMPassManagerBuilderPopulateModulePassManager(self.as_ll(), pm.as_ll())
        }
    }
    pub fn pass_manager_builder_populate_function_pass_manager(&self, pm: &PassManager) {
        unsafe {
            ll::LLVMPassManagerBuilderPopulateFunctionPassManager(self.as_ll(), pm.as_ll())
        }
    }
    pub fn pass_manager_builder_populate_lto_pass_manager(&self, pm: &PassManager, internalize: bool, run_inliner: bool) {
        unsafe {
            ll::LLVMPassManagerBuilderPopulateLTOPassManager(self.as_ll(), pm.as_ll(), to_c_bool(internalize), to_c_bool(run_inliner))
        }
    }
    pub fn rust_add_builder_library_info(&self, m: &Module, disable_simplify_lib_calls: bool) {
        unsafe {
            ll::LLVMRustAddBuilderLibraryInfo(self.as_ll(), m.as_ll(), disable_simplify_lib_calls)
        }
    }
    pub fn rust_add_always_inline_pass(&self, add_lifetimes: bool) {
        unsafe {
            ll::LLVMRustAddAlwaysInlinePass(self.as_ll(), add_lifetimes)
        }
    }
}
llref_ownable_ty!(Module, ll::ModuleRef, ll::LLVMDisposeModule);
impl Module {
    pub fn get_module_context(&self) -> Option<&Context> {
        unsafe {
            Context::from_ll(ll::LLVMGetModuleContext(self.as_ll()))
        }
    }
    pub fn get_data_layout(&self) -> String {
        unsafe {
            c_str_to_string(ll::LLVMGetDataLayout(self.as_ll()))
        }
    }
    pub fn set_data_layout(&self, triple: &str) {
        unsafe {
            ll::LLVMSetDataLayout(self.as_ll(), str_to_c_str(triple).as_ptr())
        }
    }
    pub fn get_target(&self) -> String {
        unsafe {
            c_str_to_string(ll::LLVMGetTarget(self.as_ll()))
        }
    }
    pub fn set_target(&self, triple: &str) {
        unsafe {
            ll::LLVMSetTarget(self.as_ll(), str_to_c_str(triple).as_ptr())
        }
    }
    pub fn dump_module(&self) {
        unsafe {
            ll::LLVMDumpModule(self.as_ll())
        }
    }
    pub fn set_module_inline_asm(&self, asm: &str) {
        unsafe {
            ll::LLVMSetModuleInlineAsm(self.as_ll(), str_to_c_str(asm).as_ptr())
        }
    }
    pub fn add_named_metadata_operand(&self, str: &str, val: &Value) {
        unsafe {
            ll::LLVMAddNamedMetadataOperand(self.as_ll(), str_to_c_str(str).as_ptr(), val.as_ll())
        }
    }
    pub fn add_global(&self, ty: &Type, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMAddGlobal(self.as_ll(), ty.as_ll(), str_to_c_str(name).as_ptr()))
        }
    }
    pub fn add_global_in_address_space(&self, ty: &Type, name: &str, address_space: c_uint) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMAddGlobalInAddressSpace(self.as_ll(), ty.as_ll(), str_to_c_str(name).as_ptr(), address_space))
        }
    }
    pub fn get_named_global(&self, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMGetNamedGlobal(self.as_ll(), str_to_c_str(name).as_ptr()))
        }
    }
    pub fn get_first_global(&self) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMGetFirstGlobal(self.as_ll()))
        }
    }
    pub fn get_last_global(&self) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMGetLastGlobal(self.as_ll()))
        }
    }
    pub fn add_alias(&self, ty: &Type, aliasee: &Value, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMAddAlias(self.as_ll(), ty.as_ll(), aliasee.as_ll(), str_to_c_str(name).as_ptr()))
        }
    }
    pub fn add_function(&self, name: &str, function_ty: &Type) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMAddFunction(self.as_ll(), str_to_c_str(name).as_ptr(), function_ty.as_ll()))
        }
    }
    pub fn get_named_function(&self, name: &str) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMGetNamedFunction(self.as_ll(), str_to_c_str(name).as_ptr()))
        }
    }
    pub fn get_first_function(&self) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMGetFirstFunction(self.as_ll()))
        }
    }
    pub fn get_last_function(&self) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMGetLastFunction(self.as_ll()))
        }
    }
    pub fn get_or_insert_function(&self, name: &str, function_ty: &Type) -> Option<&Value> {
        unsafe {
            Value::from_ll(ll::LLVMGetOrInsertFunction(self.as_ll(), str_to_c_str(name).as_ptr(), function_ty.as_ll()))
        }
    }
    pub fn write_bitcode_to_file(&self, path: &str) -> c_int {
        unsafe {
            ll::LLVMWriteBitcodeToFile(self.as_ll(), str_to_c_str(path).as_ptr())
        }
    }
    pub fn create_function_pass_manager_for_module(&self) -> Option<&PassManager> {
        unsafe {
            PassManager::from_ll(ll::LLVMCreateFunctionPassManagerForModule(self.as_ll()))
        }
    }
    pub fn rust_add_module_flag(&self, name: &str, value: u32) {
        unsafe {
            ll::LLVMRustAddModuleFlag(self.as_ll(), str_to_c_str(name).as_ptr(), value)
        }
    }
    pub fn di_builder_create(&self) -> Box<DIBuilder> {
        unsafe {
            DIBuilder::box_from_ll(ll::LLVMDIBuilderCreate(self.as_ll()))
        }
    }
    pub fn rust_set_normalized_target(&self, triple: &str) {
        unsafe {
            ll::LLVMRustSetNormalizedTarget(self.as_ll(), str_to_c_str(triple).as_ptr())
        }
    }
    pub fn rust_link_in_external_bitcode(&self, bc: &str, len: size_t) -> bool {
        unsafe {
            ll::LLVMRustLinkInExternalBitcode(self.as_ll(), str_to_c_str(bc).as_ptr(), len)
        }
    }
    pub fn rust_run_restriction_pass(&self, syms: *const *const c_char, len: size_t) {
        unsafe {
            ll::LLVMRustRunRestrictionPass(self.as_ll(), syms, len)
        }
    }
    pub fn rust_mark_all_functions_nounwind(&self) {
        unsafe {
            ll::LLVMRustMarkAllFunctionsNounwind(self.as_ll())
        }
    }
}
llref_ty!(RustString, ll::RustStringRef);
llref_ty!(DebugLoc, ll::DebugLocRef);

pub fn context_create() -> Box<Context> {
    unsafe {
        Context::box_from_ll(ll::LLVMContextCreate())
    }
}
pub fn module_create_with_name_in_context(module_id: &str, c: &Context) -> Box<Module> {
    unsafe {
        Module::box_from_ll(ll::LLVMModuleCreateWithNameInContext(str_to_c_str(module_id).as_ptr(), c.as_ll()))
    }
}
pub fn const_i_cmp(pred: c_ushort, v1: &Value, v2: &Value) -> Option<&'static Value> {
    unsafe {
        Value::from_ll(ll::LLVMConstICmp(pred, v1.as_ll(), v2.as_ll()))
    }
}
pub fn const_f_cmp(pred: c_ushort, v1: &Value, v2: &Value) -> Option<&'static Value> {
    unsafe {
        Value::from_ll(ll::LLVMConstFCmp(pred, v1.as_ll(), v2.as_ll()))
    }
}
pub fn const_vector(scalar_constant_vals: &[&Value]) -> Option<&'static Value> {
    unsafe {
        Value::from_ll(ll::LLVMConstVector(scalar_constant_vals.as_ll(), scalar_constant_vals.len() as u32))
    }
}
pub fn create_target_data(string_rep: &str) -> Box<TargetData> {
    unsafe {
        TargetData::box_from_ll(ll::LLVMCreateTargetData(str_to_c_str(string_rep).as_ptr()))
    }
}
pub fn create_pass_manager() -> Box<PassManager> {
    unsafe {
        PassManager::box_from_ll(ll::LLVMCreatePassManager())
    }
}
pub fn initialize_passes() {
    unsafe {
        ll::LLVMInitializePasses()
    }
}
pub fn pass_manager_builder_create() -> Box<PassManagerBuilder> {
    unsafe {
        PassManagerBuilder::box_from_ll(ll::LLVMPassManagerBuilderCreate())
    }
}
pub fn rust_create_memory_buffer_with_contents_of_file(path: &str) -> Box<MemoryBuffer> {
    unsafe {
        MemoryBuffer::box_from_ll(ll::LLVMRustCreateMemoryBufferWithContentsOfFile(str_to_c_str(path).as_ptr()))
    }
}
pub fn create_memory_buffer_with_memory_range(input_data: &str, input_data_length: size_t, buffer_name: &str, requires_null: bool) -> Box<MemoryBuffer> {
    unsafe {
        MemoryBuffer::box_from_ll(ll::LLVMCreateMemoryBufferWithMemoryRange(str_to_c_str(input_data).as_ptr(), input_data_length, str_to_c_str(buffer_name).as_ptr(), to_c_bool(requires_null)))
    }
}
pub fn create_memory_buffer_with_memory_range_copy(input_data: &str, input_data_length: size_t, buffer_name: &str) -> Box<MemoryBuffer> {
    unsafe {
        MemoryBuffer::box_from_ll(ll::LLVMCreateMemoryBufferWithMemoryRangeCopy(str_to_c_str(input_data).as_ptr(), input_data_length, str_to_c_str(buffer_name).as_ptr()))
    }
}
pub fn is_multithreaded() -> bool {
    unsafe {
        from_c_bool(ll::LLVMIsMultithreaded())
    }
}
pub fn start_multithreaded() -> bool {
    unsafe {
        from_c_bool(ll::LLVMStartMultithreaded())
    }
}
pub fn rust_get_last_error() -> String {
    unsafe {
        c_str_to_string(ll::LLVMRustGetLastError())
    }
}
pub fn rust_print_pass_timings() {
    unsafe {
        ll::LLVMRustPrintPassTimings()
    }
}
pub fn set_debug(enabled: c_int) {
    unsafe {
        ll::LLVMSetDebug(enabled)
    }
}
pub fn initialize_x86_target_info() {
    unsafe {
        ll::LLVMInitializeX86TargetInfo()
    }
}
pub fn initialize_x86_target() {
    unsafe {
        ll::LLVMInitializeX86Target()
    }
}
pub fn initialize_x86_target_mc() {
    unsafe {
        ll::LLVMInitializeX86TargetMC()
    }
}
pub fn initialize_x86_asm_printer() {
    unsafe {
        ll::LLVMInitializeX86AsmPrinter()
    }
}
pub fn initialize_x86_asm_parser() {
    unsafe {
        ll::LLVMInitializeX86AsmParser()
    }
}
pub fn initialize_arm_target_info() {
    unsafe {
        ll::LLVMInitializeARMTargetInfo()
    }
}
pub fn initialize_arm_target() {
    unsafe {
        ll::LLVMInitializeARMTarget()
    }
}
pub fn initialize_arm_target_mc() {
    unsafe {
        ll::LLVMInitializeARMTargetMC()
    }
}
pub fn initialize_arm_asm_printer() {
    unsafe {
        ll::LLVMInitializeARMAsmPrinter()
    }
}
pub fn initialize_arm_asm_parser() {
    unsafe {
        ll::LLVMInitializeARMAsmParser()
    }
}
pub fn initialize_mips_target_info() {
    unsafe {
        ll::LLVMInitializeMipsTargetInfo()
    }
}
pub fn initialize_mips_target() {
    unsafe {
        ll::LLVMInitializeMipsTarget()
    }
}
pub fn initialize_mips_target_mc() {
    unsafe {
        ll::LLVMInitializeMipsTargetMC()
    }
}
pub fn initialize_mips_asm_printer() {
    unsafe {
        ll::LLVMInitializeMipsAsmPrinter()
    }
}
pub fn initialize_mips_asm_parser() {
    unsafe {
        ll::LLVMInitializeMipsAsmParser()
    }
}
pub fn rust_create_target_machine(triple: &str, cpu: &str, features: &str, model: CodeGenModel, reloc: RelocMode, level: CodeGenOptLevel, enable_segstk: bool, use_soft_fp: bool, no_frame_pointer_elim: bool, position_independent_executable: bool, function_sections: bool, data_sections: bool) -> Box<TargetMachine> {
    unsafe {
        TargetMachine::box_from_ll(ll::LLVMRustCreateTargetMachine(str_to_c_str(triple).as_ptr(), str_to_c_str(cpu).as_ptr(), str_to_c_str(features).as_ptr(), model, reloc, level, enable_segstk, use_soft_fp, no_frame_pointer_elim, position_independent_executable, function_sections, data_sections))
    }
}
pub fn rust_set_llvm_options(argc: c_int, argv: *const *const c_char) {
    unsafe {
        ll::LLVMRustSetLLVMOptions(argc, argv)
    }
}
pub fn rust_print_passes() {
    unsafe {
        ll::LLVMRustPrintPasses()
    }
}
pub fn rust_open_archive(path: &str) -> Option<&Archive> {
    unsafe {
        Archive::from_ll(ll::LLVMRustOpenArchive(str_to_c_str(path).as_ptr()))
    }
}
pub fn version_major() -> c_int {
    unsafe {
        ll::LLVMVersionMajor()
    }
}
pub fn version_minor() -> c_int {
    unsafe {
        ll::LLVMVersionMinor()
    }
}
