pub mod flattened;
pub mod normal;

pub use self::flattened::*;
pub use self::normal::*;

pub trait HasTokenKind {
    fn is_arg_delim(&self) -> bool;
    fn is_redirect(&self) -> bool;
    fn is_pipe(&self) -> bool;
    fn is_delim(&self) -> bool;
}
