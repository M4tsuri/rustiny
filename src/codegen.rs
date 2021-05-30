//! build a executable file for tinyvm from our IR

use std::path::Path;

pub struct ExeBuilder {
    res: Vec<u8>,
    output_file: Path,
    
}