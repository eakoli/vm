UC Compiler
===========

source -> parser -> AST

AST -> assember -> BIN( pcode + metadata )

BIN -> generator -> CPP/H (native import/export impls for BIN)

At runtime the VM can execute PCODE directly, or JIT compile routines to raw assembly.


CodeBlock
    Represents a block os code in scope.
    Declarations should be added 'inline' so that they can be 'registered' as they are 
    seen into the variable resolver. This way uses of variables can only occur after its been defined.