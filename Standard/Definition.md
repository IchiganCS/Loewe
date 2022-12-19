# Definition of Loewe Language

## Type qualifiers

A type consists out of a class name and type modifiers. There are only a few types of modifiers

- **Owner**: The variable is the owner of the value and is the standard type. If this value is destroyed, the memory is cleared. `classname`
- **Pointer**: The variable is a raw pointer to an existing value. `classname*`
- **Const Pointer**: The variable is a raw, readonly pointer to an existing value `const classname*`
- **Reference**: The variable is a checked pointer to an existing value. `classname&`
    - It can be checked if it is still valid via `alive(value)` where value is of type `classname&`. However, it is not automatically checked.
- **Const Reference**: The variable is a checked, readonly pointer to an existing value. `const classname&`
    - It can be checked if it is still valid via `alive(value)` where value is of type `const classname&`. However, it is not automatically checked.

