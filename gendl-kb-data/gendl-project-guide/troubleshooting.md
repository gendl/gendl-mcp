# Gendl Project Troubleshooting Guide

## Common Issues and Solutions

### System Loading Issues

**Problem**: Project won't load with error about missing system
**Solution**: 
- Check that the path in `cl-lite` and `pushnew` calls is correct and
  evaluate `ql:*local-project-directories*` to confirm that your
  expected project directory is listed.
- Verify your ASDF system name matches your project name
- Make sure Quicklisp is loaded first with `(load-quicklisp)`, can
  verify by evaluating `(find-package :ql)`

**Problem**: "The variable X is unbound" errors during loading
**Solution**:
- Check your file loading order in file-ordering.isc
- Ensure dependent objects are defined before objects that use them
- Verify package names in source files match your defined package

### Package Issues

**Problem**: Symbols not found or wrong package errors
**Solution**:
- Check that all source files have the correct `in-package` statement
- Make sure you're using the right package prefix when referencing objects
- Use package-qualified names for functions/objects outside your package
- Export supported symbols from packages with (:export ...) in package.lisp. 
- Use single-colon i.e. `my-package:my-symbol` to specify exported
  symbols outside the package you're currently in.
- Avoid use of double colons (::) to access non-exported symbols;
  question why the symbols aren't exported and whether they should be,
  or whether maybe you should not be accessing if not exported.

### Object Definition Issues

**Problem**: Object not created properly or missing slots
**Solution**:
- Double-check the inheritance hierarchy (mixin list)
- Verify that all required input slots are provided
- Check for typos in slot names

**Problem**: Object child not properly instantiated
**Solution**:
- Make sure the child's type is not dependent on (the-child ... ) at
  all [type has to be known in order for `the-child` to work). Use
  :type (:sequence ...) for making a heterogeneous :sequence.
- Check that all required parameters are passed to the child
- Verify that any computed values for the child's parameters are valid

### Reference Issues

**Problem**: Can't access object properties with `the` or `theo`
**Solution**:
- Make sure you're using the correct syntax for references
- For nested references, check each level in the chain
- Verify that the referenced slots actually exist on the object
- Use (defaulting (the[o] ...) <default-value>) if unsure whether a
  message is handled (this avoids "could not handle" errors).

## Debugging Techniques

1. **Use `message-list`**: Examine the structure of your objects
   ```lisp
(the-object *assembly* (message-list :category :inputs))
(the-object *assembly* (message-list :category :required-input-slots))
(the-object *assembly* (message-list :category :optional-input-slots))
   ```

3. **Check slot values**: Directly check the value of a slot, evaluating slot name at runtime if need be:
   ```lisp
   (let ((my-slot :length))
     (theo *your-object* (evaluate my-slot)))
   ```

## Performance Issues

1. **Large object definitions with endless list of computed-slots**:
   Break them into child objects in which you can hide complexity from
   the parent and just reference relevant results.
2. **Complex computed slots**: Optimize calculations or cache intermediate results
4. **Dependency chains**: Minimize long chains of interdependent slots

## Best Practices for Robust Projects

1. **Use clear naming conventions**: long indentifiers, kebab-case
   (with hyphen separation). Makes debugging easier
2. **Document your code**: Add comments explaining complex logic
3. **Modular design**: Keep related functionality together
4. **Test incrementally**: Build and test in small steps (!)
5. **Use version control**: Track changes to your project

## Common Error Messages and Their Meaning

1. **"Symbol X not found in package Y"**:
   - Symbol is being used without having been exported or
     package-qualified
   - Solution: Export the symbol and use full package qualification
     with single colon.

2. **"Undefined function ... called with arguments ... "**:
   - Function or slot referenced before it's defined (if slot, it
     means no other definitions of that slot in other object
     definitions either -- otherwise it'd be a "... could not handle
     the ... message" (see below)).
   - Solution: Check file loading order or function definition

3. **"... could not handle the ___ message" or "**:
   - Trying to access a slot that doesn't exist
   - Solution: Check slot name spelling and object definition

## Tips for Testing and Validation

1. **Create test fixtures**: Standard objects for testing
2. **Write test functions**: Verify object behavior
3. **Test edge cases**: Ensure objects work with extreme values
4. **Validate inputs**: Check input values before using them
5. **Add debugging messages**: Use format statements for tracing issues

## Resources for More Help

1. Gendl Documentation
2. GDL User Guide
3. Common Lisp HyperSpec
4. Gendl Community Support (##gendl on irc libera.chat)
