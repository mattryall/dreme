package dreme.macros;

import dreme.Environment;
import dreme.Identifier;
import dreme.SchemeObject;

public class DefineMacro extends BindingMacro {

    protected void bind(Environment environment, Identifier identifier, SchemeObject value) {
        environment.define(identifier, value);
    }
}
