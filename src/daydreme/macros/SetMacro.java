package daydreme.macros;

import daydreme.Environment;
import daydreme.Identifier;
import daydreme.SchemeObject;

public class SetMacro extends BindingMacro {
    protected void bind(Environment environment, Identifier identifier, SchemeObject value) {
        environment.set(identifier, value);
    }
}
