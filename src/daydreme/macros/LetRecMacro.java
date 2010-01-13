package daydreme.macros;

import daydreme.*;

import static daydreme.List.toList;

public class LetRecMacro extends PrimitiveMacro {
    public void process(List arguments, ExecutionContext ctx) {
        List declarations = toList(arguments.head());
        List body = new List(new Identifier("begin"), arguments.tail());
        Environment bodyEnv = ctx.getEnvironment(); // TODO: should be .copy()
        for (SchemeObject declaration : declarations) {
            if (!(declaration instanceof Pair))
                throw new IllegalArgumentException("Invalid binding: " + declaration);
            List decl = toList(declaration);
            System.out.println("Let rec binding " + decl.get(0) + " as " + decl.get(1));
            bodyEnv.bind(decl.get(0), decl.get(1));
        }
        ctx.executeInPlace(body, bodyEnv);
    }
}
