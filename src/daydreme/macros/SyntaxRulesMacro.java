package daydreme.macros;

import daydreme.List;
import daydreme.ExecutionContext;

public class SyntaxRulesMacro extends PrimitiveMacro {

    public void process(List body, ExecutionContext ctx) {
        SyntaxRules rules = new SyntaxRules(List.toList(body.head()), body.tail());
        ctx.returnValue(rules);
    }
}
