package dreme.runtime;

import dreme.*;
import dreme.macros.*;
import org.apache.log4j.Logger;

import java.io.InputStreamReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import static dreme.Procedures.BUILT_IN_PROCEDURES;

public class Runtime {
    private static final Logger log = Logger.getLogger(Runtime.class);

    public static SchemeObject run(List executable) {
        Environment environment = new Environment();
        environment.define(new Identifier(Unspecified.INSTANCE.toString()), Unspecified.INSTANCE);
        for (Map.Entry<String, Macro> entry : BUILT_IN_MACROS.entrySet()) {
            environment.define(new Identifier(entry.getKey()), entry.getValue());
        }
        for (Map.Entry<String, Procedure> entry : BUILT_IN_PROCEDURES.entrySet()) {
            environment.define(new Identifier(entry.getKey()), entry.getValue());
        }

        ListEvaluator evaluator = new ListEvaluator();
        List builtIns = getOtherBuiltIns();
        for (SchemeObject builtIn : builtIns) {
            evaluator.evaluate(List.toList(builtIn), environment);
        }
        return evaluator.evaluate(executable, environment);
    }

    private static final Map<String, Macro> BUILT_IN_MACROS = new HashMap<String, Macro>();

    static {
        BUILT_IN_MACROS.put("lambda", new LambdaMacro());
        BUILT_IN_MACROS.put("define", new DefineMacro());
        BUILT_IN_MACROS.put("define-syntax", new DefineSyntaxMacro());
        BUILT_IN_MACROS.put("syntax-rules", new SyntaxRulesMacro());
        BUILT_IN_MACROS.put("quote", new QuoteMacro());
        BUILT_IN_MACROS.put("set!", new SetMacro());
        BUILT_IN_MACROS.put("if", new IfMacro());
    }

    private static List getOtherBuiltIns() {
        TokenStream tokenStream = new TokenStream(getOtherBuiltInsReader());
        Parser parser = new Parser();
        List result = new List();
        List form;
        try {
            while ((form = parser.parse(tokenStream)) != null) {
                result.add(form);
            }
        }
        catch (IOException e) {
            log.error("Error parsing token stream", e);
        }
        return result;
    }

    private static java.io.Reader getOtherBuiltInsReader() {
        return new InputStreamReader(Runtime.class.getResourceAsStream("/dreme/builtin-macros.scm"));
    }

}
