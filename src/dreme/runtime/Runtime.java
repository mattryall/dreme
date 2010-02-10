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
    private ListEvaluator evaluator;

    public Runtime() {
        this(SchemeStack.UNLIMITED_STACK_SIZE);
    }

    public Runtime(int stackSizeLimit) {
        Environment environment = new Environment();
        environment.define(new Identifier(Unspecified.INSTANCE.toString()), Unspecified.INSTANCE);
        for (Map.Entry<String, SchemeObject> entry : BUILT_INS.entrySet()) {
            environment.define(new Identifier(entry.getKey()), entry.getValue());
        }
        for (Map.Entry<String, Procedure> entry : BUILT_IN_PROCEDURES.entrySet()) {
            environment.define(new Identifier(entry.getKey()), entry.getValue());
        }

        evaluator = new ListEvaluator(stackSizeLimit, environment);
        List builtIns = getOtherBuiltIns();
        for (SchemeObject builtIn : builtIns) {
            evaluator.evaluate(List.toList(builtIn));
        }
    }

    public SchemeObject run(List executable) {
        return evaluator.evaluate(executable);
    }

    private static final Map<String, SchemeObject> BUILT_INS = new HashMap<String, SchemeObject>();

    static {
        BUILT_INS.put("lambda", new LambdaMacro());
        BUILT_INS.put("define", new DefineMacro());
        BUILT_INS.put("define-syntax", new DefineSyntaxMacro());
        BUILT_INS.put("syntax-rules", new SyntaxRulesMacro());
        BUILT_INS.put("quote", new QuoteMacro());
        BUILT_INS.put("set!", new SetMacro());
        BUILT_INS.put("if", new IfMacro());
        BUILT_INS.put("call-with-current-continuation", new CallCCOperator());
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
