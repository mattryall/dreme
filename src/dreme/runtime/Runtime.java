package dreme.runtime;

import dreme.*;
import dreme.macros.*;
import dreme.macros.interop.MethodInvocationMacro;
import dreme.macros.interop.ConstructorInvocationMacro;
import org.apache.log4j.Logger;

import java.io.InputStreamReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import static dreme.runtime.Procedures.*;

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

        evaluator = new ListEvaluator(stackSizeLimit, environment);
        List builtIns = getSchemeBuiltIns();
        for (SchemeObject builtIn : builtIns) {
            evaluator.evaluate(List.toList(builtIn));
        }
    }

    public SchemeObject run(List executable) {
        return evaluator.evaluate(executable);
    }

    private static final Map<String, SchemeObject> BUILT_INS = new HashMap<String, SchemeObject>();

    static {
        // primitive macros
        BUILT_INS.put("lambda", new LambdaMacro());
        BUILT_INS.put("define", new DefineMacro());
        BUILT_INS.put("define-syntax", new DefineSyntaxMacro());
        BUILT_INS.put("syntax-rules", new SyntaxRulesMacro());
        BUILT_INS.put("quote", new QuoteMacro());
        BUILT_INS.put("set!", new SetMacro());
        BUILT_INS.put("if", new IfMacro());
        BUILT_INS.put("call-with-current-continuation", new CallCCOperator());

        // interop macros
        BUILT_INS.put("new", new ConstructorInvocationMacro());
        BUILT_INS.put(".", new MethodInvocationMacro());

        // built-in procedures
        BUILT_INS.put("cons", CONS);
        BUILT_INS.put("car", CAR);
        BUILT_INS.put("cdr", CDR);
        BUILT_INS.put("eq?", EQ);
        BUILT_INS.put("eqv?", EQUAL); // eqv? is same as equal? for the moment
        BUILT_INS.put("equal?", EQUAL);
        BUILT_INS.put("pair?", PAIR);
        BUILT_INS.put("null?", NULL);
        BUILT_INS.put("integer?", INTEGER);
        BUILT_INS.put("env", ENV);
        BUILT_INS.put("+", PLUS);
        BUILT_INS.put("-", MINUS);
        BUILT_INS.put("*", MULTIPLY);
        BUILT_INS.put("/", DIVIDE);
        BUILT_INS.put(">", GT);
        BUILT_INS.put("<", LT);
        BUILT_INS.put(">=", GE);
        BUILT_INS.put("<=", LE);
        BUILT_INS.put("=", NUMERIC_EQUALS);

        BUILT_INS.put("symbol->string", Coercions.TO_STRING);
        BUILT_INS.put("sleep", SLEEP);
        
        BUILT_INS.put("string-append", STRING_APPEND);
        BUILT_INS.put("open-tcp-stream-socket", Networking.OPEN_TCP_SOCKET_STREAM);
    }

    private static List getSchemeBuiltIns() {
        TokenStream tokenStream = new TokenStream(getSchemeBuiltInsReader());
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

    private static java.io.Reader getSchemeBuiltInsReader() {
        return new InputStreamReader(Runtime.class.getResourceAsStream("/dreme/builtin-macros.scm"));
    }

    public void bind(String var, SchemeObject val) {
        evaluator.bind(new Identifier(var), val);
    }
}
