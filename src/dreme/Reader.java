package dreme;

import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Map;

import static dreme.Procedures.*;

public class Reader {
    public void read(java.io.Reader reader) throws IOException {
        Environment environment = createDefaultEnvironment();
        evaluate(reader, environment);
    }

    private static void evaluate(java.io.Reader reader, Environment environment) throws IOException {
        TokenStream tokenStream = new TokenStream(reader);
        Parser parser = new Parser();
        ListEvaluator evaluator = new ListEvaluator();
        List form;
        while ((form = parser.parse(tokenStream)) != null) {
            evaluator.evaluate(form, environment);
        }
    }

    public static Environment createDefaultEnvironment() throws IOException {
        Environment environment = new Environment();
        for (Map.Entry<String, Procedure> entry : BUILT_IN_PROCEDURES.entrySet()) {
            environment.define(new Identifier(entry.getKey()), entry.getValue());
        }
        evaluate(getBuiltInMacros(), environment);
        return environment;
    }

    private static java.io.Reader getBuiltInMacros() {
        return new InputStreamReader(Reader.class.getResourceAsStream("builtin-macros.scm"));
    }
}
