package daydreme;

import dreme.TokenStream;

import java.io.IOException;
import java.io.InputStreamReader;

import static daydreme.Procedures.*;

public class Reader {
    public void read(java.io.Reader reader) throws IOException {
        Environment environment = createDefaultEnvironment();
        evaluate(reader, environment);
    }

    private static void evaluate(java.io.Reader reader, Environment environment) throws IOException {
        TokenStream tokenStream = new TokenStream(reader);
        Parser parser = new Parser();
        List form;
        while ((form = parser.parse(tokenStream)) != null) {
            form.evaluate(environment);
        }
    }

    public static Environment createDefaultEnvironment() throws IOException {
        Environment environment = new Environment(DEFINE, SET, LET, LETREC, IF,
            CONS, CAR, CDR, EQV, CALL_CC,
            PLUS, MINUS, MULTIPLY, DIVIDE, GT, LT, GE, LE, EQ);
        evaluate(getBuiltInMacros(), environment);
        return environment;
    }

    private static java.io.Reader getBuiltInMacros() {
        return new InputStreamReader(Reader.class.getResourceAsStream("builtin-macros.scm"));
    }
}
