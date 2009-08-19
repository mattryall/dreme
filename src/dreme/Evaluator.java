package dreme;

import java.io.IOException;
import java.util.Map;
import java.util.HashMap;

public class Evaluator
{
    private Map<Tokens.BareWord, Function> environment = new HashMap<Tokens.BareWord, Function>();

    public Object eval(String scheme) throws IOException
    {
        TokenStream tokens = new TokenStream(scheme);
        Parser parser = new Parser(tokens);
        Cons cons = parser.parse();
        return cons.evaluate(environment);
    }

    public void define(String name, Function function)
    {
        environment.put(new Tokens.BareWord(name), function);
    }

    public interface Function
    {
        Cons apply(Cons arguments, Map<Tokens.BareWord, Function> environment);
    }
}
