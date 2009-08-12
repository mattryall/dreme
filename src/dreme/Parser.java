package dreme;

import java.io.*;

public class Parser
{
    static private String source = "(first (pass 1))";

    public static void main(String[] args) throws Exception
    {
        TokenStream stream = new TokenStream(new StringReader(source));
        Tokens.Token t;
        while ((t = stream.getToken()) != Tokens.END_OF_STREAM)
        {
            System.out.println(t);
        }
    }


    static class Cons
    {
        private Object car;
        private Object cdr;
    }
}