package dreme;

import java.io.*;
import java.util.Stack;

import static dreme.Tokens.*;

public class Parser
{
    private final TokenStream tokenStream;
    
    public Parser(TokenStream stream)
    {
        this.tokenStream = stream;
    }
    
    public Cons parse() throws IOException
    {
        final Cons rootCons = new Cons();
        Cons lp = rootCons;
        Stack<Cons> stack = new Stack<Cons>();
        Tokens.Token t;
        while ((t = tokenStream.getToken()) != Tokens.END_OF_STREAM)
        {
            if (t == OPEN_PARENS)
            {
                Cons listCons;
                if (lp == rootCons) // first parens is special
                {
                    listCons = rootCons;
                }
                else
                {
                    listCons = new Cons();
                    lp.assign(listCons);
                }
                Cons firstItemCons = new Cons();
                listCons.car = firstItemCons;
                stack.push(listCons);
                lp = firstItemCons;
            }
            else if (t instanceof Value)
            {
                if (lp == rootCons)
                    throw new IllegalStateException("List pointer is null, atom outside list!");
                lp = lp.append(t);
            }
            else if (t == CLOSE_PARENS)
            {
                if (stack.isEmpty())
                    throw new IllegalStateException("Too many closing parens");
                lp = stack.pop();
            }
            else
            {
                throw new RuntimeException("I got a token I wasn't expecting!");
            }
        }

        if (!stack.isEmpty())
        {
            throw new IllegalStateException("Not enough closing parens: " + stack);
        }
        
        return (Cons) rootCons.car;
    }
}