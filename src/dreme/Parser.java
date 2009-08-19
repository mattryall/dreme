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

    static class Cons
    {
        Object car;
        Object cdr;

        public Cons()
        {
            this(null, null);
        }

        public Cons(Object car)
        {
            this(car, null);
        }

        public Cons(Object car, Object cdr)
        {
            this.car = car;
            this.cdr = cdr;
        }

        public void assign(Object o)
        {
            if (car == null)
                car = o;
            else
                cdr = o;
        }

        public Cons append(Object o)
        {
            if (this.car == null) { // we're the first entry in an empty list
                this.car = o;
                return this;
            }
            return (Cons) (this.cdr = new Cons(o));
        }

        public String toString()
        {
            return "(" + car + " . " + (cdr == null ? "()" : cdr) + ")";
        }

        public boolean equals(Object o)
        {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;

            Cons cons = (Cons) o;

            if (car != null ? !car.equals(cons.car) : cons.car != null) return false;
            return !(cdr != null ? !cdr.equals(cons.cdr) : cons.cdr != null);
        }

        public int hashCode()
        {
            int result = car != null ? car.hashCode() : 0;
            result = 31 * result + (cdr != null ? cdr.hashCode() : 0);
            return result;
        }
    }
}