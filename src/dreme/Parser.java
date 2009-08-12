package dreme;

import java.io.*;
import java.util.Stack;

import static dreme.Tokens.*;

public class Parser
{
    static private String oneToken = "(test)"; // (test . ())
    static private String twoTokens = "(test 1)"; // (test . (1 . ()))
    static private String threeTokens = "(test 1 2)"; // (test . (1 . (2 . ())))
    static private String nested = "(test (foo))"; // (test . ((foo . ()) . ()))
    static private String hard = "(test (foo) bar)"; // (test . ((foo . ()) . (bar . ())))
    static private String source = "(first (pass 1 -345))"; // (first . ((pass . (1 . (-345 . ()))) . ()))

    public static void main(String[] args) throws Exception
    {
        Stack<Cons> stack = new Stack<Cons>();
        Cons currentCons = null;
        TokenStream stream = new TokenStream(new StringReader(source));
        Tokens.Token t;
        while ((t = stream.getToken()) != Tokens.END_OF_STREAM)
        {
            if (t == OPEN_PARENS) {
                Cons newCons = new Cons();
                if (currentCons != null)
                    currentCons.append(newCons);
                currentCons = newCons;
                stack.push(newCons);
            }
            else if (t instanceof Value) {
                Cons newCons = new Cons(t);
                if (currentCons == null)
                    throw new IllegalStateException("Unexpected value:" + t);
                currentCons.append(newCons);
                currentCons = newCons;
            }
            else if (t == CLOSE_PARENS) {
                currentCons = stack.pop();
            }
            else {
                throw new RuntimeException("I got a token I wasn't expecting!");
            }
            if (!stack.isEmpty())
                System.out.println("currentStack = " + stack.peek());
        }
        System.out.println("staSck = " + stack);
        System.out.println(currentCons);
    }

    static class Cons
    {
        private Object car;
        private Object cdr;

        public Cons()
        {
        }

        public Cons(Object car)
        {
            this.car = car;
        }

        public void append(Object o)
        {
            if (car == null)
                car = o;
            else
                cdr = o;
        }

        public String toString()
        {
            return "(" + car + " . " + (cdr == null ? "()" : cdr) + ")";
        }
    }
}