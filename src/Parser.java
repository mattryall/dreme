/**
 * Created by IntelliJ IDEA.
 * User: oysta
 * Date: Aug 12, 2009
 * Time: 6:53:33 PM
 * To change this template use File | Settings | File Templates.
 */

import java.io.*;
import java.util.*;

public class Parser
{
    public static interface Token {}
    public static final Token OPEN_PARENS = new Token() {
        public String toString()
        {
            return "(";
        }
    };
    public static final Token CLOSE_PARENS = new Token() {
        public String toString()
        {
            return ")";
        }
    };
    public static class BareWord implements Token {
        private String value;

        public BareWord(String value)
        {
            this.value = value;
        }

        public String toString()
        {
            return value;
        }
    }

    static private String source = "(first (pass 1))";

    public static void main(String[] args) throws Exception
    {
        List<Token> token = new LinkedList<Token>();

        Reader sourceReader = new StringReader(source);
        int readChar = 0;
        StringBuffer bareWord = null;
        while ((readChar = sourceReader.read()) != -1)
        {
            char theChar = (char) readChar;
            switch (theChar)
            {
                case '(':
                    if (bareWord != null) {
                        token.add(new BareWord(bareWord.toString()));
                        bareWord = null;
                    }
                    token.add(OPEN_PARENS);
                    break;
                case ')':
                    if (bareWord != null) {
                        token.add(new BareWord(bareWord.toString()));
                        bareWord = null;
                    }
                    token.add(CLOSE_PARENS);
                    break;
                case ' ':
                case '\n':
                case '\t':
                    if (bareWord != null) {
                        token.add(new BareWord(bareWord.toString()));
                        bareWord = null;
                    }
                    break;
                default:
                    if (bareWord == null)
                        bareWord = new StringBuffer(50);
                    bareWord.append(theChar);
            }
        }
        for (Token t : token)
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