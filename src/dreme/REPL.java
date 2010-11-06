package dreme;

import java.io.*;
import java.util.ArrayList;
import java.util.Collections;

import dreme.runtime.Runtime;

/**
 * REPL for the Dreme runtime
 */
public class REPL implements Runnable {

    private final Parser parser = new Parser();
    private final InputStream in;
    private final PrintStream out;
    private final PrintStream err;
    private final List commandLine;
    private final boolean interactive;

    public REPL(InputStream in, PrintStream out, PrintStream err, List commandLine, boolean interactive) {
        this.in = in;
        this.out = out;
        this.err = err;
        this.commandLine = commandLine;
        this.interactive = interactive;
    }

    public static void main(String[] args) throws FileNotFoundException {
        int i = 0;
        java.util.List<String> sourceFileNames = new ArrayList<String>();
        for (; i < args.length; i++) {
            if (args[i].equals("--")) break;
            sourceFileNames.add(args[i]);
        }
        List commandLine = new List();
        for (i++ ; i < args.length; i++) {
            commandLine.add(new SchemeString(args[i]));
        }

        java.util.List<InputStream> streams = new ArrayList<InputStream>();
        for (String fileName : sourceFileNames) {
            streams.add(new FileInputStream(fileName));
        }
        InputStream argf = streams.isEmpty() ? System.in :
            new SequenceInputStream(Collections.enumeration(streams));

        boolean interactive = System.console() != null && streams.isEmpty();
        REPL repl = new REPL(argf, System.out, System.err, commandLine, interactive);
        repl.run();
    }

    public void run() {
        Reader inReader = new InputStreamReader(in);
        TokenStream inTokens = new TokenStream(inReader);
        Runtime runtime = new Runtime();
        runtime.bind("display", new DisplayProcedure(out));
        runtime.bind("command-line", new CommandLineProcedure(commandLine));
        while (true) {
            try {
                if (interactive) {
                    out.print("dreme> ");
                    out.flush();
                }
                List inputForm = parser.parse(inTokens);
                if (inputForm == null) {
                    out.println();
                    out.flush();
                    return;
                }
                SchemeObject result = runtime.run(inputForm);
                if (result != Unspecified.INSTANCE)
                    out.println(result);
            }
            catch (RuntimeException e) {
                out.println("ERROR: " + e.getClass().getSimpleName() + ": " + e.getMessage());
                e.printStackTrace(err);
            }
            catch (IOException e) {
                e.printStackTrace(err);
            }
        }
    }

    private static class DisplayProcedure extends Procedure {
        private SchemeObjectVisitor displayVisitor;

        private DisplayProcedure(final PrintStream out) {
            displayVisitor = new AbstractSchemeObjectVisitor() {
                public void object(SchemeObject object) {
                    out.print(object);
                    out.flush();
                }

                public void string(SchemeString string) {
                    out.print(string.getValue());
                    out.flush();
                }

                public void unspecified(Unspecified unspecified) {
                    // do nothing
                }
            };
        }

        protected SchemeObject apply(List arguments, Environment environment) {
            arguments.head().acceptVisitor(displayVisitor);
            return Unspecified.INSTANCE;
        }
    }

    private class CommandLineProcedure extends Procedure {
        private final List commandLine;

        public CommandLineProcedure(List commandLine) {
            super("command-line");
            this.commandLine = commandLine;
        }

        protected SchemeObject apply(List arguments, Environment environment) {
            return commandLine;
        }
    }
}
