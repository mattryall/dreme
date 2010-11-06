package dreme.runtime;

import dreme.*;
import dreme.macros.interop.ConversionUtils;

import java.io.*;
import java.net.Socket;
import java.net.SocketTimeoutException;

public class Networking {

    public static final Procedure OPEN_TCP_SOCKET_STREAM = new Procedure("open-tcp-stream-socket") {

        protected SchemeObject apply(List arguments, Environment environment) {
            SchemeString host = (SchemeString) arguments.head();
            SchemeString service = (SchemeString) arguments.tail().head();

            InputOutputPort port;
            try {
                Socket socket = new Socket(host.getValue(), Integer.valueOf(service.getValue()));
                port = new InputOutputPort(socket, true);
            }
            catch (IOException e) {
                throw new RuntimeException(e);
            }
            return ConversionUtils.toSchemeObject(port);
        }
    };

    public static final Procedure READ_STRING = new Procedure("read-string") {

        protected SchemeObject apply(List arguments, Environment environment) {
            InputOutputPort port = (InputOutputPort) ConversionUtils.toJavaObject(arguments.head());
            try
            {
                return ConversionUtils.toSchemeObject(port.getIn().readLine() + "\n");
            }
            catch (SocketTimeoutException e)
            {
                return SchemeBoolean.FALSE;
            }
            catch (IOException e)
            {
                throw new RuntimeException(e);
            }
        }
    };

    public static final Procedure WRITE_STRING = new Procedure("write-string") {

        protected SchemeObject apply(List arguments, Environment environment) {
            InputOutputPort port = (InputOutputPort) ConversionUtils.toJavaObject(arguments.head());
            String value = (String) ConversionUtils.toJavaObject(arguments.tail().head());

            port.getOut().println(value);
            port.getOut().flush();
            return Unspecified.INSTANCE;
        }
    };

    private static class InputOutputPort
    {
        private BufferedReader in;
        private PrintWriter out;

        private InputOutputPort(Socket socket, boolean nonBlocking) throws IOException
        {
            if (nonBlocking) {
                socket.setSoTimeout(100);
            }
            this.in = new BufferedReader(new InputStreamReader(socket.getInputStream()));
            this.out = new PrintWriter(new OutputStreamWriter(socket.getOutputStream()));
        }

        public BufferedReader getIn()
        {
            return in;
        }

        public PrintWriter getOut()
        {
            return out;
        }
    }

}
