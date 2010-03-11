package dreme.runtime;

import dreme.*;
import dreme.Number;
import dreme.macros.interop.ConversionUtils;

import java.io.IOException;
import java.net.Socket;

public class Networking {

    public static final Procedure OPEN_TCP_SOCKET_STREAM = new Procedure("open-tcp-stream-socket") {

        protected SchemeObject apply(List arguments, Environment environment) {
            SchemeString host = (SchemeString) arguments.head();
            SchemeString service = (SchemeString) arguments.tail().head();

            Socket socket;
            try {
                socket = new Socket(host.getValue(), Integer.valueOf(service.getValue()));
            }
            catch (IOException e) {
                throw new RuntimeException(e);
            }
            return ConversionUtils.toSchemeObject(socket);
        }
    };

}
