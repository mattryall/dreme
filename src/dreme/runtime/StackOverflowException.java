package dreme.runtime;

class StackOverflowException extends RuntimeException
{
    StackOverflowException() {
    }

    StackOverflowException(String message) {
        super(message);
    }

    StackOverflowException(String message, Throwable cause) {
        super(message, cause);
    }

    StackOverflowException(Throwable cause) {
        super(cause);
    }
}
