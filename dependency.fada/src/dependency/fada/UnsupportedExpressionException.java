package dependency.fada;

public class UnsupportedExpressionException extends Exception {
	private static final long serialVersionUID = -2561185852487593935L;

    public UnsupportedExpressionException() {
        super();
    }

    public UnsupportedExpressionException(String message) {
        super(message);
    }

    public UnsupportedExpressionException(String message, Throwable cause) {
        super(message, cause);
    }

    public UnsupportedExpressionException(Throwable cause) {
        super(cause);
    }
}
