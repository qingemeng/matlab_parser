package dependency.fada;

public class UnsupportedConditionException extends Exception {
	private static final long serialVersionUID = -2271254409363928582L;

    public UnsupportedConditionException() {
        super();
    }

    public UnsupportedConditionException(String message) {
        super(message);
    }

    public UnsupportedConditionException(String message, Throwable cause) {
        super(message, cause);
    }

    public UnsupportedConditionException(Throwable cause) {
        super(cause);
    }
}
