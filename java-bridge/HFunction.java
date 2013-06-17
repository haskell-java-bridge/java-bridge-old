import java.lang.reflect.*;

/**
 * An HFunction can be used as a invocation handler which actually
 * handles the invocation by calling a Haskell function.
 *
 * @author Julian Fleischer
 */
public class HFunction implements InvocationHandler {

    private final long hFunction;

    static native void release(long func);
    static native Object call(long func, Method self, Object[] args);

    /**
     * Creates an HFunction from a function pointer.
     *
     * @param func
     */
    public HFunction(long func) {
        hFunction = func;
    }

    public Object invoke(Object proxy, Method method, Object[] args) {
        return call(hFunction, method, args);
    }

    protected void finalize() {
        release(hFunction);
    }

    /**
     * Make an HFunction for a given iface from a function pointer.
     *
     * @param iface
     * @param func
     */
    @SuppressWarnings("unchecked")
    public static <T> T makeFunction(Class<T> iface, long func) {
        InvocationHandler handler = new HFunction(func);
        return (T) Proxy.newProxyInstance(
                            iface.getClassLoader(),
                            new Class<?>[] { iface },
                            handler);
    }
}



