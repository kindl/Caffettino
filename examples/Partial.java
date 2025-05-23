import java.util.function.Consumer;
import java.nio.file.LinkOption;
import java.lang.reflect.Array;

public class Partial {
    public static <T> Runnable toRunnable(Consumer<T> consumer, T parameter) {
        return () -> consumer.accept(parameter);
    }

    public static void forever(Runnable r) {
        while (true) {
            r.run();
        }
    }
}