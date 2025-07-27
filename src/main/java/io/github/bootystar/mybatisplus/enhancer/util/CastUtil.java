package io.github.bootystar.mybatisplus.enhancer.util;

/**
 * @author bootystar
 */
public class CastUtil {

    @SuppressWarnings("unchecked")
    public static <T> T cast(Object obj, Class<T> clazz) {
        boolean b = clazz.isAssignableFrom(obj.getClass());
        if (!b) {
            throw new IllegalArgumentException("obj is not a " + clazz.getName());
        }
        return (T) obj;
    }

}
