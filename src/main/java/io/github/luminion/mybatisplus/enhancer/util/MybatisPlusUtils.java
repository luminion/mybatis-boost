package io.github.luminion.mybatisplus.enhancer.util;

import io.github.luminion.mybatisplus.enhancer.core.SFunction;

import java.util.LinkedHashMap;
import java.util.Map;

/**
 * @author luminion
 */
public abstract class MybatisPlusUtils {
    private static boolean IS_MYBATIS_PLUS_ENV;

    static {
        try {
            Class.forName("com.baomidou.mybatisplus.core.mapper.BaseMapper");
            IS_MYBATIS_PLUS_ENV = true;
        } catch (ClassNotFoundException e) {
            IS_MYBATIS_PLUS_ENV = false;
        }
    }

    public static void isMybatisPlusEnv(boolean b) {
        IS_MYBATIS_PLUS_ENV = b;
    }

    public static boolean isMybatisPlusEnv() {
        return IS_MYBATIS_PLUS_ENV;
    }

    /**
     * 获取指定类对应的表名
     *
     * @param clazz 实体类
     * @return {@link String} 表名称
     */
    public static String getEntityTableName(Class<?> clazz) {
       return null;
    }
    
    
    /**
     * 获取ID字段属性名
     *
     * @param clazz 实体类
     * @return {@link String} ID字段属性名
     */
    public static String getIdPropertyName(Class<?> clazz) {
//        TableInfo tableInfo = TableInfoHelper.getTableInfo(clazz);
//        if (tableInfo != null) {
//            tableInfo.getKeyProperty();
//        }
        return null;
    }

    /**
     * 获取ID字段getter
     *
     * @param clazz 实体类
     * @return {@link String} ID字段getter
     */
    public static <T, R> SFunction<T, R> getIdPropertyGetter(Class<T> clazz) {
        // 完善该方法
        return null;
    }
    

    /**
     * 获取getter方法对应的字段名
     *
     * @param getter getter方法
     * @return {@link String} 字段名
     */
    public static <T, R> String getGetterPropertyName(SFunction<T, R> getter) {
//        return PropertyNamer.methodToProperty(LambdaUtils.extract(getter).getImplMethodName());
        return null;
    }


    /**
     * java字段到jdbc列映射
     *
     * @param entityClass 实体类
     * @return {@link Map } java字段到jdbc列映射
     */
    public static Map<String, String> javaFieldToJdbcColumnMap(Class<?> entityClass) {
        LinkedHashMap<String, String> map = new LinkedHashMap<>();
        return map;
    }
}
