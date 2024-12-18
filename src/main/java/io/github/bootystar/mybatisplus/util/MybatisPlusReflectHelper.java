package io.github.bootystar.mybatisplus.util;

import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableLogic;
import com.baomidou.mybatisplus.core.metadata.TableFieldInfo;
import com.baomidou.mybatisplus.core.metadata.TableInfo;
import com.baomidou.mybatisplus.core.metadata.TableInfoHelper;
import com.baomidou.mybatisplus.core.toolkit.reflect.GenericTypeUtils;
import com.baomidou.mybatisplus.core.toolkit.support.SFunction;
import io.github.bootystar.mybatisplus.enhance.core.DynamicEntity;
import io.github.bootystar.mybatisplus.generate.info.MethodInfo;
import lombok.SneakyThrows;

import java.lang.invoke.SerializedLambda;
import java.lang.reflect.*;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * 针对mybatis-plus增强的反射工具类
 *
 * @author bootystar
 */
public abstract class MybatisPlusReflectHelper extends ReflectHelper {

    private static final Map<Class<?>, Map<String, String>> FIELD_TO_JDBC_COLUMN_CACHE_MAP = new ConcurrentHashMap<>();

    /**
     * lambda方法信息
     *
     * @param methodReference lambda方法引用
     * @param parameterClass  参数类型
     * @return {@link MethodInfo }
     * @author bootystar
     */
    public static MethodInfo lambdaMethodInfo(SFunction<?, ?> methodReference, Class<?> parameterClass) {
        String methodName = "", className = "";
        try {
            Method lambdaMethod = methodReference.getClass().getDeclaredMethod("writeReplace");
            lambdaMethod.setAccessible(Boolean.TRUE);
            SerializedLambda serializedLambda = (SerializedLambda) lambdaMethod.invoke(methodReference);
            className = serializedLambda.getImplClass().replace("/", ".");
            methodName = serializedLambda.getImplMethodName();
            Class<?> methodClass = Class.forName(className);
            try {
                Method returnMethod = methodClass.getMethod(methodName, parameterClass);
                Class<?> returnType = returnMethod.getReturnType();
                int modifiers = returnMethod.getModifiers();
                if (!returnType.equals(methodClass) || !Modifier.isPublic(modifiers)) {
                    throw new NoSuchMethodException("no public method found which return instance of class itself");
                }
                return new MethodInfo(returnMethod);
            } catch (Exception e) {
                Constructor<?> constructor = methodClass.getConstructor(parameterClass);
                return new MethodInfo(constructor);
            }
        } catch (Exception e) {
            String msg = String.format("can't find constructor or method with parameter[%s] source:%s.%s() ", parameterClass.getName(), className, methodName);
            throw new IllegalStateException(msg);
        }
    }

    /**
     * 解析超类泛型参数
     *
     * @param clazz      指定类
     * @param superClass 超类
     * @return {@link Class }
     * @author bootystar
     */
    public static Class<?>[] resolveTypeArguments(Class<?> clazz, Class<?> superClass) {
        return GenericTypeUtils.resolveTypeArguments(clazz, superClass);
    }

    /**
     * id字段属性名
     *
     * @param clazz 克拉兹
     * @return {@link String }
     * @author bootystar
     */
    public static String idFieldPropertyName(Class<?> clazz) {
        TableInfo tableInfo = TableInfoHelper.getTableInfo(clazz);
        if (tableInfo == null) {
            return null;
        }
        return tableInfo.getKeyProperty();
    }

    /**
     * 从mybatis plus获取实体类属性与数据库字段转换映射
     *
     * @param clazz 实体类
     * @return {@link Map }<{@link String }, {@link String }>
     * @author bootystar
     */
    public static Map<String, String> filed2JdbcColumnByMybatisPlusTableInfo(Class<?> clazz) {
        TableInfo tableInfo = TableInfoHelper.getTableInfo(clazz);
        List<TableFieldInfo> fieldList = tableInfo.getFieldList();
        Map<String, String> result = new HashMap<>();
        for (TableFieldInfo fieldInfo : fieldList) {
            Field field = fieldInfo.getField();
            String fieldName = field.getName();
            String jdbcColumn = fieldInfo.getColumn();
            result.put(fieldName, jdbcColumn);
        }
        TableFieldInfo logicDeleteFieldInfo = tableInfo.getLogicDeleteFieldInfo();
        if (logicDeleteFieldInfo != null) {
            String name = logicDeleteFieldInfo.getField().getName();
            result.remove(name);
        }
        return result;
    }

    /**
     * 实体类与数据库字段转换映射
     *
     * @param clazz 克拉兹
     * @return {@link Map }<{@link String },{@link String }>
     * @author bootystar
     */
    public static Map<String, String> field2JdbcColumnByMybatisPlusAnnotation(Class<?> clazz) {
        HashMap<String, String> result = new HashMap<>();
        Map<String, Field> fieldMap = fieldMap(clazz);
        for (Field field : fieldMap.values()) {
            String fieldName = field.getName();
            TableLogic tableLogic = field.getAnnotation(TableLogic.class);
            if (tableLogic != null) {
                continue;
            }
            TableId tableId = field.getAnnotation(TableId.class);
            if (tableId != null) {
                String value = tableId.value();
                if (!value.isEmpty()) {
                    result.putIfAbsent(fieldName, value);
                }
                continue;
            }
            TableField tableField = field.getAnnotation(TableField.class);
            if (tableField != null) {
//                boolean exist = tableField.exist();
                String value = tableField.value();
                if (value != null) {
                    result.putIfAbsent(fieldName, value);
                }
            }
            // 无注解字段不处理
//            result.putIfAbsent(fieldName, jdbcColumn);
        }
        return result;
    }

    @SneakyThrows
    public static Map<String, String> field2JdbcColumnMapByEnhanceEntity(Class<?> entityClass) {
        if (DynamicEntity.class.isAssignableFrom(entityClass)) {
            DynamicEntity enhanceEntity = (DynamicEntity) entityClass.getConstructor().newInstance();
            return enhanceEntity.extraFieldColumnMap();
        }
        return new HashMap<>();
    }


    /**
     * 获取实体类属性与数据库字段的映射关系
     * 包含:
     * 1.mybatis-plus实体类属性与字段映射信息
     * 2.mybatis-plus注解指定的映射信息
     * 3.实现了EnhanceEntity接口的映射信息
     *
     * @param entityClass 实体类
     * @return {@link Map }<{@link String }, {@link String }>
     * @author bootystar
     */
    public static Map<String, String> field2JdbcColumnMap(Class<?> entityClass) {
        Map<String, String> map = FIELD_TO_JDBC_COLUMN_CACHE_MAP.get(entityClass);
        if (map != null) {
            return map;
        }
        Map<String, String> result = field2JdbcColumnMap(entityClass, "a.%s", ".");
        FIELD_TO_JDBC_COLUMN_CACHE_MAP.put(entityClass, result);
        return result;
    }

    /**
     * 获取实体类属性与数据库字段的映射关系
     *
     * @param entityClass  实体类
     * @param columnFormat 数据库字段映射格式{@link String#format(String, Object...)}
     * @param ignoreFormat 当字段名包含该值时,不进行字段映射
     * @return {@link Map }<{@link String }, {@link String }>
     * @author bootystar
     */
    public static Map<String, String> field2JdbcColumnMap(Class<?> entityClass, String columnFormat, String ignoreFormat) {
        String format = columnFormat == null || columnFormat.isEmpty() ? "%s" : columnFormat;
        LinkedHashMap<String, String> result = new LinkedHashMap<>();
        Map<String, String> tableInfoMap = filed2JdbcColumnByMybatisPlusTableInfo(entityClass);
        Map<String, String> annotationMap = field2JdbcColumnByMybatisPlusAnnotation(entityClass);
        Map<String, String> enhanceEntityMap = field2JdbcColumnMapByEnhanceEntity(entityClass);
        // 表信息优先
        tableInfoMap.forEach((key, value) -> {
            if (ignoreFormat != null && value.contains(ignoreFormat)) {
                result.putIfAbsent(key, value);
            } else {
                result.putIfAbsent(key, String.format(format, value));
            }
        });
        annotationMap.forEach((key, value) -> {
            if (ignoreFormat != null && value.contains(ignoreFormat)) {
                result.putIfAbsent(key, value);
            } else {
                result.putIfAbsent(key, String.format(format, value));
            }
        });
        enhanceEntityMap.forEach((key, value) -> {
            if (ignoreFormat != null && value.contains(ignoreFormat)) {
                result.putIfAbsent(key, value);
            } else {
                result.putIfAbsent(key, String.format(format, value));
            }
        });
        return result;
    }


}
