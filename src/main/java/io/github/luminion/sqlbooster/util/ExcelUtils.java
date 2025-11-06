package io.github.luminion.sqlbooster.util;

import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;

import java.io.InputStream;
import java.io.OutputStream;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

/**
 * Excel 工具类.
 * <p>
 * 提供对 Excel 导入和导出功能的抽象封装, 支持通过反射动态切换 FastExcel 和 EasyExcel 等底层实现.
 *
 * @author luminion
 * @since 1.0.0
 */
@Slf4j
public abstract class ExcelUtils {
    /**
     * 底层 Excel 库的基础包名.
     * @since 1.0.0
     */
    public static String excelBasePackage = "cn.idev.excel";
    /**
     * 底层 Excel 库的入口类名.
     * @since 1.0.0
     */
    public static String excelClassName = "FastExcel";

    /**
     * 切换底层实现为 FastExcel.
     * @since 1.0.0
     */
    public static void userFastExcel() {
        excelBasePackage = "cn.idev.excel";
        excelClassName = "FastExcel";
    }

    /**
     * 切换底层实现为 EasyExcel.
     * @since 1.0.0
     */
    public static void userEasyExcel() {
        excelBasePackage = "com.alibaba.excel";
        excelClassName = "EasyExcel";
    }

    /**
     * 根据当前配置获取底层 Excel 库的入口 Class 对象.
     *
     * @return {@link Class} Excel类
     * @since 1.0.0
     */
    @SneakyThrows
    public static Class<?> excelClass() {
        return Class.forName(excelBasePackage + "." + excelClassName);
    }


    /**
     * 将数据列表写入到 Excel 输出流.
     *
     * @param os            输出流
     * @param clazz         数据类
     * @param dataList      数据列表
     * @param includeFields 要包含的字段
     * @since 1.0.0
     */
    @SneakyThrows
    public static void write(OutputStream os, Class<?> clazz, List<?> dataList, String... includeFields) {
        if (os == null) {
            throw new IllegalArgumentException("OutputStream cannot be null");
        }
        if (clazz == null) {
            throw new IllegalArgumentException("Class cannot be null");
        }
        if (dataList == null) {
            dataList = Collections.emptyList();
        }

        try {
            Object invoke = excelClass().getMethod("write", OutputStream.class, Class.class).invoke(null, os, clazz);
            if (includeFields != null && includeFields.length > 0) {
                invoke = invoke.getClass().getMethod("includeColumnFieldNames", Collection.class).invoke(invoke, Arrays.asList(includeFields));
            }
            Class<?> aClass = Class.forName(excelBasePackage + ".write.handler.WriteHandler");
            Object o = Class.forName(excelBasePackage + ".write.style.column.LongestMatchColumnWidthStyleStrategy").newInstance();
            invoke = invoke.getClass().getMethod("registerWriteHandler", aClass).invoke(invoke, o);
            invoke = invoke.getClass().getMethod("sheet").invoke(invoke);
            invoke.getClass().getMethod("doWrite", Collection.class).invoke(invoke, dataList);
        } catch (Exception e) {
            log.error("Failed to write Excel file", e);
            throw new RuntimeException("Excel write operation failed", e);
        }
    }

    /**
     * 从 Excel 输入流中读取数据并转换为指定类型的列表.
     *
     * @param is    输入流
     * @param clazz 数据类
     * @param <T>   数据类型
     * @return {@link List} 数据列表
     * @since 1.0.0
     */
    @SuppressWarnings("unchecked")
    @SneakyThrows
    public static <T> List<T> read(InputStream is, Class<T> clazz) {
        if (is == null) {
            throw new IllegalArgumentException("InputStream cannot be null");
        }
        if (clazz == null) {
            throw new IllegalArgumentException("Class cannot be null");
        }

        try {
            Object invoke = excelClass().getMethod("read", InputStream.class).invoke(null, is);
            invoke = invoke.getClass().getMethod("head", Class.class).invoke(invoke, clazz);
            invoke = invoke.getClass().getMethod("sheet").invoke(invoke);
            Object result = invoke.getClass().getMethod("doReadSync").invoke(invoke);
            return result != null ? (List<T>) result : Collections.emptyList();
        } catch (Exception e) {
            log.error("Failed to read Excel file", e);
            throw new RuntimeException("Excel read operation failed", e);
        }
    }
}