package io.github.bootystar.mybatisplus.enhancer.util;

import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;

import java.io.InputStream;
import java.io.OutputStream;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

/**
 * Excel工具类
 * <p>
 * 提供Excel导入导出功能，支持FastExcel和EasyExcel两种实现
 *
 * @author bootystar
 */
@Slf4j
public abstract class ExcelUtil {
    /**
     * Excel库基础包名
     */
    public static String excelBasePackage = "cn.idev.excel";
    /**
     * Excel类名
     */
    public static String excelClassName = "FastExcel";

    /**
     * 使用FastExcel库
     */
    public static void userFastExcel() {
        excelBasePackage = "cn.idev.excel";
        excelClassName = "FastExcel";
    }

    /**
     * 使用EasyExcel库
     */
    public static void userEasyExcel() {
        excelBasePackage = "com.alibaba.excel";
        excelClassName = "EasyExcel";
    }

    /**
     * 获取Excel类
     *
     * @return {@link Class} Excel类
     * @throws ClassNotFoundException 当找不到Excel类时抛出
     */
    @SneakyThrows
    public static Class<?> excelClass() {
        return Class.forName(excelBasePackage + "." + excelClassName);
    }


    /**
     * 写入Excel文件
     *
     * @param os            输出流
     * @param clazz         数据类
     * @param dataList      数据列表
     * @param includeFields 包含的字段
     * @throws Exception 当写入出现异常时抛出
     */
    @SneakyThrows
    public static void write(OutputStream os, Class<?> clazz, List<?> dataList, String... includeFields) {
//        EasyExcel.write(os, clazz)
//                .includeColumnFieldNames(Arrays.asList(includeFields))
//                .registerWriteHandler(new LongestMatchColumnWidthStyleStrategy())
//                .sheet()
//                .doWrite(dataList);
        Object invoke = excelClass().getMethod("write", OutputStream.class, Class.class).invoke(null, os, clazz);
        if (includeFields.length > 0) {
            invoke = invoke.getClass().getMethod("includeColumnFieldNames", Collection.class).invoke(invoke, Arrays.asList(includeFields));
        }
        Class<?> aClass = Class.forName(excelBasePackage + ".write.handler.WriteHandler");
        Object o = Class.forName(excelBasePackage + ".write.style.column.LongestMatchColumnWidthStyleStrategy").newInstance();
        invoke = invoke.getClass().getMethod("registerWriteHandler", aClass).invoke(invoke, o);
        invoke = invoke.getClass().getMethod("sheet").invoke(invoke);
        invoke.getClass().getMethod("doWrite", Collection.class).invoke(invoke, dataList);
    }

    /**
     * 读取Excel文件
     *
     * @param is    输入流
     * @param clazz 数据类
     * @param <T>   数据类型
     * @return {@link List} 数据列表
     * @throws Exception 当读取出现异常时抛出
     */
    @SuppressWarnings("unchecked")
    @SneakyThrows
    public static <T> List<T> read(InputStream is, Class<T> clazz) {
//        List<?> dataList = EasyExcel.read(is).head(clazz).sheet().doReadSync();
        Object invoke = excelClass().getMethod("read", InputStream.class).invoke(null, is);
        invoke = invoke.getClass().getMethod("head", Class.class).invoke(invoke, clazz);
        invoke = invoke.getClass().getMethod("sheet").invoke(invoke);
        return (List<T>) invoke.getClass().getMethod("doReadSync").invoke(invoke);
    }
}