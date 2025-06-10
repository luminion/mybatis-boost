package io.github.bootystar.mybatisplus.enhancer.util;

import cn.idev.excel.EasyExcel;
import cn.idev.excel.write.style.column.LongestMatchColumnWidthStyleStrategy;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;

import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

/**
 * @author bootystar
 */
@Slf4j
public abstract class ExcelAdapter {
    public static String excelBasePackage = "cn.idev.excel";
    public static String excelClassName = "FastExcel";

    public static void userFastExcel() {
        excelBasePackage = "cn.idev.excel";
        excelClassName = "FastExcel";
    }

    public static void userEasyExcel() {
        excelBasePackage = "com.alibaba.excel";
        excelClassName = "EasyExcel";
    }

    @SneakyThrows
    public static Class<?> excelClass() {
        return Class.forName(excelBasePackage + "." + excelClassName);
    }


    @SneakyThrows
    public static void write(OutputStream os,  Class<?> clazz, List<?> dataList,String... includeFields) {
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
        invoke = invoke.getClass().getMethod("registerWriteHandler",aClass).invoke(invoke, o);
        invoke = invoke.getClass().getMethod("sheet").invoke(invoke);
        invoke.getClass().getMethod("doWrite",  Collection.class).invoke(invoke, dataList);
    }

    @SuppressWarnings("unchecked")
    @SneakyThrows
    public static <T> List<T> read(InputStream is, Class<T> clazz) {
//        List<?> dataList = EasyExcel.read(is).head(clazz).sheet().doReadSync();
        Object invoke = excelClass().getMethod("read",  InputStream.class).invoke(null, is);
        invoke = invoke.getClass().getMethod("head",  Class.class).invoke(invoke, clazz);
        invoke = invoke.getClass().getMethod("sheet").invoke(invoke);
        return (List<T>) invoke.getClass().getMethod("doReadSync").invoke(invoke);
    }
}
