package io.github.bootystar.mybatisplus.util;

import com.alibaba.excel.EasyExcel;
import com.alibaba.excel.converters.Converter;
import com.alibaba.excel.converters.DefaultConverterLoader;
import com.alibaba.excel.enums.CellDataTypeEnum;
import com.alibaba.excel.metadata.GlobalConfiguration;
import com.alibaba.excel.metadata.data.ReadCellData;
import com.alibaba.excel.metadata.data.WriteCellData;
import com.alibaba.excel.metadata.property.ExcelContentProperty;
import lombok.extern.slf4j.Slf4j;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.sql.Time;
import java.sql.Timestamp;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.format.DateTimeFormatter;

/**
 * @author bootystar
 */
@Slf4j
public abstract class ExcelHelper extends EasyExcel {
    private static final String WRITE_METHOD = "putWriteConverter";
    private static final String ALL_METHOD = "putAllConverter";

    static {
        addConverters(
                new LocalDateConverter(),
                new LocalDateTimeConverter(),
                new LocalTimeConverter(),
                new TimeConverter(),
                new TimestampConverter(),
                new LongConverter(),
                new DoubleConverter(),
                new BooleanConverter()
        );
    }

    private static Method getMethod(String writeMethod) {
        try {
            Class<DefaultConverterLoader> clazz = DefaultConverterLoader.class;
            Method method = clazz.getDeclaredMethod(writeMethod, Converter.class);
            method.setAccessible(true);
            return method;
        } catch (Exception e) {
            log.debug("error", e);
        }
        return null;
    }

    private static Method method4Converter2write() {
        return getMethod(WRITE_METHOD);
    }

    private static Method method4Converter2all() {
        return getMethod(ALL_METHOD);
    }

    public static void addConverters(Converter<?>... converters) {
        Method method = method4Converter2write();
        Method method2 = method4Converter2all();
        if (method == null || method2 == null) {
            log.warn("EasyExcel add excel converter failed , export or import may produce error on special field!");
            return;
        }
        try {
            for (Converter<?> converter : converters) {
                method.invoke(null, converter);
                method2.invoke(null, converter);
            }
        } catch (IllegalAccessException e) {
            log.warn("IllegalAccessException", e);
        } catch (InvocationTargetException e) {
            log.warn("InvocationTargetException", e);
        }
    }

    public static class LocalDateConverter implements Converter<LocalDate> {
        private static final DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");

        @Override
        public Class<LocalDate> supportJavaTypeKey() {
            return LocalDate.class;
        }

        @Override
        public CellDataTypeEnum supportExcelTypeKey() {
            return CellDataTypeEnum.STRING;
        }

        @Override
        public LocalDate convertToJavaData(ReadCellData cellData, ExcelContentProperty contentProperty, GlobalConfiguration globalConfiguration) {
            String cellValue = cellData.getStringValue();
            if (cellValue == null || cellValue.isEmpty()) {
                return null;
            }
            return LocalDate.parse(cellValue, formatter);
        }

        @Override
        public WriteCellData<String> convertToExcelData(LocalDate value, ExcelContentProperty contentProperty, GlobalConfiguration globalConfiguration) {
            if (value == null) {
                return new WriteCellData<>("");
            }
            return new WriteCellData<>(value.format(formatter));
        }
    }

    public static class LocalDateTimeConverter implements Converter<LocalDateTime> {
        private final DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");

        @Override
        public Class<LocalDateTime> supportJavaTypeKey() {
            return LocalDateTime.class;
        }

        @Override
        public CellDataTypeEnum supportExcelTypeKey() {
            return CellDataTypeEnum.STRING;
        }

        @Override
        public LocalDateTime convertToJavaData(ReadCellData cellData, ExcelContentProperty contentProperty, GlobalConfiguration globalConfiguration) {
            String cellValue = cellData.getStringValue();
            if (cellValue == null || cellValue.isEmpty()) {
                return null;
            }
            return LocalDateTime.parse(cellValue, formatter);
        }

        @Override
        public WriteCellData<String> convertToExcelData(LocalDateTime value, ExcelContentProperty contentProperty, GlobalConfiguration globalConfiguration) {
            if (value == null) {
                return new WriteCellData<>("");
            }
            return new WriteCellData<>(value.format(formatter));
        }

    }

    public static class LocalTimeConverter implements Converter<LocalTime> {
        private final DateTimeFormatter formatter = DateTimeFormatter.ofPattern("HH:mm:ss");

        @Override
        public Class<LocalTime> supportJavaTypeKey() {
            return LocalTime.class;
        }

        @Override
        public CellDataTypeEnum supportExcelTypeKey() {
            return CellDataTypeEnum.STRING;
        }

        @Override
        public LocalTime convertToJavaData(ReadCellData cellData, ExcelContentProperty contentProperty, GlobalConfiguration globalConfiguration) {
            String cellValue = cellData.getStringValue();
            if (cellValue == null || cellValue.isEmpty()) {
                return null;
            }
            return LocalTime.parse(cellValue, formatter);
        }

        @Override
        public WriteCellData<String> convertToExcelData(LocalTime value, ExcelContentProperty contentProperty, GlobalConfiguration globalConfiguration) {
            if (value == null) {
                return new WriteCellData<>("");
            }
            return new WriteCellData<>(value.format(formatter));
        }
    }

    public static class TimeConverter implements Converter<Time> {
        private final DateTimeFormatter formatter = DateTimeFormatter.ofPattern("HH:mm:ss");

        @Override
        public Class<Time> supportJavaTypeKey() {
            return Time.class;
        }

        @Override
        public CellDataTypeEnum supportExcelTypeKey() {
            return CellDataTypeEnum.STRING;
        }

        @Override
        public Time convertToJavaData(ReadCellData cellData, ExcelContentProperty contentProperty, GlobalConfiguration globalConfiguration) {
            String cellValue = cellData.getStringValue();
            if (cellValue == null || cellValue.isEmpty()) {
                return null;
            }
            LocalTime lt = LocalTime.parse(cellValue, formatter);
            return Time.valueOf(lt);
        }

        @Override
        public WriteCellData<String> convertToExcelData(Time value, ExcelContentProperty contentProperty, GlobalConfiguration globalConfiguration) {
            if (value == null) {
                return new WriteCellData<>("");
            }
            LocalTime lt = value.toLocalTime();
            return new WriteCellData<>(lt.format(formatter));
        }
    }


    public static class TimestampConverter implements Converter<Timestamp> {
        private final DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");

        @Override
        public Class<Timestamp> supportJavaTypeKey() {
            return Timestamp.class;
        }

        @Override
        public CellDataTypeEnum supportExcelTypeKey() {
            return CellDataTypeEnum.STRING;
        }

        @Override
        public Timestamp convertToJavaData(ReadCellData cellData, ExcelContentProperty contentProperty, GlobalConfiguration globalConfiguration) {
            String cellValue = cellData.getStringValue();
            if (cellValue == null || cellValue.isEmpty()) {
                return null;
            }
            LocalDateTime ldt = LocalDateTime.parse(cellValue, formatter);
            return Timestamp.valueOf(ldt);
        }

        @Override
        public WriteCellData<String> convertToExcelData(Timestamp value, ExcelContentProperty contentProperty, GlobalConfiguration globalConfiguration) {
            if (value == null) {
                return new WriteCellData<>("");
            }
            LocalDateTime ldt = value.toLocalDateTime();
            return new WriteCellData<>(ldt.format(formatter));
        }
    }

    public static class LongConverter implements Converter<Long> {

        @Override
        public Class<Long> supportJavaTypeKey() {
            return Long.class;
        }

        @Override
        public CellDataTypeEnum supportExcelTypeKey() {
            return CellDataTypeEnum.STRING;
        }

        @Override
        public Long convertToJavaData(ReadCellData cellData, ExcelContentProperty contentProperty, GlobalConfiguration globalConfiguration) {
            String cellValue = cellData.getStringValue();
            if (cellValue == null || cellValue.isEmpty()) {
                return null;
            }
            return Long.parseLong(cellValue);
        }

        @Override
        public WriteCellData<String> convertToExcelData(Long value, ExcelContentProperty contentProperty, GlobalConfiguration globalConfiguration) {
            if (value == null) {
                return new WriteCellData<>("");
            }
            return new WriteCellData<>(String.valueOf(value));
        }
    }

    public static class DoubleConverter implements Converter<Double> {

        @Override
        public Class<Double> supportJavaTypeKey() {
            return Double.class;
        }

        @Override
        public CellDataTypeEnum supportExcelTypeKey() {
            return CellDataTypeEnum.STRING;
        }

        @Override
        public Double convertToJavaData(ReadCellData cellData, ExcelContentProperty contentProperty, GlobalConfiguration globalConfiguration) {
            String cellValue = cellData.getStringValue();
            if (cellValue == null || cellValue.isEmpty()) {
                return null;
            }
            return Double.parseDouble(cellValue);
        }

        @Override
        public WriteCellData<String> convertToExcelData(Double value, ExcelContentProperty contentProperty, GlobalConfiguration globalConfiguration) {
            if (value == null) {
                return new WriteCellData<>("");
            }
            return new WriteCellData<>(String.valueOf(value));
        }
    }

    public static class BooleanConverter implements Converter<Boolean> {

        @Override
        public Class<Boolean> supportJavaTypeKey() {
            return Boolean.class;
        }

        @Override
        public CellDataTypeEnum supportExcelTypeKey() {
            return CellDataTypeEnum.STRING;
        }

        @Override
        public Boolean convertToJavaData(ReadCellData cellData, ExcelContentProperty contentProperty, GlobalConfiguration globalConfiguration) {
            String cellValue = cellData.getStringValue();
            if (cellValue == null || cellValue.isEmpty()) {
                return null;
            }
            return "是".equals(cellValue);
        }

        @Override
        public WriteCellData<String> convertToExcelData(Boolean value, ExcelContentProperty contentProperty, GlobalConfiguration globalConfiguration) {
            if (value == null) {
                return new WriteCellData<>("");
            }
            return new WriteCellData<>(value ? "是" : "否");
        }
    }
}
