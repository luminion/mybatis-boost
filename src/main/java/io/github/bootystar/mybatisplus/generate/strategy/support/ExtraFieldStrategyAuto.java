package io.github.bootystar.mybatisplus.generate.strategy.support;

import com.baomidou.mybatisplus.generator.config.po.TableField;
import io.github.bootystar.mybatisplus.enhance.enums.SqlKeyword;
import io.github.bootystar.mybatisplus.generate.strategy.ExtraFieldGenerateStrategy;

import java.util.Arrays;
import java.util.List;

/**
 * @author bootystar
 */
public class ExtraFieldStrategyAuto implements ExtraFieldGenerateStrategy {
    private static final List<String> ALLOW_COMPARE = Arrays.asList(
            "Byte"
            ,
            "Short"
            ,
            "Integer"
            ,
            "Long"
            ,
            "Float"
            ,
            "Double"
            ,
            "BigInteger"
            ,
            "BigDecimal"
            ,
            "Date"
            ,
            "Time"
            ,
            "Timestamp"
            ,
            "LocalDate"
            ,
            "LocalTime"
            ,
            "LocalDateTime"
    );
    private static final List<String> ALLOW_MULTI = Arrays.asList(
            "Byte"
            ,
            "Short"
            ,
            "Integer"
            ,
            "Long"
//            ,
//            "Float"
//            ,
//            "Double"
//            ,
//            "BigInteger"
//            ,
//            "BigDecimal"
//            ,
//            "Date"
//            ,
//            "Time"
//            ,
//            "Timestamp"
            ,
            "LocalDate"
//            ,
//            "LocalTime"
//            ,
//            "LocalDateTime"
    );

    public boolean allowGenerate(String sqlOperator, TableField tableField) {
        if (sqlOperator == null || sqlOperator.isEmpty() || tableField == null) {
            return false;
        }
        sqlOperator = sqlOperator.toUpperCase();
        String propertyType = tableField.getPropertyType();
//        int length = tableField.getMetaInfo().getLength();
//        boolean isShortString = isString && length > 0 && length <= 64;
        boolean isKeyFlag = tableField.isKeyFlag();
        boolean isNullable = tableField.getMetaInfo().isNullable();
        boolean isString = "String".equals(propertyType);
        boolean isIdColumn = tableField.getColumnName().endsWith("id");

        // 大小比较
        if (SqlKeyword.CONDITION_OPERATORS_COMPARE.contains(sqlOperator)) {
            return ALLOW_COMPARE.contains(propertyType)
                    && !isKeyFlag
                    && !isIdColumn
                    ;
        }

        // 模糊查询
        if (SqlKeyword.CONDITION_OPERATORS_LIKE.contains(sqlOperator)) {
            return isString;
        }

        // in查询
        if (SqlKeyword.CONDITION_OPERATORS_MULTI.contains(sqlOperator)) {
            return ALLOW_MULTI.contains(propertyType)
//                    || isShortString
                    || isKeyFlag
                    || isIdColumn
                    ;
        }

        // 是否为空
        if (SqlKeyword.CONDITION_OPERATORS_NONE.contains(sqlOperator)) {
            return isNullable
                    && !isKeyFlag
                    ;
        }

        return true;
    }

}
