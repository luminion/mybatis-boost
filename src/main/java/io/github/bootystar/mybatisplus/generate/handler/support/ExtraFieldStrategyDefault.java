package io.github.bootystar.mybatisplus.generate.handler.support;

import com.baomidou.mybatisplus.generator.config.po.TableField;
import io.github.bootystar.mybatisplus.enhance.enums.SqlKeyword;
import io.github.bootystar.mybatisplus.generate.handler.ExtraFieldStrategy;

import java.util.Arrays;
import java.util.List;

/**
 * @author bootystar
 */
public class ExtraFieldStrategyDefault implements ExtraFieldStrategy {
    private static final List<String> ALLOW_COMPARE = Arrays.asList(
            "Byte",
            "Short",
            "Integer",
            "Long",
            "Float",
            "Double",
            "BigInteger",
            "BigDecimal",
            "Date",
            "Time",
            "Timestamp",
            "LocalDate",
            "LocalTime",
            "LocalDateTime"
    );

    public boolean allowGenerate(String keyword, TableField field) {
        if (keyword == null || keyword.isEmpty() || field == null) {
            return false;
        }
        keyword = keyword.toUpperCase();
        String propertyType = field.getPropertyType();
        int length = field.getMetaInfo().getLength();
        boolean isKeyFlag = field.isKeyFlag();
        boolean isNullable = field.getMetaInfo().isNullable();
        boolean isString = "String".equals(propertyType);
        boolean isShortString = isString && length > 0 && length <= 64;

        // 大小比较
        if (SqlKeyword.CONDITION_OPERATORS_COMPARE.contains(keyword)) {
            return ALLOW_COMPARE.contains(propertyType) || isShortString;
        }

        // 模糊查询
        if (SqlKeyword.CONDITION_OPERATORS_LIKE.contains(keyword)) {
            return isString;
        }

        // in查询
        if (SqlKeyword.CONDITION_OPERATORS_MULTI.contains(keyword)) {
            return ALLOW_COMPARE.contains(propertyType) || isShortString || isKeyFlag;
        }

        // 是否为空
        if (SqlKeyword.CONDITION_OPERATORS_NONE.contains(keyword)) {
            return isNullable || isKeyFlag;
        }

        return false;
    }

}
