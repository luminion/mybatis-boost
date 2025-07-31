package io.github.bootystar.mybatisplus.enhancer.enums;

import lombok.AllArgsConstructor;

import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * SQL字段后缀枚举
 * <p>
 * 定义了常用的字段后缀及其对应的SQL操作符，用于动态SQL构建时根据字段后缀自动识别操作符
 *
 * @author bootystar
 */
@AllArgsConstructor
public enum SqlExtraSuffix {

    //    EQ("Eq", SqlKeyword.EQ),
//    NE("Ne", SqlKeyword.NE),
    /**
     * 不等于操作后缀
     */
    NE("Ne", SqlKeyword.NE),
    /**
     * 小于操作后缀
     */
    LT("Lt", SqlKeyword.LT),
    /**
     * 小于等于操作后缀
     */
    LE("Le", SqlKeyword.LE),
    /**
     * 大于操作后缀
     */
    GT("Gt", SqlKeyword.GT),
    /**
     * 大于等于操作后缀
     */
    GE("Ge", SqlKeyword.GE),
    /**
     * 模糊匹配操作后缀
     */
    LIKE("Like", SqlKeyword.LIKE),
    /**
     * 不模糊匹配操作后缀
     */
    NOT_LIKE("NotLike", SqlKeyword.NOT_LIKE),

    /**
     * IN操作后缀
     */
    IN("In", SqlKeyword.IN),
    /**
     * NOT IN操作后缀
     */
    NOT_IN("NotIn", SqlKeyword.NOT_IN),

    /**
     * IS NULL操作后缀
     */
    IS_NULL("IsNull", SqlKeyword.IS_NULL),
    /**
     * IS NOT NULL操作后缀
     */
    IS_NOT_NULL("IsNotNull", SqlKeyword.IS_NOT_NULL),

    /**
     * 位运算包含操作后缀
     */
    BITWISE_WITH("WithBit", SqlKeyword.BITWISE_WITH),
    /**
     * 位运算不包含操作后缀
     */
    BITWISE_WITHOUT("WithoutBit", SqlKeyword.BITWISE_WITHOUT),
    
    ;

    /**
     * 后缀名称
     */
    public final String suffix;
    /**
     * 对应的SQL关键字
     */
    public final SqlKeyword sqlKeyword;
    /**
     * 默认完整映射表
     */
    public static final Map<String, String> DEFAULT_COMPLETE_MAP;
    /**
     * 默认简单映射表
     */
    public static final Map<String, String> DEFAULT_SIMPLE_MAP;

    static {
        LinkedHashMap<String, String> map = new LinkedHashMap<>();
        for (SqlExtraSuffix value : values()) {
            map.put(value.suffix, value.sqlKeyword.keyword);
        }
        DEFAULT_COMPLETE_MAP = Collections.unmodifiableMap(map);

        LinkedHashMap<String, String> simpleMap = new LinkedHashMap<>();
        simpleMap.put(LIKE.suffix, LIKE.sqlKeyword.keyword);
        simpleMap.put(IN.suffix, IN.sqlKeyword.keyword);
        simpleMap.put(GE.suffix, GE.sqlKeyword.keyword);
        simpleMap.put(LE.suffix, LE.sqlKeyword.keyword);
        DEFAULT_SIMPLE_MAP = Collections.unmodifiableMap(simpleMap);
    }

}