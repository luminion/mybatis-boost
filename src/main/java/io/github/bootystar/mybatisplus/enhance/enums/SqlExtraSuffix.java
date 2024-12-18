package io.github.bootystar.mybatisplus.enhance.enums;

import lombok.AllArgsConstructor;

import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * @author bootystar
 */
@AllArgsConstructor
public enum SqlExtraSuffix {

    //    EQ("Eq", SqlKeyword.EQ),
//    NE("Ne", SqlKeyword.NE),
    NE("Ne", SqlKeyword.NE2),// 使用!=便于调用者理解
    GT("Gt", SqlKeyword.GT),
    GE("Ge", SqlKeyword.GE),
    LT("Lt", SqlKeyword.LT),
    LE("Le", SqlKeyword.LE),
    LIKE("Like", SqlKeyword.LIKE),
    NOT_LIKE("NotLike", SqlKeyword.NOT_LIKE),

    IN("In", SqlKeyword.IN),
    NOT_IN("NotIn", SqlKeyword.NOT_IN),

    IS_NULL("IsNull", SqlKeyword.IS_NULL),
    IS_NOT_NULL("IsNotNull", SqlKeyword.IS_NOT_NULL);

    public final String suffix;
    public final SqlKeyword sqlKeyword;
    public static final Map<String, String> DEFAULT_MAP;

    static {
        LinkedHashMap<String, String> map = new LinkedHashMap<>();
        for (SqlExtraSuffix value : values()) {
            map.put(value.suffix, value.sqlKeyword.keyword);
        }
        DEFAULT_MAP = Collections.unmodifiableMap(map);
    }

}
