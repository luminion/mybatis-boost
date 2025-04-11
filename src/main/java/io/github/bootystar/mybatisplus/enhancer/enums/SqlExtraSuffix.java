package io.github.bootystar.mybatisplus.enhancer.enums;

import lombok.AllArgsConstructor;

import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * @author bootystar
 */
@AllArgsConstructor
public enum SqlExtraSuffix {

    //    EQ("Eq", SqlKeyword.EQ),
//    NE("Ne", SqlKeyword.NE),
    NE("Ne", SqlKeyword.NE),
    LT("Lt", SqlKeyword.LT),
    LE("Le", SqlKeyword.LE),
    GT("Gt", SqlKeyword.GT),
    GE("Ge", SqlKeyword.GE),
    LIKE("Like", SqlKeyword.LIKE),
    NOT_LIKE("NotLike", SqlKeyword.NOT_LIKE),

    IN("In", SqlKeyword.IN),
    NOT_IN("NotIn", SqlKeyword.NOT_IN),

    IS_NULL("IsNull", SqlKeyword.IS_NULL),
    IS_NOT_NULL("IsNotNull", SqlKeyword.IS_NOT_NULL);

    public final String suffix;
    public final SqlKeyword sqlKeyword;
    public static final Map<String, String> DEFAULT_COMPLETE_MAP;
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
