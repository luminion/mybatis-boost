package io.github.bootystar.mybatisplus.enhancer.util;

import com.baomidou.mybatisplus.annotation.TableName;
import com.baomidou.mybatisplus.core.metadata.TableInfoHelper;
import com.baomidou.mybatisplus.core.toolkit.LambdaUtils;
import com.baomidou.mybatisplus.core.toolkit.support.SFunction;
import org.apache.ibatis.reflection.property.PropertyNamer;

import java.util.Map;
import java.util.concurrent.atomic.AtomicReference;

/**
 * @author bootystar
 */
public abstract class MapperHelper {

    public static String getXmlContent(Class<?> entityClass, Class<?> voClass) {
       return getXmlContent(entityClass,voClass,null);
    }

    public static <T> String getXmlContent(Class<T> entityClass, Class<?> voClass, Map<SFunction<T, ?>,Boolean> sortMap) {
        AtomicReference<String> orderBySqlRef = new AtomicReference<>("");
        if (sortMap!=null && !sortMap.isEmpty()) {
            sortMap.entrySet().stream().map(e->{
                SFunction<T, ?> key = e.getKey();
                String columnName = PropertyNamer.methodToProperty(LambdaUtils.extract(key).getImplMethodName());
                return String.format("a.%s%s", columnName, e.getValue() ? " DESC" : "" );
            }).reduce((e1, e2) -> e1 + " , " + e2).ifPresent(orderBySqlRef::set);
        }
        String orderBySql = orderBySqlRef.get();
        String tableName;
        TableName annotation = entityClass.getAnnotation(TableName.class);
        if (annotation != null && !annotation.value().isEmpty()) {
            tableName = annotation.value();
        } else {
            tableName = entityClass.getName();
        }
        return "    <sql id=\"selectFragment\">\n" +
                "        <if test=\"param1 != null\">\n" +
                "            <foreach collection=\"param1\" item=\"gen\">\n" +
                "                <if test=\"gen.conditions != null and gen.conditions.size() > 0\">\n" +
                "                    <trim prefix=\"AND ( \" suffix=\" )\" prefixOverrides=\"AND|OR\">\n" +
                "                        <foreach collection=\"gen.conditions\" item=\"item\">\n" +
                "                            <choose>\n" +
                "                                <when test=\"item.isOr()\">\n" +
                "                                    OR\n" +
                "                                </when>\n" +
                "                                <otherwise>\n" +
                "                                    AND\n" +
                "                                </otherwise>\n" +
                "                            </choose>\n" +
                "                            <choose>\n" +
                "                                <when test=\"item.operator=='IN' or item.operator=='NOT IN'\">\n" +
                "                                    ${item.field} ${item.operator}\n" +
                "                                    <foreach collection=\"item.value\" item=\"val\" separator=\",\" open=\"(\" close=\")\">\n" +
                "                                        #{val}\n" +
                "                                    </foreach>\n" +
                "                                </when>\n" +
                "                                <when test=\"item.operator=='IS NULL' or item.operator=='IS NOT NULL'\">\n" +
                "                                    ${item.field} ${item.operator}\n" +
                "                                </when>\n" +
                "                                <otherwise>\n" +
                "                                    ${item.field} ${item.operator} #{item.value}\n" +
                "                                </otherwise>\n" +
                "                            </choose>\n" +
                "                        </foreach>\n" +
                "                    </trim>\n" +
                "                </if>\n" +
                "            </foreach>\n" +
                "        </if>\n" +
                "    </sql>\n" +
                "\n" +
                "    <sql id=\"sortFragment\">\n" +
                "        <if test=\"param1 != null and param1.sorts != null and param1.sorts.size() > 0\">\n" +
                "            <foreach collection=\"param1.sorts\" item=\"item\" separator=\",\" close=\",\">\n" +
                "                ${item.field}\n" +
                "                <if test=\"item.isDesc()\">\n" +
                "                    DESC\n" +
                "                </if>\n" +
                "            </foreach>\n" +
                "        </if>\n" +
                "    </sql>\n" +
                "\n" +
                "    <select id=\"doSelectVO\" resultType=\"" + voClass.getName() + "\">\n" +
                "        SELECT\n" +
                "        a.*\n" +
                "        FROM\n" +
                "        " + tableName + " a\n" +
                "        <trim prefix=\"WHERE\" prefixOverrides=\"AND|OR\">\n" +
                "            <include refid=\"selectFragment\"/>\n" +
//                "            AND a.deleted = 0\n" + // 逻辑删除
                "        </trim>\n" +
                "        <trim prefix=\"ORDER BY\" suffixOverrides=\",\">\n" +
                "            <include refid=\"sortFragment\"/>\n" +
                orderBySql+
//                "            a.sort , a.create_time DESC , a.id DESC\n" + // 默认排序
                "        </trim>\n" +
                "    </select>"
                ;
    }

 

}
