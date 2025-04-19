package io.github.bootystar.mybatisplus.enhancer.util;

import com.baomidou.mybatisplus.annotation.TableName;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.toolkit.LambdaUtils;
import com.baomidou.mybatisplus.core.toolkit.support.SFunction;
import com.baomidou.mybatisplus.extension.service.IService;
import io.github.bootystar.mybatisplus.enhancer.core.base.EnhancedQuery;
import org.apache.ibatis.reflection.property.PropertyNamer;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.atomic.AtomicReference;

/**
 * @author bootystar
 */
public abstract class MapperHelper {

    public static <T> String getXmlContent(Class<? extends EnhancedQuery<?>> enhancedQueryClass) {
        return getXmlContent(enhancedQueryClass, new HashMap<>());
    }

    @SuppressWarnings("unchecked")
    public static <T> String getXmlContent(Class<? extends EnhancedQuery<?>> enhancedQueryClass, Map<SFunction<T, ?>, Boolean> sortMap) {
        Class<?> voClass = MybatisPlusReflectHelper.resolveTypeArguments(enhancedQueryClass, EnhancedQuery.class)[0];
        Class<?>[] mapperClasses = MybatisPlusReflectHelper.resolveTypeArguments(enhancedQueryClass, BaseMapper.class);
        Class<?>[] serviceClasses = MybatisPlusReflectHelper.resolveTypeArguments(enhancedQueryClass, IService.class);
        Class<T> entityClass = null;
        if (mapperClasses != null && mapperClasses.length > 0) {
            entityClass = (Class<T>) mapperClasses[0];
        } else if (serviceClasses != null && serviceClasses.length > 0) {
            entityClass = (Class<T>) serviceClasses[0];
        } else {
            throw new IllegalArgumentException("no base entity info in " + enhancedQueryClass.getName());
        }
        return getXmlContent(entityClass, voClass, sortMap);
    }

    public static String getXmlContent(Class<?> entityClass, Class<?> voClass) {
        return getXmlContent(entityClass, voClass, null);
    }

    public static <T> String getXmlContent(Class<T> entityClass, Class<?> voClass, Map<SFunction<T, ?>, Boolean> sortMap) {
        AtomicReference<String> orderBySqlRef = new AtomicReference<>("");
        if (sortMap != null && !sortMap.isEmpty()) {
            sortMap.entrySet().stream().map(e -> {
                SFunction<T, ?> key = e.getKey();
                String columnName = PropertyNamer.methodToProperty(LambdaUtils.extract(key).getImplMethodName());
                return String.format("a.%s%s", columnName, e.getValue() ? " DESC" : "");
            }).reduce((e1, e2) -> e1 + ", " + e2).ifPresent(orderBySqlRef::set);
        }
        String orderBySql = orderBySqlRef.get();
        if (!orderBySql.isEmpty()) {
            orderBySql = "            , " + orderBySql + "\n";
        }
        String tableName;
        TableName annotation = entityClass.getAnnotation(TableName.class);
        if (annotation != null && !annotation.value().isEmpty()) {
            tableName = annotation.value();
        } else {
            tableName = entityClass.getName();
        }
        return
//                "    <sql id=\"selectFragment\">\n" +
//                "        <if test=\"param1 != null\">\n" +
//                "            <foreach collection=\"param1\" item=\"gen\">\n" +
//                "                <if test=\"gen.conditions != null and gen.conditions.size() > 0\">\n" +
//                "                    <trim prefix=\"AND ( \" suffix=\" )\" prefixOverrides=\"AND|OR\">\n" +
//                "                        <foreach collection=\"gen.conditions\" item=\"item\">\n" +
//                "                            <choose>\n" +
//                "                                <when test=\"item.isOr()\">\n" +
//                "                                    OR\n" +
//                "                                </when>\n" +
//                "                                <otherwise>\n" +
//                "                                    AND\n" +
//                "                                </otherwise>\n" +
//                "                            </choose>\n" +
//                "                            <choose>\n" +
//                "                                <when test=\"item.operator=='IN' or item.operator=='NOT IN'\">\n" +
//                "                                    ${item.field} ${item.operator}\n" +
//                "                                    <foreach collection=\"item.value\" item=\"val\" separator=\",\" open=\"(\" close=\")\">\n" +
//                "                                        #{val}\n" +
//                "                                    </foreach>\n" +
//                "                                </when>\n" +
//                "                                <when test=\"item.operator=='IS NULL' or item.operator=='IS NOT NULL'\">\n" +
//                "                                    ${item.field} ${item.operator}\n" +
//                "                                </when>\n" +
//                "                                <otherwise>\n" +
//                "                                    ${item.field} ${item.operator} #{item.value}\n" +
//                "                                </otherwise>\n" +
//                "                            </choose>\n" +
//                "                        </foreach>\n" +
//                "                    </trim>\n" +
//                "                </if>\n" +
//                "            </foreach>\n" +
//                "        </if>\n" +
//                "    </sql>\n" +
//                "\n" +
//                "    <sql id=\"sortFragment\">\n" +
//                "        <if test=\"param1 != null and param1.sorts != null and param1.sorts.size() > 0\">\n" +
//                "            <foreach collection=\"param1.sorts\" item=\"item\" separator=\",\" close=\",\">\n" +
//                "                ${item.field}\n" +
//                "                <if test=\"item.isDesc()\">\n" +
//                "                    DESC\n" +
//                "                </if>\n" +
//                "            </foreach>\n" +
//                "        </if>\n" +
//                "    </sql>\n" +
//                "\n" +
                "    <select id=\"voSelectByXml\" resultType=\"" + voClass.getName() + "\">\n" +
                "        SELECT\n" +
                "        a.*\n" +
                "        FROM\n" +
                "        " + tableName + " a\n" +
                "        <where>\n" +
                "            <include refid=\"io.github.bootystar.mybatisplus.enhancer.core.DynamicMapper.selectFragment\"/>\n" +
//                "            AND a.deleted = 0\n" + // 逻辑删除
                "        </where>\n" +
                "        <trim prefix=\"ORDER BY\" prefixOverrides=\",\">\n" +
                "            <include refid=\"io.github.bootystar.mybatisplus.enhancer.core.DynamicMapper.sortFragment\"/>\n" +
                orderBySql +  // 默认排序
                "        </trim>\n" +
                "    </select>"
                ;
    }


}
