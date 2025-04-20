package io.github.bootystar.mybatisplus.enhancer.util;

import com.baomidou.mybatisplus.annotation.TableName;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.extension.service.IService;
import io.github.bootystar.mybatisplus.enhancer.core.base.EnhancedQuery;
import lombok.extern.slf4j.Slf4j;
import org.apache.ibatis.builder.xml.XMLMapperBuilder;
import org.apache.ibatis.io.Resources;
import org.apache.ibatis.session.Configuration;
import org.apache.ibatis.session.SqlSessionFactory;

import java.io.IOException;
import java.io.InputStream;

/**
 * @author bootystar
 */
@Slf4j
public abstract class MapperHelper {

    public static boolean initSqlFragment(SqlSessionFactory sqlSessionFactory) {
        Configuration configuration = sqlSessionFactory.getConfiguration();
        String resource = "mapper/DynamicMapper.xml";
        try (InputStream inputStream = Resources.getResourceAsStream(resource)) {
            XMLMapperBuilder mapperBuilder = new XMLMapperBuilder(inputStream, configuration, resource, configuration.getSqlFragments());
            mapperBuilder.parse();
            return true;
        } catch (IOException e) {
            log.error("error creating DynamicMapper sqlFragments", e);
            return false;
        }
    }

    @SuppressWarnings("unchecked" )
    public static <T> String getMapperContent(Class<? extends EnhancedQuery<?>> enhancedQueryClass) {
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
        return getMapperContent(entityClass, voClass);
    }

    public static <T> String getMapperContent(Class<T> entityClass, Class<?> voClass) {
        return "    <select id=\"voSelectByXml\" resultType=\"" + voClass.getName() + "\">\n" +
                getSqlContent(entityClass) +
                "    </select>"
                ;
    }

    public static <T> String getSqlContent(Class<T> entityClass) {
        String tableName;
        TableName annotation = entityClass.getAnnotation(TableName.class);
        if (annotation != null && !annotation.value().isEmpty()) {
            tableName = annotation.value();
        } else {
            tableName = entityClass.getName();
        }
        return getSqlContent(tableName);
    }

    public static String getSqlContent(String tableName) {
        return "        SELECT\n" +
                "        a.*\n" +
                "        FROM\n" +
                "        " + tableName + " a\n" +
                "        <where>\n" +
                "            <include refid=\"io.github.bootystar.mybatisplus.enhancer.core.DynamicMapper.dynamicSelect\"/>\n" +
                "        </where>\n" +
                "        <trim prefix=\"ORDER BY\" prefixOverrides=\",\">\n" +
                "            <include refid=\"io.github.bootystar.mybatisplus.enhancer.core.DynamicMapper.dynamicSort\"/>\n" +
                "        </trim>\n"
                ;
    }

    public static String getDynamicSelectFragment() {
        return "    <sql id=\"dynamicSelect\">\n" +
                "        <if test=\"param1 != null\">\n" +
                "            <foreach collection=\"param1\" item=\"gen\">\n" +
                "                <if test=\"gen.conditions != null and gen.conditions.size() > 0\">\n" +
                "                    <trim prefix=\"AND (\" suffix=\")\" prefixOverrides=\"AND|OR\">\n" +
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
                "                                <when test=\"item.operator == 'IN' or item.operator == 'NOT IN'\">\n" +
                "                                    ${item.field} ${item.operator}\n" +
                "                                    <foreach collection=\"item.value\" item=\"val\" separator=\",\" open=\"(\" close=\")\">\n" +
                "                                        #{val}\n" +
                "                                    </foreach>\n" +
                "                                </when>\n" +
                "                                <when test=\"item.operator == 'IS NULL' or item.operator == 'IS NOT NULL'\">\n" +
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
                "    </sql>"
                ;
    }

    public static String getDynamicSortFragment() {
        return "    <sql id=\"dynamicSort\">\n" +
                "        <if test=\"param1 != null and param1.sorts != null and param1.sorts.size() > 0\">\n" +
                "            <foreach collection=\"param1.sorts\" item=\"item\" separator=\",\" open=\",\">\n" +
                "                ${item.field}\n" +
                "                <if test=\"item.isDesc()\">\n" +
                "                    DESC\n" +
                "                </if>\n" +
                "            </foreach>\n" +
                "        </if>\n" +
                "    </sql>"
                ;
    }

}
