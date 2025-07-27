package io.github.bootystar.mybatisplus.enhancer.util;

import com.baomidou.mybatisplus.annotation.TableName;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.extension.service.IService;
import io.github.bootystar.mybatisplus.enhancer.EnhancedQuery;
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
public abstract class MapperUtil {

    public static boolean initSqlFragment(SqlSessionFactory sqlSessionFactory) {
        Configuration configuration = sqlSessionFactory.getConfiguration();
        String resource = "bootystar/mapper/DynamicMapper.xml";
        try (InputStream inputStream = Resources.getResourceAsStream(resource)) {
            XMLMapperBuilder mapperBuilder = new XMLMapperBuilder(inputStream, configuration, resource, configuration.getSqlFragments());
            mapperBuilder.parse();
            return true;
        } catch (IOException e) {
            log.error("error creating DynamicMapper sqlFragments", e);
            return false;
        }
    }

    @SuppressWarnings("unchecked")
    public static <T> String getMapperContent(Class<? extends EnhancedQuery<?>> enhancedQueryClass) {
        Class<?> voClass = MybatisPlusReflectUtil.resolveTypeArguments(enhancedQueryClass, EnhancedQuery.class)[0];
        Class<?>[] mapperClasses = MybatisPlusReflectUtil.resolveTypeArguments(enhancedQueryClass, BaseMapper.class);
        Class<?>[] serviceClasses = MybatisPlusReflectUtil.resolveTypeArguments(enhancedQueryClass, IService.class);
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
                "            <include refid=\"io.github.bootystar.mybatisplus.enhancer.DynamicMapper.dynamicSelect\"/>\n" +
                "        </where>\n" +
                "        <trim prefix=\"ORDER BY\" prefixOverrides=\",\">\n" +
                "            <include refid=\"io.github.bootystar.mybatisplus.enhancer.DynamicMapper.dynamicSort\"/>\n" +
                "        </trim>\n"
                ;
    }


}
