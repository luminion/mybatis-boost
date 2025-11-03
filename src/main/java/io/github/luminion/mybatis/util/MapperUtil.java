package io.github.luminion.mybatis.util;

import io.github.luminion.mybatis.core.Booster;
import lombok.extern.slf4j.Slf4j;
import org.apache.ibatis.builder.xml.XMLMapperBuilder;
import org.apache.ibatis.io.Resources;
import org.apache.ibatis.session.Configuration;
import org.apache.ibatis.session.SqlSessionFactory;

import java.io.IOException;
import java.io.InputStream;

/**
 * Mapper工具类
 * <p>
 * 提供Mapper相关工具方法，包括SQL片段初始化和Mapper内容生成
 *
 * @author luminion
 */
@Slf4j
public abstract class MapperUtil {

    /**
     * 初始化SQL片段
     *
     * @param sqlSessionFactory SQL会话工厂
     * @return boolean 初始化是否成功
     */
    public static boolean initSqlFragment(SqlSessionFactory sqlSessionFactory) {
        Configuration configuration = sqlSessionFactory.getConfiguration();
        String resource = "luminion/mapper/BoostMapper.xml";
        try (InputStream inputStream = Resources.getResourceAsStream(resource)) {
            XMLMapperBuilder mapperBuilder = new XMLMapperBuilder(inputStream, configuration, resource, configuration.getSqlFragments());
            mapperBuilder.parse();
            return true;
        } catch (IOException e) {
            log.error("error creating sqlFragments", e);
            return false;
        }
    }

    /**
     * 获取Mapper内容
     *
     * @param boostClass 扩展查询类
     * @return {@link String} Mapper内容
     * @throws IllegalArgumentException 当无法解析实体信息时抛出
     */
    public static <T, V> String getMapperContent(Class<? extends Booster<T, V>> boostClass) {
        Class<?>[] classes = ReflectUtil.resolveTypeArguments(boostClass, Booster.class);
        return getMapperContent(classes[0], classes[1]);
    }

    /**
     * 获取Mapper内容
     *
     * @param entityClass 实体类
     * @param voClass     VO类
     * @param <T>         实体类型
     * @return {@link String} Mapper内容
     */
    public static <T> String getMapperContent(Class<T> entityClass, Class<?> voClass) {
        return "    <select id=\"selectBySqlEntity\" resultType=\"" + voClass.getName() + "\">\n" +
                getSqlContent(entityClass) +
                "    </select>"
                ;
    }

    /**
     * 获取SQL内容
     *
     * @param entityClass 实体类
     * @param <T>         实体类型
     * @return {@link String} SQL内容
     */
    public static <T> String getSqlContent(Class<T> entityClass) {
        return getSqlContent(BoostUtils.getEntityTableName(entityClass));
    }

    /**
     * 获取SQL内容
     *
     * @param tableName 表名
     * @return {@link String} SQL内容
     */
    public static String getSqlContent(String tableName) {
        return "        SELECT\n" +
                "        a.*\n" +
                "        FROM\n" +
                "        " + tableName + " a\n" +
                "        <where>\n" +
                "            <include refid=\"io.github.luminion.mybatis.core.BoostMapper.queryFragment\"/>\n" +
                "        </where>\n" +
                "        <trim prefix=\"ORDER BY\" prefixOverrides=\",\">\n" +
                "            <include refid=\"io.github.luminion.mybatis.core.BoostMapper.sortFragment\"/>\n" +
                "        </trim>\n"
                ;
    }


}