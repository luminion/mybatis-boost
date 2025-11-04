package io.github.luminion.mybatis.config;

import io.github.luminion.mybatis.provider.support.BasicProvider;
import io.github.luminion.mybatis.util.BoostUtils;
import io.github.luminion.mybatis.util.MapperUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.ibatis.session.Configuration;
import org.apache.ibatis.session.SqlSessionFactory;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.boot.autoconfigure.AutoConfiguration;
import org.springframework.boot.autoconfigure.condition.ConditionalOnBean;
import org.springframework.context.ApplicationContext;

import java.util.Map;

/**
 * Mybatis-Boost 自动配置类.
 * <p>
 * 使用 {@link ApplicationRunner} 确保在所有Bean初始化完成后再执行SQL片段的配置.
 * 使用 {@link ConditionalOnBean} 确保仅在存在 {@link SqlSessionFactory} Bean时才激活此配置.
 *
 * @author luminion
 * @since 1.0.0
 */
@Slf4j
@AutoConfiguration
@ConditionalOnBean(SqlSessionFactory.class)
public class BoostAutoConfiguration implements ApplicationRunner {

    private final ApplicationContext applicationContext;

    /**
     * 构造函数,注入 {@link ApplicationContext}.
     *
     * @param applicationContext Spring应用上下文
     * @since 1.0.0
     */
    public BoostAutoConfiguration(ApplicationContext applicationContext) {
        this.applicationContext = applicationContext;
    }

    /**
     * 在 Spring Boot 应用启动后执行.
     * <p>
     * 遍历所有的 {@link SqlSessionFactory} beans, 并为它们初始化SQL片段.
     * 同时, 注册 {@link BasicProvider} 到 {@link BoostUtils}.
     *
     * @param args 应用程序参数
     * @since 1.0.0
     */
    @Override
    public void run(ApplicationArguments args) throws Exception {
        Map<String, SqlSessionFactory> sqlSessionFactoryMap = applicationContext.getBeansOfType(SqlSessionFactory.class);
        log.info("Found {} SqlSessionFactory bean(s), starting to configure sqlFragments...", sqlSessionFactoryMap.size());
        for (Map.Entry<String, SqlSessionFactory> entry : sqlSessionFactoryMap.entrySet()) {
            String beanName = entry.getKey();
            SqlSessionFactory sqlSessionFactory = entry.getValue();
            boolean success = MapperUtils.initSqlFragment(sqlSessionFactory);
            if (success) {
                log.debug("sqlFragments configured for SqlSessionFactory bean: {}", beanName);
            } else {
                log.error("sqlFragments configuration failed for SqlSessionFactory bean: {}, dynamic sql may not work", beanName);
            }
        }
        ObjectProvider<SqlSessionFactory> beanProvider = applicationContext.getBeanProvider(SqlSessionFactory.class);
        SqlSessionFactory sqlSessionFactory = beanProvider.getIfAvailable();
        if (sqlSessionFactory != null) {
            Configuration configuration = sqlSessionFactory.getConfiguration();
            boolean mapUnderscoreToCamelCase = configuration.isMapUnderscoreToCamelCase();
            BasicProvider basicProvider = new BasicProvider(mapUnderscoreToCamelCase);
            boolean b = BoostUtils.registerProvider(basicProvider);
            log.info("BoostUtils Provider register success {}", b);
        }
    }
}