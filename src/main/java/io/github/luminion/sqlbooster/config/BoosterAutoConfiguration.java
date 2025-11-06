package io.github.luminion.sqlbooster.config;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import io.github.luminion.sqlbooster.provider.BoostProvider;
import io.github.luminion.sqlbooster.provider.support.BasicProvider;
import io.github.luminion.sqlbooster.provider.support.MybatisPlusProvider;
import io.github.luminion.sqlbooster.provider.support.MybatisProvider;
import io.github.luminion.sqlbooster.util.BoostUtils;
import io.github.luminion.sqlbooster.util.MapperUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.ibatis.session.SqlSessionFactory;
import org.mybatis.spring.boot.autoconfigure.MybatisAutoConfiguration;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.boot.autoconfigure.AutoConfiguration;
import org.springframework.boot.autoconfigure.condition.ConditionalOnBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.Map;

/**
 * Mybatis-Boost 自动配置类.
 *
 * @author luminion
 * @since 1.0.0
 */
@Slf4j
@AutoConfiguration
public class BoosterAutoConfiguration implements InitializingBean {

    private final ApplicationContext applicationContext;

    public BoosterAutoConfiguration(ApplicationContext applicationContext) {
        this.applicationContext = applicationContext;
    }

    @Override
    public void afterPropertiesSet() {
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

        Map<String, BoostProvider> providerMap = applicationContext.getBeansOfType(BoostProvider.class);
        for (BoostProvider provider : providerMap.values()) {
            BoostUtils.registerProvider(provider);
        }
    }

    @Configuration(proxyBeanMethods = false)
    @ConditionalOnClass(BaseMapper.class)
    static class MybatisPlusConfiguration {

        @Bean
//        @ConditionalOnMissingBean
        public BoostProvider mybatisPlusProvider() {
            return new MybatisPlusProvider();
        }
    }

    @Configuration(proxyBeanMethods = false)
    @ConditionalOnClass(SqlSessionFactory.class)
    static class MybatisConfiguration {

        @Bean
//        @ConditionalOnMissingBean
        public BoostProvider mybatisProvider(ObjectProvider<SqlSessionFactory> sqlSessionFactoryProvider) {
            SqlSessionFactory sqlSessionFactory = sqlSessionFactoryProvider.getIfAvailable();
            if (sqlSessionFactory != null) {
                boolean mapUnderscoreToCamelCase = sqlSessionFactory.getConfiguration().isMapUnderscoreToCamelCase();
                return new BasicProvider(mapUnderscoreToCamelCase);
            }
            return new BasicProvider(true);
        }
    }
}
