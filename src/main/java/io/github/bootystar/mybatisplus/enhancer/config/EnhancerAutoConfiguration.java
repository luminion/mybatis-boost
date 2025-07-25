package io.github.bootystar.mybatisplus.enhancer.config;

import io.github.bootystar.mybatisplus.enhancer.util.MapperHelper;
import lombok.extern.slf4j.Slf4j;
import org.apache.ibatis.session.SqlSessionFactory;
import org.springframework.beans.BeansException;
import org.springframework.boot.autoconfigure.AutoConfiguration;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;

import java.util.Map;

/**
 * @author bootystar
 */
@Slf4j
@AutoConfiguration
@ConditionalOnClass({SqlSessionFactory.class})
public class EnhancerAutoConfiguration implements ApplicationContextAware {

    @Override
    public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
        // 获取所有 SqlSessionFactory 实例
        Map<String, SqlSessionFactory> sqlSessionFactoryMap = applicationContext.getBeansOfType(SqlSessionFactory.class);
        
        // 为每个 SqlSessionFactory 实例配置 DynamicMapper
        for (Map.Entry<String, SqlSessionFactory> entry : sqlSessionFactoryMap.entrySet()) {
            String beanName = entry.getKey();
            SqlSessionFactory sqlSessionFactory = entry.getValue();
            
            boolean success = MapperHelper.initSqlFragment(sqlSessionFactory);
            if (success) {
                log.debug("DynamicMapper sqlFragments configured for SqlSessionFactory bean: {}", beanName);
            } else {
                log.error("DynamicMapper sqlFragments configuration failed for SqlSessionFactory bean: {}, dynamic sql may not work", beanName);
            }
        }
    }

}
