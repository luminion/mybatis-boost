package io.github.bootystar.mybatisplus.enhancer.config;

import io.github.bootystar.mybatisplus.enhancer.util.MapperUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.ibatis.session.SqlSessionFactory;
import org.springframework.beans.BeansException;
import org.springframework.boot.autoconfigure.AutoConfiguration;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;

import java.util.Map;

/**
 * 自动配置类
 * <p>
 * 该配置类会在存在SqlSessionFactory的条件下自动配置，主要负责为所有SqlSessionFactory实例
 * 初始化EnhancedMapper的SQL片段，确保动态SQL功能能够正常工作
 *
 * @author bootystar
 */
@Slf4j
@AutoConfiguration
@ConditionalOnClass({SqlSessionFactory.class})
public class EnhancerAutoConfiguration implements ApplicationContextAware {

    /**
     * 设置应用上下文，并为所有SqlSessionFactory实例配置EnhancedMapper
     *
     * @param applicationContext 应用上下文
     * @throws BeansException 当获取Bean出现异常时抛出
     */
    @Override
    public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
        // 获取所有 SqlSessionFactory 实例
        Map<String, SqlSessionFactory> sqlSessionFactoryMap = applicationContext.getBeansOfType(SqlSessionFactory.class);
        
        // 为每个 SqlSessionFactory 实例配置 EnhancedMapper
        for (Map.Entry<String, SqlSessionFactory> entry : sqlSessionFactoryMap.entrySet()) {
            String beanName = entry.getKey();
            SqlSessionFactory sqlSessionFactory = entry.getValue();
            
            boolean success = MapperUtil.initSqlFragment(sqlSessionFactory);
            if (success) {
                log.debug("EnhancedMapper sqlFragments configured for SqlSessionFactory bean: {}", beanName);
            } else {
                log.error("EnhancedMapper sqlFragments configuration failed for SqlSessionFactory bean: {}, dynamic sql may not work", beanName);
            }
        }
    }

}