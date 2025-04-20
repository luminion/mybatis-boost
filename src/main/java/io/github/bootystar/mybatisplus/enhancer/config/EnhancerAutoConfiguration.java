package io.github.bootystar.mybatisplus.enhancer.config;

import io.github.bootystar.mybatisplus.enhancer.util.MapperHelper;
import lombok.extern.slf4j.Slf4j;
import org.apache.ibatis.session.SqlSessionFactory;
import org.springframework.beans.BeansException;
import org.springframework.boot.autoconfigure.AutoConfiguration;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;

/**
 * @author bootystar
 */
@Slf4j
@AutoConfiguration
@ConditionalOnClass({SqlSessionFactory.class})
public class EnhancerAutoConfiguration implements ApplicationContextAware {

    @Override
    public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
        SqlSessionFactory sqlSessionFactory = applicationContext.getBean(SqlSessionFactory.class);
        boolean b = MapperHelper.initSqlFragment(sqlSessionFactory);
        if (b){
            log.debug("DynamicMapper sqlFragments Configured");
        }else{
            log.error("DynamicMapper sqlFragments Configured Failed, dynamic sql may not work");
        }
    }

}
